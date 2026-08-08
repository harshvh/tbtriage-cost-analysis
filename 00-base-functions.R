#################################################################################
# TB TRIAGE+ cost analysis
# 00 - Base: packages, shared constants, and helper functions
#############################################

library(tidyverse)   
library(data.table)
library(reshape2)
library(gridExtra)
library(ggrepel)
library(treemap)
library(plotly)
library(crosstalk)
library(scales)
library(corrplot)
library(patchwork)

# Create output directories if they do not exist (safe on a fresh clone)
dir.create("output", showWarnings = FALSE)
dir.create("Figures", showWarnings = FALSE)

# Set the number of simulations
n_simulations <- 10000
set.seed(123)
# Assumed coefficient of variation (CV)
assumed_cv <- 0.2  # 20% of the mean, WHO-CHOICE recommended

# ZAR -> USD market exchange rate (2023 average). LSL is pegged 1:1 to ZAR,
# so the same rate applies to Lesotho costs. Note: this value is currently
# hardcoded as 0.05424 at each conversion point in scripts 01a/01b.
zar_to_usd <- 0.05424

#---------------------------------------
# DEFINE FUNCTIONS
#---------------------------------------
# Function to calculate gamma distribution parameters
gamma_params <- function(mean, sd) {
  shape <- (mean^2) / (sd^2)
  scale <- (sd^2) / mean
  return(list(shape = shape, scale = scale))
}

# Function to calculate beta distribution parameters with validation
beta_params <- function(mean, sd) {
  alpha <- mean * ((mean * (1 - mean) / (sd^2)) - 1)
  beta <- (1 - mean) * ((mean * (1 - mean) / (sd^2)) - 1)

  # Ensure parameters are positive
  if (alpha <= 0 || beta <= 0) {
    stop("Invalid shape parameters: alpha = ", alpha, ", beta = ", beta)
  }

  return(list(shape1 = alpha, shape2 = beta))
}

# Function to add apostrophes for thousands separator
format_with_apostrophes <- function(x) {
  formatC(x, big.mark = "'", format = "f", digits = 0)
}

# Convert values to numeric (removing apostrophes)
convert_to_numeric <- function(df) {
  df %>%
    mutate(Value = as.numeric(str_replace_all(Value, "'", "")))  # Remove apostrophes and convert to numeric
}

# Function to format numbers with thousands separator
format_thousands <- function(x) {
  return(format(x, big.mark = "'", scientific = FALSE))
}

# Merge currecny data for each cost df
merge_currency_data <- function(data_list) {
  df_zar <- data_list$zaf
  df_usd <- data_list$usd
  colnames(df_zar)[3] <- "ZAR"
  colnames(df_usd)[3] <- "USD"
  df_usd$Category <- gsub(" \\(USD\\)", "", df_usd$Category)
  df_merged <- df_zar %>%
    left_join(df_usd, by = c("Category", "Metric"))
  return(df_merged)
}

# Function to clean numbers (remove apostrophes and convert to numeric)
clean_number <- function(x) {
  as.numeric(gsub("'", "", x))
}

# Functions for table with country percentages (2-cost-overall)
num <- function(x) as.numeric(gsub("[^0-9.]", "", x))

extract_cap_total <- function(df, label) {
  df %>%
    mutate(Category_clean = gsub(" \\(USD\\)$", "", Category)) %>%
    filter(Metric == "Mean", Category_clean %in% c("Total Cost", "Capital Cost")) %>%
    transmute(Category_clean, USD = num(USD)) %>%
    pivot_wider(names_from = Category_clean, values_from = USD) %>%
    mutate(
      `Recurrent Cost` = `Total Cost` - `Capital Cost`,
      Capital_Pct   = 100 * `Capital Cost` / `Total Cost`,
      Recurrent_Pct = 100 * `Recurrent Cost` / `Total Cost`,
      Dataset = label
    ) %>%
    select(
      Dataset,
      Total_USD     = `Total Cost`,
      Capital_USD   = `Capital Cost`,
      Recurrent_USD = `Recurrent Cost`,
      Capital_Pct,
      Recurrent_Pct
    )
}


#
