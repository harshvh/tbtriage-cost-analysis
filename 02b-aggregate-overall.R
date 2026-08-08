#################################################################################
# TB TRIAGE+ cost analysis
# 02b - Aggregation overall (capital/recurrent split, per-person costs)
# Run AFTER 01a and 01b
#############################################
#############################################

# Clear workspace only when run standalone; preserve loop drivers (n, intervention,
# run_all) if this script was sourced from run-all.R.
rm(list = setdiff(ls(), c("n", "intervention", "run_all")))
source("00-base-functions.R")

if (!exists("n")) n <- 1              # 1 for 20023, 2 for 100K screenings (override before source())
if (!exists("intervention")) intervention <- 1   # 1 for entire, 2 for TB-specific (override before source())

# Load data
if (n==1) {
  ls_costs_e_app1 = readRDS("./output/ls_costs_entire_app1.RDS")
  ls_costs_e_app2 = readRDS("./output/ls_costs_entire_app2.RDS")
  zaf_costs_e_app1 = readRDS("./output/zaf_costs_entire_app1.RDS")
  zaf_costs_e_app2 = readRDS("./output/zaf_costs_entire_app2.RDS")

  ls_costs_tb_app1 = readRDS("./output/ls_costs_tb_app1.RDS")
  ls_costs_tb_app2 = readRDS("./output/ls_costs_tb_app2.RDS")
  ls_costs_tb_app3 = readRDS("./output/ls_costs_tb_app3.RDS")
  zaf_costs_tb_app1 = readRDS("./output/zaf_costs_tb_app1.RDS")
  zaf_costs_tb_app2 = readRDS("./output/zaf_costs_tb_app2.RDS")
  zaf_costs_tb_app3 = readRDS("./output/zaf_costs_tb_app3.RDS")
} else if (n==2) {
  ls_costs_e_app1 = readRDS("./output/ls_costs_entire_app1_100k.RDS")
  ls_costs_e_app2 = readRDS("./output/ls_costs_entire_app2_100k.RDS")
  zaf_costs_e_app1 = readRDS("./output/zaf_costs_entire_app1_100k.RDS")
  zaf_costs_e_app2 = readRDS("./output/zaf_costs_entire_app2_100k.RDS")

  ls_costs_tb_app1 = readRDS("./output/ls_costs_tb_app1_100k.RDS")
  ls_costs_tb_app2 = readRDS("./output/ls_costs_tb_app2_100k.RDS")
  ls_costs_tb_app3 = readRDS("./output/ls_costs_tb_app3_100k.RDS")
  zaf_costs_tb_app1 = readRDS("./output/zaf_costs_tb_app1_100k.RDS")
  zaf_costs_tb_app2 = readRDS("./output/zaf_costs_tb_app2_100k.RDS")
  zaf_costs_tb_app3 = readRDS("./output/zaf_costs_tb_app3_100k.RDS")
}

# Modify data
dfs <- list(
  ls_costs_e_app1 = ls_costs_e_app1,
  ls_costs_e_app2 = ls_costs_e_app2,
  ls_costs_tb_app1 = ls_costs_tb_app1,
  ls_costs_tb_app2 = ls_costs_tb_app2,
  ls_costs_tb_app3 = ls_costs_tb_app3,

  zaf_costs_e_app1 = zaf_costs_e_app1,
  zaf_costs_e_app2 = zaf_costs_e_app2,
  zaf_costs_tb_app1 = zaf_costs_tb_app1,
  zaf_costs_tb_app2 = zaf_costs_tb_app2,
  zaf_costs_tb_app3 = zaf_costs_tb_app3
)

# Apply function to each df
allres <- lapply(dfs, merge_currency_data)

# Access individual merged datasets
ls_costs_e_app1 <- allres$ls_costs_e_app1
ls_costs_e_app2 <- allres$ls_costs_e_app2
ls_costs_tb_app1 <- allres$ls_costs_tb_app1
ls_costs_tb_app2 <- allres$ls_costs_tb_app2
ls_costs_tb_app3 <- allres$ls_costs_tb_app3

zaf_costs_e_app1 <- allres$zaf_costs_e_app1
zaf_costs_e_app2 <- allres$zaf_costs_e_app2
zaf_costs_tb_app1 <- allres$zaf_costs_tb_app1
zaf_costs_tb_app2 <- allres$zaf_costs_tb_app2
zaf_costs_tb_app3 <- allres$zaf_costs_tb_app3

if (intervention==1) {
  ls_costs_app1 = ls_costs_e_app1
  ls_costs_app2 = ls_costs_e_app2
  zaf_costs_app1 = zaf_costs_e_app1
  zaf_costs_app2 = zaf_costs_e_app2
} else if (intervention==2) {
  ls_costs_app1 = ls_costs_tb_app1
  ls_costs_app2 = ls_costs_tb_app2
  ls_costs_app3 = ls_costs_tb_app3
  zaf_costs_app1 = zaf_costs_tb_app1
  zaf_costs_app2 = zaf_costs_tb_app2
  zaf_costs_app3 = zaf_costs_tb_app3
}

# Look at costs
ls_costs_app1
ls_costs_app2
zaf_costs_app1
zaf_costs_app2

if (intervention==2) {
  ls_costs_app3
  zaf_costs_app3
}

#---------------------------------------
# COUNTRY COSTS %
#---------------------------------------
dfs <- list(
  ls_costs_app1 = ls_costs_app1,
  ls_costs_app2 = ls_costs_app2,
  zaf_costs_app1 = zaf_costs_app1,
  zaf_costs_app2 = zaf_costs_app2
)

# Per-dataset summary
summary_costs <- bind_rows(lapply(names(dfs), function(nm) extract_cap_total(dfs[[nm]], nm))) %>%
  mutate(
    Approach = ifelse(grepl("app1", Dataset, ignore.case = TRUE), "app1", "app2")
  )

# Combined (LS + ZAF) by approach
combined_by_app <- summary_costs %>%
  group_by(Approach) %>%
  summarise(
    Total_USD     = sum(Total_USD, na.rm = TRUE),
    Capital_USD   = sum(Capital_USD, na.rm = TRUE),
    Recurrent_USD = sum(Recurrent_USD, na.rm = TRUE),
    Capital_Pct   = 100 * Capital_USD / Total_USD,
    Recurrent_Pct = 100 * Recurrent_USD / Total_USD,
    .groups = "drop"
  ) %>%
  mutate(Dataset = paste0("overall", Approach)) %>%
  relocate(Dataset, Approach)

# Final table: individual countries + combined rows
summary_costs_all <- summary_costs %>%
  relocate(Dataset, Approach) %>%
  bind_rows(combined_by_app) %>%
  mutate(
    across(c(Total_USD, Capital_USD, Recurrent_USD), ~round(.x, 0)),
    across(c(Capital_Pct, Recurrent_Pct), ~round(.x, 1))
  ) %>%
  arrange(Approach, Dataset)

summary_costs_all


#---------------------------------------
# COMBINE COUNTRY COSTS
#---------------------------------------
# List of your six data frames
if (intervention==1) {
  df_list <- list(
    ls_costs_app1 = ls_costs_app1,
    ls_costs_app2 = ls_costs_app2,
    zaf_costs_app1 = zaf_costs_app1,
    zaf_costs_app2 = zaf_costs_app2
    )
} else {
  df_list <- list(
    ls_costs_app1 = ls_costs_app1,
    ls_costs_app2 = ls_costs_app2,
    ls_costs_app3 = ls_costs_app3,
    zaf_costs_app1 = zaf_costs_app1,
    zaf_costs_app2 = zaf_costs_app2,
    zaf_costs_app3 = zaf_costs_app3
  )
}

# Define target categories and metrics
target_categories <- c("# Screened", "# Diagnosed", "Total Cost", "Capital Cost", "Recurrent Cost")
target_metrics <- c("Observed", "Mean", "Lower 95% CI", "Upper 95% CI")

# Extract relevant rows and combine
combined_df <- rbindlist(
  lapply(names(df_list), function(name) {
    df <- df_list[[name]]
    df_filtered <- df[Category %in% target_categories & Metric %in% target_metrics, .(Category, Metric, USD)]

    country <- if (startsWith(name, "ls")) "Lesotho" else "South Africa"
    approach <- fifelse(grepl("app1", name), "CAD",
                        fifelse(grepl("app2", name), "CAD-CRP", "Xpert"))

    df_filtered[, `:=`(Country = country, Approach = approach)]
    return(df_filtered)
  }),
  use.names = TRUE
)

# Reorder columns
setcolorder(combined_df, c("Country", "Approach", "Category", "Metric", "USD"))


#---------------------------------------
# AGGREGATE COSTS
#---------------------------------------
combined_df[, USD_clean := as.numeric(gsub("'", "", USD))]

# Aggregate across countries
aggregated_df <- combined_df[
  , .(USD_total = sum(USD_clean, na.rm = TRUE)),
  by = .(Approach, Category, Metric)
]
aggregated_df[, USD_total_fmt := formatC(USD_total, format = "d", big.mark = "'")]

# View result
aggregated_df


#---------------------------------------
# CALCULATE PP COSTS
#---------------------------------------
setDT(aggregated_df)

wide_df <- dcast(
  aggregated_df,
  Approach ~ Category + Metric,
  value.var = "USD_total"
)
wide_df <- as.data.table(wide_df)

# Rename
setnames(wide_df, c(
  "# Screened_Observed", "# Diagnosed_Observed",
  "Total Cost_Mean", "Total Cost_Lower 95% CI", "Total Cost_Upper 95% CI",
  "Capital Cost_Mean", "Capital Cost_Lower 95% CI", "Capital Cost_Upper 95% CI",
  "Recurrent Cost_Mean", "Recurrent Cost_Lower 95% CI", "Recurrent Cost_Upper 95% CI"
), c(
  "screened", "diagnosed",
  "total_mean", "total_lower", "total_upper",
  "capital_mean", "capital_lower", "capital_upper",
  "recurrent_mean", "recurrent_lower", "recurrent_upper"
))

# Calculate cost per person screened and diagnosed (mean and 95% CI)
wide_df[, `:=`(
  cost_per_screened_mean = total_mean / screened,
  cost_per_screened_lower = total_lower / screened,
  cost_per_screened_upper = total_upper / screened,

  cost_per_diagnosed_mean = total_mean / diagnosed,
  cost_per_diagnosed_lower = total_lower / diagnosed,
  cost_per_diagnosed_upper = total_upper / diagnosed
)]

final_df <- wide_df[, .(
  Approach,
  total_mean, total_lower, total_upper, capital_mean, capital_lower, capital_upper, recurrent_mean, recurrent_lower, recurrent_upper,
  cost_per_screened_mean, cost_per_screened_lower, cost_per_screened_upper,
  cost_per_diagnosed_mean, cost_per_diagnosed_lower, cost_per_diagnosed_upper
)]

#---------------------------------------
# FORMAT FOR OUTPUT
#---------------------------------------
# Format numbers
format_bracket <- function(mean, lower, upper, digits = 2) {
  sprintf("%s [%s, %s]",
          formatC(mean, format = "f", digits = digits, big.mark = ","),
          formatC(lower, format = "f", digits = digits, big.mark = ","),
          formatC(upper, format = "f", digits = digits, big.mark = ","))
}

# Apply formatting
final_df[, `:=`(
  Total_Cost = format_bracket(total_mean, total_lower, total_upper, 0),
  Capital_Cost = format_bracket(capital_mean, capital_lower, capital_upper, 0),
  Recurrent_Cost = format_bracket(recurrent_mean, recurrent_lower, recurrent_upper, 0),
  Cost_per_Screening = format_bracket(cost_per_screened_mean, cost_per_screened_lower, cost_per_screened_upper, 0),
  Cost_per_Diagnosis = format_bracket(cost_per_diagnosed_mean, cost_per_diagnosed_lower, cost_per_diagnosed_upper, 0)
)]

# Keep only necessary columns
if (intervention==1) {
  costs_formatted <- rbindlist(list(
    data.table(Cost_Category = "Total Cost (USD)",
               CAD = final_df[Approach == "CAD", Total_Cost],
               `CAD-CRP` = final_df[Approach == "CAD-CRP", Total_Cost]),
    
    data.table(Cost_Category = "Capital Cost (USD)",
               CAD = final_df[Approach == "CAD", Capital_Cost],
               `CAD-CRP` = final_df[Approach == "CAD-CRP", Capital_Cost]),
    
    data.table(Cost_Category = "Recurrent Cost (USD)",
               CAD = final_df[Approach == "CAD", Recurrent_Cost],
               `CAD-CRP` = final_df[Approach == "CAD-CRP", Recurrent_Cost]),
    
    data.table(Cost_Category = "Cost per Screening",
               CAD = final_df[Approach == "CAD", Cost_per_Screening],
               `CAD-CRP` = final_df[Approach == "CAD-CRP", Cost_per_Screening]),
    
    data.table(Cost_Category = "Cost per Diagnosis",
               CAD = final_df[Approach == "CAD", Cost_per_Diagnosis],
               `CAD-CRP` = final_df[Approach == "CAD-CRP", Cost_per_Diagnosis])
  ))
} else {
  costs_formatted <- rbindlist(list(
    data.table(Cost_Category = "Total Cost (USD)",
               CAD = final_df[Approach == "CAD", Total_Cost],
               `CAD-CRP` = final_df[Approach == "CAD-CRP", Total_Cost],
               Xpert = final_df[Approach == "Xpert", Total_Cost]),
    
    data.table(Cost_Category = "Capital Cost (USD)",
               CAD = final_df[Approach == "CAD", Capital_Cost],
               `CAD-CRP` = final_df[Approach == "CAD-CRP", Capital_Cost],
               Xpert = final_df[Approach == "Xpert", Capital_Cost]),
    
    data.table(Cost_Category = "Recurrent Cost (USD)",
               CAD = final_df[Approach == "CAD", Recurrent_Cost],
               `CAD-CRP` = final_df[Approach == "CAD-CRP", Recurrent_Cost],
               Xpert = final_df[Approach == "Xpert", Recurrent_Cost]),
    
    data.table(Cost_Category = "Cost per Screening",
               CAD = final_df[Approach == "CAD", Cost_per_Screening],
               `CAD-CRP` = final_df[Approach == "CAD-CRP", Cost_per_Screening],
               Xpert = final_df[Approach == "Xpert", Cost_per_Screening]),
    
    data.table(Cost_Category = "Cost per Diagnosis",
               CAD = final_df[Approach == "CAD", Cost_per_Diagnosis],
               `CAD-CRP` = final_df[Approach == "CAD-CRP", Cost_per_Diagnosis],
               Xpert = final_df[Approach == "Xpert", Cost_per_Diagnosis])
  ))  
}

costs_overall <- dplyr::filter(costs_formatted, !Cost_Category %in% c("Capital Cost (USD)", "Recurrent Cost (USD)"))
costs_overall


#

