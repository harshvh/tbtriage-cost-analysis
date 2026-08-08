#################################################################################
# TB TRIAGE+ cost analysis
# 03a - Figures P1: cost components, cost drivers, sensitivity prep
# Run AFTER 01a and 01b; saves total_costs_* RDS consumed by 03b
#############################################

# Clear workspace only when run standalone; preserve loop drivers (n, intervention,
# run_all) if this script was sourced from run-all.R.
rm(list = setdiff(ls(), c("n", "intervention", "run_all")))
source("00-base-functions.R")

if (!exists("n")) n <- 2              # 1 for 20023, 2 for 100K screenings (override before source())
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
# APPROACH-SPECIFIC COSTS
#---------------------------------------
##### Approach 1 CAD
# Define relevant categories and metrics
relevant_categories <- c("# Screened", "# Diagnosed", "Total Cost", "Capital Cost", "Recurrent Cost")
relevant_metrics <- c("Mean", "Lower 95% CI", "Upper 95% CI", "Observed")

# Filter for relevant rows
filtered_ls <- ls_costs_app1 %>%
  filter(Category %in% relevant_categories & Metric %in% relevant_metrics)
filtered_zaf <- zaf_costs_app1 %>%
  filter(Category %in% relevant_categories & Metric %in% relevant_metrics)

# Convert values to numeric
filtered_ls <- filtered_ls %>%
  mutate(ZAR = clean_number(ZAR),
         USD = clean_number(USD))

filtered_zaf <- filtered_zaf %>%
  mutate(ZAR = clean_number(ZAR),
         USD = clean_number(USD))

# Merge data frames
app1_total_costs <- left_join(filtered_ls, filtered_zaf, by = c("Category", "Metric"), suffix = c("_ls", "_zaf"))

# Sum corresponding values
app1_total_costs <- app1_total_costs %>%
  mutate(
    ZAR = ZAR_ls + ZAR_zaf,
    USD = USD_ls + USD_zaf
  ) %>%
  select(Category, Metric, ZAR, USD)

# Change # screened to 20024
app1_total_costs[app1_total_costs$Category == "# Screened" & app1_total_costs$Metric == "Observed", c("ZAR", "USD")] <- 20024

##### Approach 2 CAD-CRP

# Filter for relevant rows
filtered_ls <- ls_costs_app2 %>%
  filter(Category %in% relevant_categories & Metric %in% relevant_metrics)
filtered_zaf <- zaf_costs_app2 %>%
  filter(Category %in% relevant_categories & Metric %in% relevant_metrics)

# Convert values to numeric
filtered_ls <- filtered_ls %>%
  mutate(ZAR = clean_number(ZAR),
         USD = clean_number(USD))

filtered_zaf <- filtered_zaf %>%
  mutate(ZAR = clean_number(ZAR),
         USD = clean_number(USD))

# Merge data frames
app2_total_costs <- left_join(filtered_ls, filtered_zaf, by = c("Category", "Metric"), suffix = c("_ls", "_zaf"))

# Sum corresponding values
app2_total_costs <- app2_total_costs %>%
  mutate(
    ZAR = ZAR_ls + ZAR_zaf,
    USD = USD_ls + USD_zaf
  ) %>%
  select(Category, Metric, ZAR, USD)
app2_total_costs[app2_total_costs$Category == "# Screened" & app2_total_costs$Metric == "Observed", c("ZAR", "USD")] <- 20024

if (intervention==2) {
  ##### Approach 3 XPERT ALL

  # Filter for relevant rows
  filtered_ls <- ls_costs_app3 %>%
    filter(Category %in% relevant_categories & Metric %in% relevant_metrics)
  filtered_zaf <- zaf_costs_app3 %>%
    filter(Category %in% relevant_categories & Metric %in% relevant_metrics)

  # Convert values to numeric
  filtered_ls <- filtered_ls %>%
    mutate(ZAR = clean_number(ZAR),
           USD = clean_number(USD))

  filtered_zaf <- filtered_zaf %>%
    mutate(ZAR = clean_number(ZAR),
           USD = clean_number(USD))

  # Merge data frames
  app3_total_costs <- left_join(filtered_ls, filtered_zaf, by = c("Category", "Metric"), suffix = c("_ls", "_zaf"))

  # Sum corresponding values
  app3_total_costs <- app3_total_costs %>%
    mutate(
      ZAR = ZAR_ls + ZAR_zaf,
      USD = USD_ls + USD_zaf
    ) %>%
    select(Category, Metric, ZAR, USD)
  app3_total_costs[app3_total_costs$Category == "# Screened" & app3_total_costs$Metric == "Observed", c("ZAR", "USD")] <- 20024
}

#---------------------------------------
# SUMMARISE VALUES
#---------------------------------------
app1_total_costs
app2_total_costs

# Convert to data.table
setDT(app1_total_costs)
setDT(app2_total_costs)

# Cost per screening
cost_per_screening <- round(app1_total_costs[Category == "Total Cost" & Metric == "Mean", .(ZAR, USD)] /
                              app1_total_costs[Category == "# Screened" & Metric == "Observed", .(ZAR, USD)])
app1_total_costs <- rbind(app1_total_costs, data.table(Category = "Cost per Screening",
                                                       Metric = "Mean",
                                                       ZAR = cost_per_screening$ZAR,
                                                       USD = cost_per_screening$USD))
cost_per_screening <- round(app1_total_costs[Category == "Total Cost" & Metric == "Lower 95% CI", .(ZAR, USD)] /
                              app1_total_costs[Category == "# Screened" & Metric == "Observed", .(ZAR, USD)])
app1_total_costs <- rbind(app1_total_costs, data.table(Category = "Cost per Screening",
                                                       Metric = "LI",
                                                       ZAR = cost_per_screening$ZAR,
                                                       USD = cost_per_screening$USD))
cost_per_screening <- round(app1_total_costs[Category == "Total Cost" & Metric == "Upper 95% CI", .(ZAR, USD)] /
                              app1_total_costs[Category == "# Screened" & Metric == "Observed", .(ZAR, USD)])
app1_total_costs <- rbind(app1_total_costs, data.table(Category = "Cost per Screening",
                                                       Metric = "UI",
                                                       ZAR = cost_per_screening$ZAR,
                                                       USD = cost_per_screening$USD))
#
cost_per_screening <- round(app2_total_costs[Category == "Total Cost" & Metric == "Mean", .(ZAR, USD)] /
                              app2_total_costs[Category == "# Screened" & Metric == "Observed", .(ZAR, USD)])
app2_total_costs <- rbind(app2_total_costs, data.table(Category = "Cost per Screening",
                                                       Metric = "Mean",
                                                       ZAR = cost_per_screening$ZAR,
                                                       USD = cost_per_screening$USD))
cost_per_screening <- round(app2_total_costs[Category == "Total Cost" & Metric == "Lower 95% CI", .(ZAR, USD)] /
                              app2_total_costs[Category == "# Screened" & Metric == "Observed", .(ZAR, USD)])
app2_total_costs <- rbind(app2_total_costs, data.table(Category = "Cost per Screening",
                                                       Metric = "LI",
                                                       ZAR = cost_per_screening$ZAR,
                                                       USD = cost_per_screening$USD))
cost_per_screening <- round(app2_total_costs[Category == "Total Cost" & Metric == "Upper 95% CI", .(ZAR, USD)] /
                              app2_total_costs[Category == "# Screened" & Metric == "Observed", .(ZAR, USD)])
app2_total_costs <- rbind(app2_total_costs, data.table(Category = "Cost per Screening",
                                                       Metric = "UI",
                                                       ZAR = cost_per_screening$ZAR,
                                                       USD = cost_per_screening$USD))

# Cost per diagnosis
cost_per_diagnosis<- round(app1_total_costs[Category == "Total Cost" & Metric == "Mean", .(ZAR, USD)] /
                             app1_total_costs[Category == "# Diagnosed" & Metric == "Observed", .(ZAR, USD)])
app1_total_costs <- rbind(app1_total_costs, data.table(Category = "Cost per Diagnosis",
                                                       Metric = "Mean",
                                                       ZAR = cost_per_diagnosis$ZAR,
                                                       USD = cost_per_diagnosis$USD))
cost_per_diagnosis <- round(app1_total_costs[Category == "Total Cost" & Metric == "Lower 95% CI", .(ZAR, USD)] /
                              app1_total_costs[Category == "# Diagnosed" & Metric == "Observed", .(ZAR, USD)])
app1_total_costs <- rbind(app1_total_costs, data.table(Category = "Cost per Diagnosis",
                                                       Metric = "LI",
                                                       ZAR = cost_per_diagnosis$ZAR,
                                                       USD = cost_per_diagnosis$USD))
cost_per_diagnosis <- round(app1_total_costs[Category == "Total Cost" & Metric == "Upper 95% CI", .(ZAR, USD)] /
                              app1_total_costs[Category == "# Diagnosed" & Metric == "Observed", .(ZAR, USD)])
app1_total_costs <- rbind(app1_total_costs, data.table(Category = "Cost per Diagnosis",
                                                       Metric = "UI",
                                                       ZAR = cost_per_diagnosis$ZAR,
                                                       USD = cost_per_diagnosis$USD))
#
cost_per_diagnosis<- round(app2_total_costs[Category == "Total Cost" & Metric == "Mean", .(ZAR, USD)] /
                             app2_total_costs[Category == "# Diagnosed" & Metric == "Observed", .(ZAR, USD)])
app2_total_costs <- rbind(app2_total_costs, data.table(Category = "Cost per Diagnosis",
                                                       Metric = "Mean",
                                                       ZAR = cost_per_diagnosis$ZAR,
                                                       USD = cost_per_diagnosis$USD))
cost_per_diagnosis <- round(app2_total_costs[Category == "Total Cost" & Metric == "Lower 95% CI", .(ZAR, USD)] /
                              app2_total_costs[Category == "# Diagnosed" & Metric == "Observed", .(ZAR, USD)])
app2_total_costs <- rbind(app2_total_costs, data.table(Category = "Cost per Diagnosis",
                                                       Metric = "LI",
                                                       ZAR = cost_per_diagnosis$ZAR,
                                                       USD = cost_per_diagnosis$USD))
cost_per_diagnosis <- round(app2_total_costs[Category == "Total Cost" & Metric == "Upper 95% CI", .(ZAR, USD)] /
                              app2_total_costs[Category == "# Diagnosed" & Metric == "Observed", .(ZAR, USD)])
app2_total_costs <- rbind(app2_total_costs, data.table(Category = "Cost per Diagnosis",
                                                       Metric = "UI",
                                                       ZAR = cost_per_diagnosis$ZAR,
                                                       USD = cost_per_diagnosis$USD))

app1_total_costs
app2_total_costs

if (intervention==2) {
  app3_total_costs
  # Convert to data.table
  setDT(app3_total_costs)

  # Cost per screening
  cost_per_screening <- round(app3_total_costs[Category == "Total Cost" & Metric == "Mean", .(ZAR, USD)] /
                                app3_total_costs[Category == "# Screened" & Metric == "Observed", .(ZAR, USD)])
  app3_total_costs <- rbind(app3_total_costs, data.table(Category = "Cost per Screening",
                                                         Metric = "Mean",
                                                         ZAR = cost_per_screening$ZAR,
                                                         USD = cost_per_screening$USD))
  cost_per_screening <- round(app3_total_costs[Category == "Total Cost" & Metric == "Lower 95% CI", .(ZAR, USD)] /
                                app3_total_costs[Category == "# Screened" & Metric == "Observed", .(ZAR, USD)])
  app3_total_costs <- rbind(app3_total_costs, data.table(Category = "Cost per Screening",
                                                         Metric = "LI",
                                                         ZAR = cost_per_screening$ZAR,
                                                         USD = cost_per_screening$USD))
  cost_per_screening <- round(app3_total_costs[Category == "Total Cost" & Metric == "Upper 95% CI", .(ZAR, USD)] /
                                app3_total_costs[Category == "# Screened" & Metric == "Observed", .(ZAR, USD)])
  app3_total_costs <- rbind(app3_total_costs, data.table(Category = "Cost per Screening",
                                                         Metric = "UI",
                                                         ZAR = cost_per_screening$ZAR,
                                                         USD = cost_per_screening$USD))
  # Cost per diagnosis
  cost_per_diagnosis<- round(app3_total_costs[Category == "Total Cost" & Metric == "Mean", .(ZAR, USD)] /
                               app3_total_costs[Category == "# Diagnosed" & Metric == "Observed", .(ZAR, USD)])
  app3_total_costs <- rbind(app3_total_costs, data.table(Category = "Cost per Diagnosis",
                                                         Metric = "Mean",
                                                         ZAR = cost_per_diagnosis$ZAR,
                                                         USD = cost_per_diagnosis$USD))
  cost_per_diagnosis <- round(app3_total_costs[Category == "Total Cost" & Metric == "Lower 95% CI", .(ZAR, USD)] /
                                app3_total_costs[Category == "# Diagnosed" & Metric == "Observed", .(ZAR, USD)])
  app3_total_costs <- rbind(app3_total_costs, data.table(Category = "Cost per Diagnosis",
                                                         Metric = "LI",
                                                         ZAR = cost_per_diagnosis$ZAR,
                                                         USD = cost_per_diagnosis$USD))
  cost_per_diagnosis <- round(app3_total_costs[Category == "Total Cost" & Metric == "Upper 95% CI", .(ZAR, USD)] /
                                app3_total_costs[Category == "# Diagnosed" & Metric == "Observed", .(ZAR, USD)])
  app3_total_costs <- rbind(app3_total_costs, data.table(Category = "Cost per Diagnosis",
                                                         Metric = "UI",
                                                         ZAR = cost_per_diagnosis$ZAR,
                                                         USD = cost_per_diagnosis$USD))
  app3_total_costs
}


#---------------------------------------
# FIGURE: Cost Components
#---------------------------------------
if (intervention==1) {
  img1 <- readRDS("./output/cost_breakdown_sa_sampled_e_app1.rds")
  img2 <- readRDS("./output/cost_breakdown_sa_sampled_e_app2.rds")
  img3 <- readRDS("./output/cost_breakdown_ls_sampled_e_app1.rds")
  img4 <- readRDS("./output/cost_breakdown_ls_sampled_e_app2.rds")
  subtext = "TB+HIV+NCD screening"
} else {
  img1 <- readRDS("./output/cost_breakdown_sa_sampled_tb_app1.rds")
  img2 <- readRDS("./output/cost_breakdown_sa_sampled_tb_app2.rds")
  img3 <- readRDS("./output/cost_breakdown_ls_sampled_tb_app1.rds")
  img4 <- readRDS("./output/cost_breakdown_ls_sampled_tb_app2.rds")
  subtext = "TB screening only"
}

# Merge data
combined_data <- bind_rows(
  img1 %>% mutate(Source = "SA: CAD4TBv7"),
  img2 %>% mutate(Source = "SA: CAD4TBv7-CRP"),
  img3 %>% mutate(Source = "LS: CAD4TBv7"),
  img4 %>% mutate(Source = "LS: CAD4TBv7-CRP")
)
combined_data$Category[combined_data$Category == "screencosts"] <- "Screening costs"
combined_data$Source <- factor(combined_data$Source,
                               levels = c("LS: CAD4TBv7-CRP", "LS: CAD4TBv7","SA: CAD4TBv7-CRP","SA: CAD4TBv7"))

custom_colors <- c(
  "Medical equipment" = "#1B9E77",  # Teal
  "Laboratory"        = "#D95F02",  # Orange
  "Vehicles"          = "#E7298A",  # Pink
  "Personnel"         = "#66A61E",  # Green
  "Fuel"              = "#E6AB02",  # Mustard Yellow
  "Medical supplies"  = "#E41A1C",  # Red
  "Screening costs"   = "#377EB8",  # Blue
  "Other equipment"   = "#A6CEE3"   # Light Blue
)
legend_levels <- names(custom_colors)

# Plot
plot = ggplot(combined_data, aes(x = Percentage, y = Source, fill = Category)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(data = combined_data %>% filter(Percentage > 5),  
            aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.65),  
            size = 3.6, color = "black", fontface = "bold") +  
  scale_x_continuous(labels = percent_format(scale = 1),  breaks = seq(0, 100, by = 20)) + #limits = c(0, 100),
  scale_fill_manual(values = custom_colors,
                    guide = guide_legend(title = NULL)) +
  labs(title = "Cost breakdown for TB Triage screening",
       subtitle = subtext,
       x = "Percentage of Total Cost", y = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

# save
if (intervention==1) {
  plote = plot 
  ggsave("./Figures/cost_components_e.png", plot = plot, width = 12, height = 8, dpi = 600)
  saveRDS(plote,  "./output/cost_breakdown_plot_e.rds")
} else {
  plottb = plot
  ggsave("./Figures/cost_components_tb.png", plot = plot, width = 12, height = 8, dpi = 600)
  saveRDS(plottb, "./output/cost_breakdown_plot_tb.rds")
}


#---------------------------------------
# FIGURE: Cost Drivers
#---------------------------------------
suffix_n <- if (n == 2) "_100k" else ""

read_plot <- function(path) {
  if (!file.exists(path)) return(NULL)
  p <- readRDS(path)
  if (!inherits(p, "gg")) return(NULL)
  p
}

if (intervention == 1) {
  # ENTIRE (NCD+HIV+TB)
  img1 <- read_plot(sprintf("./output/cost_corr_sa_e_app1%s.rds", suffix_n))
  img2 <- read_plot(sprintf("./output/cost_corr_sa_e_app2%s.rds", suffix_n))
  img3 <- read_plot(sprintf("./output/cost_corr_ls_e_app1%s.rds", suffix_n))
  img4 <- read_plot(sprintf("./output/cost_corr_ls_e_app2%s.rds", suffix_n))
  subtext <- "NCD+HIV+TB screening"
} else {
  # TB-only
  img1 <- read_plot(sprintf("./output/cost_corr_sa_tb_app1%s.rds", suffix_n))
  img2 <- read_plot(sprintf("./output/cost_corr_sa_tb_app2%s.rds", suffix_n))
  img3 <- read_plot(sprintf("./output/cost_corr_ls_tb_app1%s.rds", suffix_n))
  img4 <- read_plot(sprintf("./output/cost_corr_ls_tb_app2%s.rds", suffix_n))
  subtext <- "TB screening only"
}

# Apply consistent panel titles and remove extra axis labels
fix_panel <- function(p, title_txt, drop_x = FALSE, drop_y = FALSE) {
  if (is.null(p)) return(NULL)
  p +
    labs(title = title_txt, subtitle = NULL,
         x = if (drop_x) NULL else waiver(),
         y = if (drop_y) NULL else waiver()) +
    theme(plot.title = element_text(size = 12, face = "bold"))
}

img1 <- fix_panel(img1, "CAD4TBv7: South Africa", drop_y = TRUE)
img2 <- fix_panel(img2, "CAD4TBv7-CRP: South Africa", drop_x = TRUE, drop_y = TRUE)
img3 <- fix_panel(img3, "CAD4TBv7: Lesotho")
img4 <- fix_panel(img4, "CAD4TBv7-CRP: Lesotho", drop_x = TRUE)

# Compose 2x2
plots_list <- list(img1, img2, img3, img4)
safe_plot <- function(p, txt) {
  if (!is.null(p)) return(p)
  ggplot() + theme_void() + labs(caption = paste("Missing:", txt))
}

p1 <- safe_plot(img1, "CAD4TBv7: South Africa")
p2 <- safe_plot(img2, "CAD4TBv7-CRP: South Africa")
p3 <- safe_plot(img3, "CAD4TBv7: Lesotho")
p4 <- safe_plot(img4, "CAD4TBv7-CRP: Lesotho")

library(patchwork)
combined_plot <- (p1 | p2) / (p3 | p4) +
  plot_layout(guides = "collect")

combined_plot <- combined_plot +
  plot_annotation(
    title = "Cost drivers for TB Triage+",
    subtitle = subtext,
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
  )

combined_plot

ggsave(
  if (intervention == 1) sprintf("./Figures/cost_drivers_e%s.png", if (n==2) "_100k" else "")
  else                   sprintf("./Figures/cost_drivers_tb%s.png", if (n==2) "_100k" else ""),
  plot = combined_plot, width = 12, height = 8, dpi = 600
)


#---------------------------------------
# FIGURE: SENSITIVITY
#---------------------------------------
clean_numeric <- function(x) as.numeric(gsub("[^0-9.]", "", as.character(x)))
get_total_costs <- function(df, approach) {
  out <- df %>%
    dplyr::filter(Category == "Total Cost") %>%
    dplyr::select(Metric, USD) %>%
    tidyr::pivot_wider(names_from = Metric, values_from = USD) %>%
    dplyr::mutate(
      Approach = approach,
      Mean      = clean_numeric(`Mean`),
      Lower_CI  = clean_numeric(`Lower 95% CI`),
      Upper_CI  = clean_numeric(`Upper 95% CI`)
    ) %>%
    dplyr::select(Approach, Mean, Lower_CI, Upper_CI)
  return(out)
}

# Build combined tables (overall, SA, LS)
overall_total <- dplyr::bind_rows(
  get_total_costs(app1_total_costs, "CAD4TBv7"),
  get_total_costs(app2_total_costs, "CAD4TBv7-CRP")
)

sa_total <- dplyr::bind_rows(
  get_total_costs(zaf_costs_app1, "CAD4TBv7"),
  get_total_costs(zaf_costs_app2, "CAD4TBv7-CRP")
)

ls_total <- dplyr::bind_rows(
  get_total_costs(ls_costs_app1, "CAD4TBv7"),
  get_total_costs(ls_costs_app2, "CAD4TBv7-CRP")
)

# attach metadata
attr(overall_total, "n") <- n
attr(overall_total, "intervention") <- intervention
attr(sa_total,      "n") <- n
attr(sa_total,      "intervention") <- intervention
attr(ls_total,      "n") <- n
attr(ls_total,      "intervention") <- intervention

# Save with the plain names that 03b-plots-sensitivity.R reads.
# 03b combines the intervention==1 ("_e") and intervention==2 ("_tb") outputs,
# so run 03a once per intervention (with intervention <- 1, then <- 2) before 03b.
file_suffix <- if (intervention == 1) "e" else "tb"

saveRDS(overall_total, sprintf("./output/total_costs_overall_%s.rds", file_suffix))
saveRDS(sa_total,      sprintf("./output/total_costs_sa_%s.rds",      file_suffix))
saveRDS(ls_total,      sprintf("./output/total_costs_ls_%s.rds",      file_suffix))

# Also keep a tagged archive copy (n and intervention encoded) for provenance.
tag <- sprintf("_n%d_int%d", n, intervention)
saveRDS(overall_total, sprintf("./output/total_costs_overall_%s%s.rds", file_suffix, tag))
saveRDS(sa_total,      sprintf("./output/total_costs_sa_%s%s.rds",      file_suffix, tag))
saveRDS(ls_total,      sprintf("./output/total_costs_ls_%s%s.rds",      file_suffix, tag))



#
