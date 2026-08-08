#################################################################################
# TB TRIAGE+ cost analysis
# 02a - Aggregation by country + country-level cost figures
# Run AFTER 01a and 01b
#############################################
#############################################

# Clear workspace only when run standalone; preserve loop drivers (n, intervention,
# run_all) if this script was sourced from run-all.R.
rm(list = setdiff(ls(), c("n", "intervention", "run_all")))
source("00-base-functions.R")

if (!exists("n")) n <- 2              # 1 for 20023, 2 for 100K screenings (override before source())
if (!exists("intervention")) intervention <- 2   # 1 for entire, 2 for TB-specific (override before source())

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
# APPROACH-SPECIFIC COSTS
#---------------------------------------
##### Approach 1 CAD
# Define relevant categories and metrics
relevant_categories <- c("# Screened", "# Diagnosed", "Total Cost", "Fixed Cost", "Variable Cost")
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
# PLOTS
#---------------------------------------
##### OVERALL
# Extraction function
extract_costs <- function(df, approach) {
  df %>%
    filter(Category %in% c("Total Cost", "Fixed Cost", "Variable Cost")) %>%
    select(Category, Metric, USD) %>%
    pivot_wider(names_from = Metric, values_from = USD) %>%
    mutate(Approach = approach)
}

# Process data
app1_costs <- extract_costs(app1_total_costs, "CAD4TB")
app2_costs <- extract_costs(app2_total_costs, "CAD4TB + CRP")
cost_data <- bind_rows(app1_costs, app2_costs)
colnames(cost_data) <- c("Cost_Type", "Mean", "Lower_CI", "Upper_CI", "Approach")
cost_data$Cost_Type <- factor(cost_data$Cost_Type, levels = c("Variable Cost", "Fixed Cost", "Total Cost"))

# Plot
if (intervention==1) {
  subtext = "Screening for NCD, HIV, and TB"
  filename = "./Figures/overall_cost_e.png"
} else {
  subtext = "Screening for TB only"
  filename = "./Figures/overall_cost_tb.png"
}

p = ggplot(cost_data, aes(y = Cost_Type, x = Mean, fill = Approach)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(xmin = Lower_CI, xmax = Upper_CI),
                position = position_dodge(width = 0.7), width = 0.2) +

  labs(title = "Overall costs for TB Triage+ Screening",
       subtitle = subtext,
       x = "Costs in thousands USD",
       y = "",
       fill = "Approach") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

if (intervention==1) {
  p = p + scale_x_continuous(breaks = c(0, 200000, 400000, 600000, 800000, 1000000),
                             labels = c("0", "200K", "400K", "600K", "800K", "1M"))
} else {
  p = p + scale_x_continuous(breaks = c(0, 200000, 400000, 600000, 800000),
                             labels = c("0", "200K", "400K", "600K", "800K"))
}
p

# save
ggsave(filename, plot = p, width = 12, height = 8, dpi = 600)


###### South Africa
# Process data
app1_costs <- extract_costs(zaf_costs_app1, "CAD4TB")
app2_costs <- extract_costs(zaf_costs_app2, "CAD4TB + CRP")
cost_data <- bind_rows(app1_costs, app2_costs)
colnames(cost_data) <- c("Cost_Type", "Mean", "Lower_CI", "Upper_CI", "Approach")
cost_data$Cost_Type <- factor(cost_data$Cost_Type, levels = c("Variable Cost", "Fixed Cost", "Total Cost"))
cost_data = as.data.table(cost_data)
clean_numeric <- function(x) as.numeric(gsub("'", "", x))
cost_data[, `:=`(Mean = clean_numeric(Mean),
                 Lower_CI = clean_numeric(Lower_CI),
                 Upper_CI = clean_numeric(Upper_CI))]
# Observed costs
if (intervention==1) {
  cost_data$Observed <- c(364636, 320126, 44510, 406606, 358362, 48244)
} else {
  cost_data$Observed <- c(242560, 223288, 19272, 285076, 261524, 23552)
}

pza = ggplot(cost_data, aes(y = Cost_Type, x = Mean, fill = Approach)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(xmin = Lower_CI, xmax = Upper_CI),
                position = position_dodge(width = 0.7), width = 0.2) +
  geom_point(aes(x = Observed), position = position_dodge(width = 0.7), color = "black", size = 2) +
  labs(subtitle = paste(subtext, ": SA"),
       x = "",
       y = "",
       fill = "Approach") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

if (intervention==1) {
  pza = pza + scale_x_continuous(limits = c(0, 570000),
                                 breaks = seq(0, 570000, by = 100000),
                                 labels = c("0", "100K", "200K", "300K", "400K", "500K"))
} else {
  pza = pza + scale_x_continuous(limits = c(0, 450000),
                                 breaks = seq(0, 450000, by = 100000),
                                 labels = c("0", "100K", "200K", "300K", "400K"))
}
pza

###### Lesotho
app1_costs <- extract_costs(ls_costs_app1, "CAD4TB")
app2_costs <- extract_costs(ls_costs_app2, "CAD4TB + CRP")
cost_data <- bind_rows(app1_costs, app2_costs)
colnames(cost_data) <- c("Cost_Type", "Mean", "Lower_CI", "Upper_CI", "Approach")
cost_data$Cost_Type <- factor(cost_data$Cost_Type, levels = c("Variable Cost", "Fixed Cost", "Total Cost"))
cost_data = as.data.table(cost_data)
clean_numeric <- function(x) as.numeric(gsub("'", "", x))
cost_data[, `:=`(Mean = clean_numeric(Mean),
                 Lower_CI = clean_numeric(Lower_CI),
                 Upper_CI = clean_numeric(Upper_CI))]
# Observed costs
if (intervention==1) {
  cost_data$Observed <- c(451152, 352130, 99021, 474047, 370216, 103831)
} else {
  cost_data$Observed <- c(306945, 254915, 52031, 329840, 273000, 56840)
}

# Create the plot with observed value markers
pls = ggplot(cost_data, aes(y = Cost_Type, x = Mean, fill = Approach)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(xmin = Lower_CI, xmax = Upper_CI),
                position = position_dodge(width = 0.7), width = 0.2) +
  geom_point(aes(x = Observed), position = position_dodge(width = 0.7), color = "black", size = 2) +
  labs(subtitle = paste(subtext, ": LS"),
       x = "Costs in thousands USD",
       y = "",
       fill = "Approach") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

if (intervention==1) {
  pls = pls + scale_x_continuous(limits = c(0, 570000),
                                 breaks = seq(0, 570000, by = 100000),
                                 labels = c("0", "100K", "200K", "300K", "400K", "500K"))
} else {
  pls = pls + scale_x_continuous(limits = c(0, 450000),
                                 breaks = seq(0, 450000, by = 100000),
                                 labels = c("0", "100K", "200K", "300K", "400K"))
}
pls

# Combine plots
combined_plot <- pza + pls + plot_layout(ncol = 1) +
  plot_annotation(title = "Overall costs for TB Triage+ Screening")
combined_plot
ggsave( "./Figures/overall_costs_e.png", plot = combined_plot, width = 12, height = 8, dpi = 600)

# save
if (intervention==1) {
  filename = "./Figures/country_cost_e.png"
} else {
  filename = "./Figures/country_cost_tb.png"
}
ggsave(filename, plot = combined_plot, width = 12, height = 8, dpi = 600)





# NOTE: Cost-component and cost-driver figures are produced by 03a-plots-components.R
# (with CAD4TBv7 labels and safe-plot guards); the old inline versions were removed here.


#---------------------------------------
# COST IMPLICATIONS
#---------------------------------------
# Approach 2 is overall more costly than approach 1
# Fixed costs for app2 are lower
# Reduction in costs from Xpert tests foregone is not enough to offset costs of screening for CRP

# CRP costs incurred
cost_crp_za = 0.1664 * 6962 * 64.35
cost_crp_ls = 0.4774 * 12716 * 64.35

# Xpert costs incurred (app2)
cost_xpert_app2_za = 0.0818 * 6962 * 193.1
cost_xpert_app2_ls = 0.1645 * 12716 * 193.1

# Xpert costs incurred (app1)
cost_xpert_app1_za = 0.0893 * 6962 * 193.1
cost_xpert_app1_ls = 0.2904 * 12716 * 193.1

# Cost difference between Xperts saved
xpert_app2_savings_ls = cost_xpert_app1_ls - cost_xpert_app2_ls
xpert_app2_savings_za = cost_xpert_app1_za - cost_xpert_app2_za

# Difference between cost-savings from foregone Xperts and CRP costs incurres
cost_savings_za = xpert_app2_savings_za - cost_crp_za
cost_savings_ls = xpert_app2_savings_ls - cost_crp_ls

# Costly to implement approach 2
cost_savings_za
cost_savings_ls
cost_savings_za+cost_savings_ls















#
