#################################################################################
# TB TRIAGE+ cost analysis
# 03b - Figures P2: combined components + sensitivity figure
# Run AFTER 03a
#############################################

# Clear workspace only when run standalone; preserve loop drivers (n, intervention,
# run_all) if this script was sourced from run-all.R.
rm(list = setdiff(ls(), c("n", "intervention", "run_all")))
source("00-base-functions.R")

#---------------------------------------
# COST COMPONENTS 
#---------------------------------------
# Combine plots (load both first)
plote  <- readRDS("./output/cost_breakdown_plot_e.rds")   # NCD+HIV+TB
plottb <- readRDS("./output/cost_breakdown_plot_tb.rds")  # TB only

plote <- plote +
  labs(title = "Screening for TB, HIV, and NCD", subtitle = NULL) +
  theme(legend.position = "none",
        axis.text.y = element_text(color = "black"))

plottb <- plottb +
  labs(title = "Screening for TB only", subtitle = NULL) +
  theme(legend.position = "right",
        axis.text.y = element_blank())

# Combine the plots side by side with a common legend and title
combined_plot <- (plote + plottb) +
  plot_layout(ncol = 2)
combined_plot <- combined_plot + plot_annotation(title = "Cost components for TB Triage+")

combined_plot
ggsave("./Figures/cost_components_combined.png", plot = combined_plot, width = 12, height = 8, dpi = 600)


#---------------------------------------
# FIGURE: SENSITIVITY
#---------------------------------------
# Make sure below are for n=1 only
overall_e <- readRDS("./output/total_costs_overall_e.rds")
sa_e      <- readRDS("./output/total_costs_sa_e.rds")
ls_e      <- readRDS("./output/total_costs_ls_e.rds")

overall_tb <- readRDS("./output/total_costs_overall_tb.rds")
sa_tb      <- readRDS("./output/total_costs_sa_tb.rds")
ls_tb      <- readRDS("./output/total_costs_ls_tb.rds")

# Prep
prep <- function(df, intervention_label){
  df %>%
    mutate(
      Intervention = intervention_label,
      Mean     = as.numeric(Mean),
      Lower_CI = as.numeric(Lower_CI),
      Upper_CI = as.numeric(Upper_CI)
    )
}

overall <- bind_rows(prep(overall_e, "TB+HIV+NCD"),
                     prep(overall_tb, "TB only"))
sa      <- bind_rows(prep(sa_e,      "TB+HIV+NCD"),
                     prep(sa_tb,     "TB only"))
ls      <- bind_rows(prep(ls_e,      "TB+HIV+NCD"),
                     prep(ls_tb,     "TB only"))

# Define empirical observed costs:
dots_raw <- tribble(
  ~Intervention,  ~Approach,         ~Overall, ~SA,     ~LS,
  "TB only",      "CAD4TBv7-CRP",     444695,   185084,  259611,
  "TB only",      "CAD4TBv7",         388312,   147945,  240367,
  "TB+HIV+NCD",   "CAD4TBv7-CRP",     633607,   284166,  349441,
  "TB+HIV+NCD",   "CAD4TBv7",         577749,   247552,  330197
)

# Split into per-panel dot data.frames matching your panel data
dots_overall <- dots_raw %>% transmute(Intervention, Approach, dot_mean = Overall)
dots_sa      <- dots_raw %>% transmute(Intervention, Approach, dot_mean = SA)
dots_ls      <- dots_raw %>% transmute(Intervention, Approach, dot_mean = LS)

# Common y-axis (cost) across all panels for comparability
xmax <- max(c(overall$Upper_CI, sa$Upper_CI, ls$Upper_CI,
              dots_overall$dot_mean, dots_sa$dot_mean, dots_ls$dot_mean), na.rm = TRUE)
nice_up <- ceiling(xmax / 50000) * 50000
axis_breaks <- pretty(c(0, nice_up), n = 6)

# Panel function
make_panel <- function(df, dots_df, title_txt, 
                       show_ylab = FALSE,
                       ymax = NULL,
                       remove_ylab = FALSE,
                       remove_catlabels = FALSE) {
  ggplot(df, aes(x = Approach, y = Mean, fill = Intervention)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI),
                  position = position_dodge(width = 0.7), width = 0.2) +
    geom_point(
      data = dots_df,
      aes(x = Approach, y = dot_mean, group = Intervention),
      inherit.aes = FALSE,
      position = position_dodge(width = 0.7),
      size = 1.6, shape = 21, stroke = 0.6, colour = "black", fill = "black"
    ) +
    coord_flip() +
    labs(
      title = title_txt,
      x = NULL,
      y = if (show_ylab && !remove_ylab) "Costs (in thousands USD)" else NULL,
      fill = "Intervention"
    ) +
    scale_y_continuous(
      limits = c(0, ifelse(is.null(ymax), nice_up, ymax)),
      breaks = axis_breaks,
      labels = label_number(scale_cut = cut_si(""))   
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      axis.text.y = if (remove_catlabels) element_blank() else element_text(color = "black")
    )
}

# Now call:
p_overall <- make_panel(overall, dots_overall, "Overall", show_ylab = FALSE)
p_sa      <- make_panel(sa, dots_sa, "South Africa", show_ylab = T, ymax = 500000)
p_ls      <- make_panel(ls, dots_ls, "Lesotho", show_ylab = T, ymax = 500000,
                        remove_ylab = F, remove_catlabels = TRUE)

# Stack vertically with shared legend
combined <- (p_overall / (p_sa | p_ls)) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom") 

combined = combined + 
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 15))

combined

ggsave("./Figures/sensitivity_total_costs.png",
       plot = combined, width = 14, height = 10, dpi = 600)



#
