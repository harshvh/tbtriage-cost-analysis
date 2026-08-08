#################################################################################
# TB TRIAGE+ cost analysis
# 01a - Primary costing: South Africa (economic costs, PSA)
#############################################
#############################################

# Clear workspace only when run standalone; preserve loop drivers (n, intervention,
# run_all) if this script was sourced from run-all.R.
rm(list = setdiff(ls(), c("n", "intervention", "run_all")))
source("00-base-functions.R")

# n = 1 (20,000 participants) or 2 (100,000 participants)
# intervention 1 (entire) 2 (TB-only)
# approach = 1 (CAD) 2 (CAD-CRP) 3 (Xpert-all)

# Loop over values
for (n in c(1, 2)) {
  for (intervention in c(1, 2)) {
    for (approach in c(1, 2, 3)) {

      # Skip invalid combo
      if (intervention == 1 && approach == 3) next

      # Print progress (optional)
      cat("Running: n =", n, "| intervention =", intervention, "| approach =", approach, "\n")

      #---------------------------------------
      # ASSIGN COSTS (total over two years, ZAR)
      #---------------------------------------
      # N = 20,023 participants
      if (n==1) {
        # ENTIRE intervention
        if (intervention==1) {
          ####################### APP2: CAD-CRP #####################
          # Fixed costs
          fixed_costs_app2 <- data.frame(
            category = c("Medical equipment", "Laboratory",
                         "Vehicles", "Personnel", "Fuel",
                         "Medical supplies"),
            mean = c(730066, 6057,
                     602628, 3157776, 181896,
                     16124)
          )
          # CAD-CRP Variable costs
          variable_costs_app2 <- data.frame(
            category = c("CAD", "CRP", "HIV (1)",
                         "HIV (2)", "MGIT", "Xpert",
                         "NCD", "TB LAM", "PIMA",
                         "CrAg", "Gowns"),
            mean = c(14.75, 62.1, 53.82,
                     53.8, 579.55, 186.14,
                     72.99, 76.63, 187.9,
                     65.36, 32.5)
          )
          # CAD-CRP Screening probabilities
          pr_scr_app2 <- data.frame(
            category = c("CAD", "CRP", "HIV (1)",
                         "HIV (2)", "MGIT", "Xpert",
                         "NCD", "TB LAM", "PIMA",
                         "CrAg", "Gowns"),
            mean = c(1, 0.167, 0.2278,
                     0.0057, 0.0022, 0.082,
                     0.3105, 0.0083, 0.0001,
                     0.0085, 0.5464)
          )
          ####################### APP1: CAD #####################
          # CAD Fixed costs approach 1
          fixed_costs_app1 <- data.frame(
            category = c("Medical equipment", "Laboratory",
                         "Vehicles", "Personnel", "Fuel",
                         "Medical supplies"),
            mean = c(712931, 6057,
                     491673, 2739552, 122376,
                     12883)
          )

          # CAD Variable costs approach 1
          variable_costs_app1 <- variable_costs_app2 %>%
            filter(category != "CRP")

          # CAD Screening prob approach 1
          pr_scr_app1 <- data.frame(
            category = c("CAD", "HIV (1)",
                         "HIV (2)", "MGIT", "Xpert",
                         "NCD", "TB LAM", "PIMA",
                         "CrAg", "Gowns"),
            mean = c(1, 0.2278,
                     0.0057, 0.0022, 0.0896,
                     0.3105, 0.0083, 0.0001,
                     0.0085, 0.5464)
          )
        } else if (intervention==2) {
          # TB-specific intervention
          ####################### APP2: CAD-CRP #####################
          # Fixed costs
          fixed_costs_app2 <- data.frame(
            category = c("Medical equipment",
                         "Vehicles", "Personnel", "Fuel",
                         "Medical supplies"),
            mean = c(630015,
                     387320, 1903104, 181896,
                     10031)
          )
          # CAD-CRP Variable costs
          variable_costs_app2 <- data.frame(
            category = c("CAD", "CRP", "Xpert"),
            mean = c(14.75, 62.1, 186.14)
          )
          # CAD-CRP Screening probabilities
          pr_scr_app2 <- data.frame(
            category = c("CAD", "CRP", "Xpert"),
            mean = c(1, 0.167, 0.082)
          )

          ####################### APP1: CAD #####################
          # CAD Fixed costs approach 1
          fixed_costs_app1 <- data.frame(
            category = c("Medical equipment",
                         "Vehicles", "Personnel", "Fuel",
                         "Medical supplies"),
            mean = c(612880,
                     276366, 1484880, 122376,
                     6790)
          )
          # CAD Variable costs approach 1
          variable_costs_app1 <- variable_costs_app2 %>%
            filter(category != "CRP")

          # CAD Screening prob approach 1
          pr_scr_app1 <- data.frame(
            category = c("CAD", "Xpert"),
            mean = c(1, 0.0896)
          )

          ####################### APP3: Xpert only #####################
          # XPERT Fixed costs approach 3
          fixed_costs_app3 <- data.frame(
            category = c("Medical equipment",
                         "Vehicles", "Personnel", "Fuel",
                         "Medical supplies"),
            mean = c(627323,
                     157703, 1663704, 80400,
                     10031)
          )

          # XPERT Variable costs approach 3
          variable_costs_app3 <- data.frame(
            category = c("Xpert"),
            mean = c(186.14)
          )

          # XPERT Screening prob approach 3
          pr_scr_app3 <- data.frame(
            category = c("Xpert"),
            mean = c(1)
          )
        }
      } else if (n==2) {
        # N = 100,000 participants
        # ENTIRE intervention
        if (intervention==1) {
          ####################### APP2: CAD-CRP #####################
          # CAD-CRP Fixed costs
          fixed_costs_app2 <- data.frame(
            category = c("Medical equipment", "Laboratory",
                         "Vehicles", "Personnel", "Fuel",
                         "Medical supplies"),
            mean = c(3650332, 44321,
                     3013141, 15788882, 909480,
                     157290)
          )
          # CAD-CRP Variable costs
          variable_costs_app2 <- data.frame(
            category = c("CAD", "CRP", "HIV (1)",
                         "HIV (2)", "MGIT", "Xpert",
                         "NCD", "TB LAM", "PIMA",
                         "CrAg", "Gowns"),
            mean = c(14.75, 62.1, 53.82,
                     53.8, 579.55, 186.14,
                     72.99, 76.63, 187.9,
                     65.36, 32.5)
          )
          # CAD-CRP Screening probabilities
          pr_scr_app2 <- data.frame(
            category = c("CAD", "CRP", "HIV (1)",
                         "HIV (2)", "MGIT", "Xpert",
                         "NCD", "TB LAM", "PIMA",
                         "CrAg", "Gowns"),
            mean = c(1, 0.167, 0.2278,
                     0.0057, 0.0022, 0.082,
                     0.3105, 0.0083, 0.0001,
                     0.0085, 0.5464)
          )

          ####################### APP1: CAD #####################
          # CAD Fixed costs approach 1
          fixed_costs_app1 <- data.frame(
            category = c("Medical equipment", "Laboratory",
                         "Vehicles", "Personnel", "Fuel",
                         "Medical supplies"),
            mean = c(3471481, 44321,
                     2902186, 15370658, 849960,
                     142640)
          )
          # CAD Variable costs approach 1
          variable_costs_app1 <- variable_costs_app2 %>%
            filter(category != "CRP")
          # CAD Screening prob approach 1
          pr_scr_app1 <- data.frame(
            category = c("CAD", "HIV (1)",
                         "HIV (2)", "MGIT", "Xpert",
                         "NCD", "TB LAM", "PIMA",
                         "CrAg", "Gowns"),
            mean = c(1, 0.2278,
                     0.0057, 0.0022, 0.0896,
                     0.3105, 0.0083, 0.0001,
                     0.0085, 0.5464)
          )
        } else if (intervention==2) {
          # TB-specific intervention
          ####################### APP2: CAD-CRP #####################
          # CAD-CRP Fixed costs
          fixed_costs_app2 <- data.frame(
            category = c("Medical equipment",
                         "Vehicles", "Personnel", "Fuel",
                         "Medical supplies"),
            mean = c(2654576,
                     1315020, 14534210, 909480,
                     101847)
          )
          # CAD-CRP Variable costs
          variable_costs_app2 <- data.frame(
            category = c("CAD", "CRP", "Xpert"),
            mean = c(14.75, 62.1, 186.14)
          )
          # CAD-CRP Screening probabilities
          pr_scr_app2 <- data.frame(
            category = c("CAD", "CRP", "Xpert"),
            mean = c(1, 0.167, 0.082)
          )

          ####################### APP1: CAD #####################
          # CAD Fixed costs approach 1
          fixed_costs_app1 <- data.frame(
            category = c("Medical equipment",
                         "Vehicles", "Personnel", "Fuel",
                         "Medical supplies"),
            mean = c(2475724,
                     1204065, 14115986, 849960,
                     87196)
          )
          # CAD Variable costs approach 1
          variable_costs_app1 <- variable_costs_app2 %>%
            filter(category != "CRP")
          # CAD Screening prob approach 1
          pr_scr_app1 <- data.frame(
            category = c("CAD", "Xpert"),
            mean = c(1, 0.0896)
          )

          ####################### APP3: Xpert #####################
          # XPERT Fixed costs approach 3
          fixed_costs_app3 <- data.frame(
            category = c("Medical equipment",
                         "Vehicles", "Personnel", "Fuel",
                         "Medical supplies"),
            mean = c(1060880,
                     683671, 10827865, 402000,
                     149647)
          )
          # XPERT Variable costs approach 3
          variable_costs_app3 <- data.frame(
            category = c("Xpert"),
            mean = c(186.14)
          )
          # XPERT Screening prob approach 3
          pr_scr_app3 <- data.frame(
            category = c("Xpert"),
            mean = c(1)
          )
        }
      }

      #---------------------------------------
      # COST ARITHMETICS
      #---------------------------------------
      if (approach == 1) {
        fixed_costs = fixed_costs_app1
        variable_costs = variable_costs_app1
        pr_scr = pr_scr_app1
      } else if (approach == 2) {
        fixed_costs = fixed_costs_app2
        variable_costs = variable_costs_app2
        pr_scr = pr_scr_app2
      } else if (approach == 3) {
        fixed_costs = fixed_costs_app3
        variable_costs = variable_costs_app3
        pr_scr = pr_scr_app3
      }

      fixed_costs <- fixed_costs %>%
        mutate(sd = mean * assumed_cv)

      variable_costs <- variable_costs %>%
        mutate(sd = mean * assumed_cv)

      pr_scr <- pr_scr %>%
        mutate(sd = mean * assumed_cv)
      # For CAD, set to max acceptable variance for beta distribution
      pr_scr$sd[pr_scr$category == "CAD"] <- pr_scr$mean[pr_scr$category == "CAD"] * (1 - pr_scr$mean[pr_scr$category == "CAD"])

      # Convert to data.table
      setDT(fixed_costs)
      setDT(variable_costs)
      setDT(pr_scr)

      #---------------------------------------
      # TB PREVALANCE AND NUMBER SCREENED
      #---------------------------------------
      # Number of CAD tests deployed (from costing excel)
      mean_screened <- switch(n,
                              "1" = 6986,
                              "2" = 34890
      )
      sd_screened <- mean_screened * assumed_cv

      # TB+ cases
      if (n==1) {
        # TB+ when screening 20,023
        diagnosed = switch(approach,
                           "1" = 31,
                           "2" = 29,
                           "3" = 39)
      } else if (n==2) {
        # TB+ when screening 100,000
        diagnosed = switch(approach,
                           "1" = 155,
                           "2" = 145,
                           "3" = 194)
      }

      # Define mean and standard deviation for prevalence and number of people screened
      # Prevalence: Xpert TB diagnosed/ALL screening tests
      # Prevalence = # diagnosed via Xpert (41 app1 in LS, 31 app2) / # screened (all CAD Xrays -  taken from redcap = 12,716)
      if (n==1) {
        # TB+ when screening 20,023
        mean_prevalence <- switch(approach,
                                  "1" = diagnosed/mean_screened,
                                  "2" = diagnosed/mean_screened,
                                  "3" = diagnosed/mean_screened     # 9 more cases, 6 in SA, 3 in LS
        )
      } else if (n==2) {
        # TB+ when screening 100,000
        mean_prevalence <- switch(approach,
                                  "1" = diagnosed/mean_screened,
                                  "2" = diagnosed/mean_screened,
                                  "3" = diagnosed/mean_screened
        )
      }

      sd_prevalence <- mean_prevalence * assumed_cv

      # Generate samples for prevalence and number of people screened
      samples <- data.table(simulation = 1:n_simulations)
      samples[, prevalence := rbeta(n_simulations, shape1 = (mean_prevalence^2 * (1 - mean_prevalence) / (sd_prevalence^2) - mean_prevalence), shape2 = ((mean_prevalence * (1 - mean_prevalence)^2 / (sd_prevalence^2)) - (1 - mean_prevalence)))]
      samples[, screened := floor(rnorm(n_simulations, mean = mean_screened, sd = sd_screened))]

      #---------------------------------------
      # SAMPLE COSTS AND PROBABILITIES
      #---------------------------------------
      # Generate samples for each fixed cost component
      for (i in 1:nrow(fixed_costs)) {
        params <- gamma_params(fixed_costs$mean[i], fixed_costs$sd[i])
        samples[[fixed_costs$category[i]]] <- rgamma(n_simulations, shape = params$shape, scale = params$scale)
      }

      # Generate samples for probability of screening
      for (i in 1:nrow(pr_scr)) {
        category <- pr_scr$category[i]
        if (category == "CAD") {
          # CAD has fixed probability of 1, no uncertainty
          samples[[paste0("pr_", category)]] <- rep(1, n_simulations)
        } else if (approach == 3) {
          # No uncertainty: fixed value for other categories
          samples[[paste0("pr_", category)]] <- rep(pr_scr$mean[i], n_simulations)
        } else if (approach %in% c(1, 2)) {
          # Generate samples with uncertainty
          params <- beta_params(pr_scr$mean[i], pr_scr$sd[i])
          samples[[paste0("pr_", category)]] <- rbeta(n_simulations, shape1 = params$shape1, shape2 = params$shape2)
        }
      }

      # Generate samples for each variable cost component and apply the probability of screening and number of people screened
      for (i in 1:nrow(variable_costs)) {
        var_col <- variable_costs$category[i]
        pr_col <- paste0("pr_", pr_scr$category[pr_scr$category == var_col])
        params <- gamma_params(variable_costs$mean[i], variable_costs$sd[i])
        samples[[var_col]] <- rgamma(n_simulations, shape = params$shape, scale = params$scale) * samples[[pr_col]] * samples$screened
      }

      # Calculate total screening cost for each simulation
      if (intervention==1) {
        samples <- samples %>%
          mutate(total_cost = rowSums(select(., `Medical equipment`:`Medical supplies`)) + rowSums(select(., CAD:Gowns)),
                 fixed_costs = rowSums(select(., `Medical equipment`:`Medical supplies`)),
                 variable_costs = rowSums(select(., CAD:Gowns))
          )
        samples <- samples %>%
          mutate(screencosts = rowSums(select(., CAD:Gowns))
          )
      } else if (intervention==2) {
        if (approach == 3) {
          samples <- samples %>%
            mutate(total_cost = rowSums(select(., `Medical equipment`:`Medical supplies`)) + rowSums(select(., Xpert)),
                   fixed_costs = rowSums(select(., `Medical equipment`:`Medical supplies`)),
                   variable_costs = rowSums(select(., Xpert))
            )
          samples <- samples %>%
            mutate(screencosts = rowSums(select(., Xpert))
            )
        } else {
          samples <- samples %>%
            mutate(total_cost = rowSums(select(., `Medical equipment`:`Medical supplies`)) + rowSums(select(., CAD:Xpert)),
                   fixed_costs = rowSums(select(., `Medical equipment`:`Medical supplies`)),
                   variable_costs = rowSums(select(., CAD:Xpert))
            )
          samples <- samples %>%
            mutate(screencosts = rowSums(select(., CAD:Xpert))
            )
        }
      }

      # Total screening costs
      screencost = mean(samples$screencosts)
      mean(samples$variable_costs)

      # Number screened for Xpert
      #round(mean_screened*mean(samples$pr_Xpert))

      capital_cols   <- intersect(c("Medical equipment", "Other equipment", "Vehicles"), names(samples))
      recurrent_cols <- intersect(c("Personnel", "Medical supplies"), names(samples))
      
      # Calculate cost per person screened and diagnosed
      samples <- samples %>%
        mutate(cost_per_person_screened = total_cost / mean_screened,
               cost_per_person_diagnosed = total_cost / diagnosed,
               capital_costs   = rowSums(across(all_of(capital_cols)),   na.rm = TRUE),
               recurrent_costs = rowSums(across(all_of(recurrent_cols)), na.rm = TRUE) + screencosts
        )
      
      #---------------------------------------
      # CORRELATION & COST DRIVERS (Lesotho)
      #---------------------------------------
      # Which components to correlate with total cost
      cost_comp_cols <- c("Medical equipment", "Other equipment", "Personnel",
                          "Medical supplies", "Vehicles", "screencosts")
      
      # Keep only those columns that actually exist in 'samples'
      present_cols <- intersect(cost_comp_cols, names(samples))
      
      # Compute Pearson correlations vs total_cost
      correlations <- sapply(present_cols, function(col) {
        cor(samples[[col]], samples$total_cost, use = "complete.obs")
      })
      
      correlations_df <- data.frame(
        category    = names(correlations),
        correlation = as.numeric(correlations)
      ) %>%
        arrange(desc(correlation))
      
      correlations_df$category[correlations_df$category == "screencosts"] <- "Screening costs"
      
      # Plot title/labels
      approach_lab <- c("CAD4TB", "CAD4TB+CRP", "Xpert-only")[approach]
      intervention_lab <- if (intervention == 1) "Entire (TB+HIV+NCD)" else "TB-only"
      n_lab <- if (n == 1) "n = 20,023" else "n = 100,000"
      
      plot_title <- sprintf("Cost drivers: %s", approach_lab)
      plot_subtitle <- sprintf("Lesotho · %s · %s", intervention_lab, n_lab)
      
      plot_corr <- ggplot(correlations_df,
                          aes(x = reorder(category, correlation), y = correlation)) +
        geom_segment(aes(xend = category, y = 0, yend = correlation),
                     colour = "gray", linewidth = 1) +
        geom_point(size = 4, colour = "#99CC99") +
        coord_flip() +
        labs(title = plot_title,
             subtitle = plot_subtitle,
             x = "Cost component",
             y = "Correlation with total cost") +
        scale_y_continuous(limits = c(0, 1),
                           breaks = seq(0, 1, by = 0.25),
                           labels = c("0", "0.25", "0.50", "0.75", "1.00")) +
        theme_minimal() +
        theme(axis.text.y  = element_text(size = 10, colour = "black"),
              axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              plot.title   = element_text(size = 14))
      
      # Save plot (RDS) — follow your existing naming and add _100k when n == 2
      suffix_n <- if (n == 2) "_100k" else ""
      filename_corr <- if (intervention == 1) {
        sprintf("./output/cost_corr_sa_e_app%d%s.rds",  approach, suffix_n)
      } else {
        sprintf("./output/cost_corr_sa_tb_app%d%s.rds", approach, suffix_n)
      }
      saveRDS(plot_corr, file = filename_corr)
      
      # (Optional) also save the underlying correlation table for reporting
      filename_tbl <- sub("\\.rds$", "_table.rds", filename_corr)
      saveRDS(correlations_df, file = filename_tbl)

      #---------------------------------------
      # COST CATEGORIES - 100% STACKED BAR CHART 
      #---------------------------------------
      mean_values <- as_tibble(samples) %>%
        summarise(across(c(`Medical equipment`:`Medical supplies`, screencosts), ~mean(.x, na.rm = TRUE)))
      
      cost_comp <- mean_values %>%
        tidyr::pivot_longer(cols = everything(), names_to = "Category", values_to = "Mean") %>%
        mutate(Percentage = Mean / sum(Mean) * 100) %>%
        arrange(desc(Percentage)) %>%
        as.data.frame()
      
      # filename (include _100k for n==2)
      suffix_n <- if (n == 2) "_100k" else ""
      filename <- if (intervention == 1) {
        sprintf("./output/cost_breakdown_sa_sampled_e_app%d%s.rds",  approach, suffix_n)
      } else {
        sprintf("./output/cost_breakdown_sa_sampled_tb_app%d%s.rds", approach, suffix_n)
      }
      
      saveRDS(cost_comp, file = filename)
      
      #---------------------------------------
      # SUMMARY STATS - TOTAL COSTS, SCREENS, DIAGNOSES
      #---------------------------------------
      summary_stats <- data.table(
        Category = c(
          "# Screened", '# Diagnosed',
          "Total Cost", "Total Cost", "Total Cost",
          "Fixed Cost", "Fixed Cost", "Fixed Cost", "Variable Cost", "Variable Cost", "Variable Cost",
          "Screening Cost", "Screening Cost", "Screening Cost",
          "Diagnosis Cost", "Diagnosis Cost", "Diagnosis Cost",
          "Capital Cost", "Capital Cost", "Capital Cost",
          "Recurrent Cost", "Recurrent Cost", "Recurrent Cost"
        ),
        Metric = c(
          "Observed", "Observed",
          "Mean", "Lower 95% CI", "Upper 95% CI",
          "Mean", "Lower 95% CI", "Upper 95% CI", "Mean", "Lower 95% CI", "Upper 95% CI",
          "Mean", "Lower 95% CI", "Upper 95% CI",
          "Mean", "Lower 95% CI", "Upper 95% CI",
          "Mean", "Lower 95% CI", "Upper 95% CI", "Mean", "Lower 95% CI", "Upper 95% CI"
        ),
        Value = formatC(c(
          mean_screened,
          diagnosed,
          mean(samples$total_cost), quantile(samples$total_cost, 0.025), quantile(samples$total_cost, 0.975),
          mean(samples$fixed_costs), quantile(samples$fixed_costs, 0.025), quantile(samples$fixed_costs, 0.975),
          mean(samples$variable_costs), quantile(samples$variable_costs, 0.025), quantile(samples$variable_costs, 0.975),
          mean(samples$cost_per_person_screened), quantile(samples$cost_per_person_screened, 0.025), quantile(samples$cost_per_person_screened, 0.975),
          mean(samples$cost_per_person_diagnosed), quantile(samples$cost_per_person_diagnosed, 0.025), quantile(samples$cost_per_person_diagnosed, 0.975),
          mean(samples$capital_costs), quantile(samples$capital_costs, 0.025), quantile(samples$capital_costs, 0.975),
          mean(samples$recurrent_costs), quantile(samples$recurrent_costs, 0.025), quantile(samples$recurrent_costs, 0.975)
        ), format="f", big.mark="'", digits=0)
      )
      
      print(summary_stats, row.names=FALSE)
      
      # USD
      summary_stats_usd <- data.table(
        Category = c(
          "# Screened", '# Diagnosed',
          "Total Cost (USD)", "Total Cost (USD)", "Total Cost (USD)",
          "Fixed Cost", "Fixed Cost", "Fixed Cost", "Variable Cost", "Variable Cost", "Variable Cost",
          "Screening Cost (USD)", "Screening Cost (USD)", "Screening Cost (USD)",
          "Diagnosis Cost (USD)", "Diagnosis Cost (USD)", "Diagnosis Cost (USD)",
          "Capital Cost", "Capital Cost", "Capital Cost", "Recurrent Cost", "Recurrent Cost", "Recurrent Cost"
        ),
        Metric = c(
          "Observed",
          "Observed",
          "Mean", "Lower 95% CI", "Upper 95% CI",
          "Mean", "Lower 95% CI", "Upper 95% CI", "Mean", "Lower 95% CI", "Upper 95% CI",
          "Mean", "Lower 95% CI", "Upper 95% CI",
          "Mean", "Lower 95% CI", "Upper 95% CI",
          "Mean", "Lower 95% CI", "Upper 95% CI",
          "Mean", "Lower 95% CI", "Upper 95% CI"
        ),
        Value = formatC(c(
          mean_screened,
          diagnosed,
          mean(samples$total_cost) * 0.05424,
          quantile(samples$total_cost, 0.025) * 0.05424,
          quantile(samples$total_cost, 0.975) * 0.05424,
          mean(samples$fixed_costs) * 0.05424,
          quantile(samples$fixed_costs, 0.025) * 0.05424,
          quantile(samples$fixed_costs, 0.975) * 0.05424,
          mean(samples$variable_costs) * 0.05424,
          quantile(samples$variable_costs, 0.025) * 0.05424,
          quantile(samples$variable_costs, 0.975) * 0.05424,
          mean(samples$cost_per_person_screened) * 0.05424,
          quantile(samples$cost_per_person_screened, 0.025) * 0.05424,
          quantile(samples$cost_per_person_screened, 0.975) * 0.05424,
          mean(samples$cost_per_person_diagnosed) * 0.05424,
          quantile(samples$cost_per_person_diagnosed, 0.025) * 0.05424,
          quantile(samples$cost_per_person_diagnosed, 0.975) * 0.05424,
          mean(samples$capital_costs) * 0.05424,
          quantile(samples$capital_costs, 0.025) * 0.05424,
          quantile(samples$capital_costs, 0.975) * 0.05424,
          mean(samples$recurrent_costs) * 0.05424,
          quantile(samples$recurrent_costs, 0.025) * 0.05424,
          quantile(samples$recurrent_costs, 0.975) * 0.05424
        ), format="f", big.mark="'", digits=0)
      )
      
      # Print results
      print(summary_stats_usd, row.names=FALSE)

      # Save
      zaf_costs <- list(zaf = summary_stats, usd = summary_stats_usd)
      if (n==1) {
        # Screening 20,023
        if (intervention==1) {
          if (approach==1) {
            saveRDS(zaf_costs, "./output/zaf_costs_entire_app1.RDS")
          } else if (approach==2) {
            saveRDS(zaf_costs, "./output/zaf_costs_entire_app2.RDS")
          }
        } else if (intervention==2) {
          if (approach==1) {
            saveRDS(zaf_costs, "./output/zaf_costs_tb_app1.RDS")
          } else if (approach==2) {
            saveRDS(zaf_costs, "./output/zaf_costs_tb_app2.RDS")
          } else if (approach==3) {
            saveRDS(zaf_costs, "./output/zaf_costs_tb_app3.RDS")
          }
        }
      } else if (n==2) {
        # Screening 100,000
        if (intervention==1) {
          if (approach==1) {
            saveRDS(zaf_costs, "./output/zaf_costs_entire_app1_100k.RDS")
          } else if (approach==2) {
            saveRDS(zaf_costs, "./output/zaf_costs_entire_app2_100k.RDS")
          }
        } else if (intervention==2) {
          if (approach==1) {
            saveRDS(zaf_costs, "./output/zaf_costs_tb_app1_100k.RDS")
          } else if (approach==2) {
            saveRDS(zaf_costs, "./output/zaf_costs_tb_app2_100k.RDS")
          } else if (approach==3) {
            saveRDS(zaf_costs, "./output/zaf_costs_tb_app3_100k.RDS")
          }
        }
      }

    }
  }
}

