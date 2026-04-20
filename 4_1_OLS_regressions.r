library(data.table)
library(dplyr)
library(tidyr)
library(fixest)
library(openxlsx)
library(broom)
library(ggplot2)
library(car)

#########################################################################
 
# DEFINE FUNCTIONS
 
# Abbreviate model names to fit Excel's 31-character sheet name limit
shorten_model_name <- function(name) {
  abbrevs <- c(
    "Internalizing|internalising" = "Int", "Externalizing|externalising" = "Ext",
    "Aggressive|aggressive"       = "Agr", "Anxious|anxious"             = "Anx",
    "Attention|attention"         = "Att", "Delinquent|delinquent"       = "Del",
    "Social|social"               = "Soc", "Somatic|somatic"             = "Som",
    "Thought|thought"             = "Tht", "Withdrawn|withdrawn"         = "Wth",
    "_std"                        = "Std", "Model"                       = "M",
    "_"                           = ""
  )
  for (pattern in names(abbrevs)) name <- gsub(pattern, abbrevs[[pattern]], name)
  substr(trimws(name), 1, 31)
}
 
# Run a single feols regression given an outcome variable and formula RHS
run_feols <- function(outcome, rhs, data) {
  feols(as.formula(paste(outcome, "~", rhs)), data = data, cluster = "fam_id")
}
 
# Extract coefficients, SEs, and 99% CIs from a model into a data frame
model_ci <- function(model, model_label, predictor_set) {
  coefs <- coef(model)
  ses   <- sqrt(diag(vcov(model)))
  data.frame(
    term      = names(coefs),
    estimate  = coefs,
    std.error = ses,
    conf.low  = coefs - 2.58 * ses,
    conf.high = coefs + 2.58 * ses,
    model     = model_label,
    predictor_set = predictor_set
  )
}
 
# Export a named list of models to an Excel workbook, one sheet per model
export_models_to_excel <- function(models, file_name) {
  wb <- createWorkbook()
  for (model_name in names(models)) {
    model      <- models[[model_name]]
    sheet_name <- shorten_model_name(model_name)
    results <- bind_rows(
      tidy(model),
      data.frame(
        term     = c("AIC", "BIC", "Adjusted R2", "Sample Size"),
        estimate = c(AIC(model), BIC(model), glance(model)$adj.r.squared, nobs(model))
      )
    )
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, results)
  }
  saveWorkbook(wb, file_name, overwrite = TRUE)
}
 
# Run m1 and m2 regressions for all vars and sources; return nested list of models
run_all_regressions <- function(data_sources, raw_vars, rhs_m1, rhs_m2) {
  models <- list()
  for (source_name in names(data_sources)) {
    data <- data_sources[[source_name]]
    for (var in raw_vars) {
      outcome   <- paste0(var, "_std")
      models[[source_name]][[outcome]][["m1"]] <- run_feols(outcome, rhs_m1, data)
      models[[source_name]][[outcome]][["m2"]] <- run_feols(outcome, rhs_m2, data)
    }
  }
  models
}
 
##
 
# Collect all models for a source into a flat named list for Excel export
collect_models_for_export <- function(models, source_name, raw_vars) {
  out <- list()
  for (var in raw_vars) {
    key <- paste0(var, "_std")
    for (m in c("m1", "m2")) {
      flat_name        <- paste0(key, "_", m, "_", source_name)
      out[[flat_name]] <- models[[source_name]][[key]][[m]]
    }
  }
  out
}
 
# Extract regression results across all sources/vars/types into a single data frame
extract_results <- function(models, raw_vars, predictor_vars, types, data_sources) {
  predictor_set <- ifelse("pgi_EA4" %in% predictor_vars, "ea4", "ea3")
  results_list  <- list()
 
  for (source_name in names(data_sources)) {
    for (var in raw_vars) {
      for (type in types) {
        key <- if (type == "std") paste0(var, "_std") else var
        m1  <- models[[source_name]][[key]][["m1"]]
        m2  <- models[[source_name]][[key]][["m2"]]
        if (is.null(m1) || is.null(m2)) next
 
        combined <- bind_rows(
          model_ci(m1, "m1", predictor_set),
          model_ci(m2, "m2", predictor_set)
        ) %>% mutate(
          outcome     = var,
          source      = source_name,
          type        = type,
          significant = conf.low * conf.high > 0
        )
        results_list[[paste0(var, "_", type, "_", source_name)]] <- combined
      }
    }
  }
  bind_rows(results_list)
}
 
###
 
outcome_labels <- c(
  "attention"     = "Attention",    "externalising" = "Externalizing",
  "aggressive"    = "Aggressive",   "delinquent"    = "Rule-Breaking",
  "internalising" = "Internalizing","anxious"       = "Anxious",
  "somatic"       = "Somatic",      "withdrawn"     = "Withdrawn",
  "social"        = "Social",       "thought"       = "Thought"
)
 
outcome_levels <- c("Internalizing", "Anxious", "Withdrawn", "Somatic",
                    "Social", "Thought", "Attention", "Rule-Breaking",
                    "Aggressive", "Externalizing")
 
predictor_labels <- c(
  "pgi_EA3Cog"    = "EA Cog PGI",
  "pgi_EA3NonCog" = "EA NonCog PGI",
  "pgi_EA4"       = "EA PGI"
)
 
# Classify outcomes into one of four groups based on m1/m2 significance
classify_outcomes <- function(results_df, terms) {
  results_df %>%
    filter(term %in% terms) %>%
    group_by(outcome, model) %>%
    summarise(sig = any(significant), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = model, values_from = sig, values_fill = FALSE) %>%
    mutate(
      group = case_when(
        m1 & m2  ~ "direct",
        m1 & !m2 ~ "total",
        !m1 & m2 ~ "odd",
        TRUE     ~ "insignificant"
      )
    ) %>%
    select(outcome, group)
}
 
# Core plotting function: saves one figure given a subset of plot_df
save_plot <- function(df, ylim, colors, filepath) {
  if (nrow(df) == 0) return(invisible(NULL))
  plot <- ggplot(df, aes(x = outcome_label, y = estimate,
                         color = predictor_label, shape = model, linetype = linetype)) +
    geom_point(size = 3, position = position_dodge(width = 0.6)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0.3, position = position_dodge(width = 0.6)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    scale_color_manual(values = colors) +
    scale_shape_manual(values = c("m1" = 16, "m2" = 17)) +
    scale_linetype_identity() +
    scale_y_continuous(limits = ylim) +
    labs(y = "Estimate for offspring PGI", x = NULL, color = "Predictor", shape = "Model") +
    theme_minimal() +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1, size = 11),
      axis.text    = element_text(size = 11),
      axis.title   = element_text(size = 11),
      legend.text  = element_text(size = 11),
      legend.title = element_text(size = 11),
      legend.position = "bottom"
    )
  ggsave(filepath, plot, width = 12, height = 6)
}
 
generate_pgi_plots <- function(results_df, samples, types, terms, output_dir, colors,
                               title_suffix, file_suffix = "") {
 
  plot_data <- results_df %>% filter(term %in% terms)
  ylim <- range(c(plot_data$conf.low, plot_data$conf.high), na.rm = TRUE)
  all_outcomes <- unique(results_df$outcome)
 
  for (sample in samples) {
    for (type in types) {
 
      # Classify outcomes per sample
      outcome_groups <- classify_outcomes(results_df %>% filter(source == sample), terms)
      insig_outcomes <- setdiff(all_outcomes, outcome_groups$outcome)
      if (length(insig_outcomes) > 0) {
        outcome_groups <- bind_rows(
          outcome_groups,
          data.frame(outcome = insig_outcomes, group = "insignificant")
        )
      }
 
      plot_df <- results_df %>%
        filter(source == sample, type == !!type, term %in% terms, model %in% c("m1", "m2")) %>%
        distinct(outcome, model, term, .keep_all = TRUE) %>%
        mutate(
          linetype        = ifelse(significant, "solid", "dashed"),
          predictor_label = predictor_labels[term],
          outcome_label   = factor(outcome_labels[outcome], levels = outcome_levels)
        ) %>%
        filter(!is.na(predictor_label)) %>%
        group_by(outcome) %>%
        mutate(any_significant = any(significant)) %>%
        ungroup() %>%
        left_join(outcome_groups, by = "outcome")
 
      # Original significant/insignificant figures (unchanged)
      for (sig_status in c(TRUE, FALSE)) {
        df <- plot_df %>% filter(any_significant == sig_status)
        sig_label <- ifelse(sig_status, "significant", "insignificant")
        save_plot(df, ylim, colors,
                  paste0(figures_dir, sample, file_suffix, "_", title_suffix, "_", type, "_", sig_label, ".png"))
      }
 
      # New group figures: direct, total, odd, insignificant
      for (grp in c("direct", "total", "odd", "insignificant")) {
        df <- plot_df %>% filter(group == grp)
        save_plot(df, ylim, colors,
                  paste0(figures_dir, sample, file_suffix, "_", grp, "_", title_suffix, "_", type, ".png"))
      }
    }
  }
}
 
###
 
compare_coefficients_wald <- function(model1, model2, sample1_name, sample2_name, outcome,
                                      model_name = "M1", alpha = 0.05) {
  common_predictors <- intersect(names(coef(model1)), names(coef(model2)))
  results <- list()
 
  for (var_name in common_predictors) {
    coef1    <- coef(model1)[var_name]
    coef2    <- coef(model2)[var_name]
    se1      <- sqrt(diag(vcov(model1)))[var_name]
    se2      <- sqrt(diag(vcov(model2)))[var_name]
    diff     <- coef1 - coef2
    se_diff  <- sqrt(se1^2 + se2^2)
    z_value  <- diff / se_diff
    p_value  <- 2 * (1 - pnorm(abs(z_value)))
    results[[var_name]] <- data.frame(
      outcome = outcome, model = model_name,
      sample1 = sample1_name, sample2 = sample2_name,
      predictor = var_name,
      coef_sample1 = coef1, se_sample1 = se1,
      coef_sample2 = coef2, se_sample2 = se2,
      diff = diff, se_diff = se_diff,
      ci_lower = diff - 1.96 * se_diff, ci_upper = diff + 1.96 * se_diff,
      z_value = z_value, p_value = p_value,
      significant = p_value < alpha,
      sig_stars = case_when(
        p_value < 0.001 ~ "***", p_value < 0.01 ~ "**",
        p_value < 0.05  ~ "*",   TRUE            ~ ""
      )
    )
  }
 
  results_df <- bind_rows(results) %>%
    arrange(p_value) %>%
    mutate(
      p_formatted = case_when(p_value < 0.001 ~ "<0.001", TRUE ~ sprintf("%.3f", p_value)),
      comparison  = sprintf("%s: %s vs %s", model_name, sample1_name, sample2_name)
    )
 
  n_sig <- sum(results_df$significant)
  cat("\n========================================\n")
  cat(sprintf("Outcome: %s | Model: %s | %s vs %s\n", outcome, model_name, sample1_name, sample2_name))
  cat(sprintf("Predictors compared: %d | Significant (alpha=%.2f): %d\n", nrow(results_df), alpha, n_sig))
  cat("========================================\n\n")
 
  if (n_sig > 0) {
    sig_results <- results_df %>%
      filter(significant) %>%
      select(predictor, coef_sample1, coef_sample2, diff, ci_lower, ci_upper, p_formatted, sig_stars)
    colnames(sig_results) <- c("Predictor", sample1_name, sample2_name, "Difference", "CI_Lower", "CI_Upper", "p-value", "Sig")
    print(sig_results, row.names = FALSE)
    cat("\n")
  }
 
  results_df
}
 
#########################################################################
 
# PREPARE DATA
 
final_df <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/OUTPUT/data/final_df.csv",
                  sep = ",", header = TRUE) %>% as.data.frame()
 
covariates <- c("age_at_inclusion", "year_of_birth", "wave")
final_df <- final_df %>%
  group_by(source) %>%
  mutate(across(all_of(covariates), ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE), .names = "{.col}_std")) %>%
  ungroup() %>%
  mutate(female_ctd = female - mean(female, na.rm = TRUE))
 
output_dir  <- "/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/OUTPUT/results/"
figures_dir <- "/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/OUTPUT/figures/"
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)
 
#########################################################################
 
# DEFINE ALL VARIABLES USED
 
raw_vars <- c("aggressive", "anxious", "attention", "delinquent", "social",
              "somatic", "thought", "withdrawn", "externalising", "internalising")
std_vars <- paste0(raw_vars, "_std")
 
control_vars <- c("female_ctd", "age_at_inclusion_std", "year_of_birth_std", "wave_std",
                  paste0("PC", 1:10))
controls <- paste(control_vars, collapse = " + ")
 
data_sources <- list(
  cbcl = final_df %>% filter(source == "cbcl"),
  ysr  = final_df %>% filter(source == "ysr")
)
 
data_sources_gender <- list(
  boys_cbcl  = final_df %>% filter(female == 0, source == "cbcl"),
  boys_ysr   = final_df %>% filter(female == 0, source == "ysr"),
  girls_cbcl = final_df %>% filter(female == 1, source == "cbcl"),
  girls_ysr  = final_df %>% filter(female == 1, source == "ysr")
)
 
samples        <- c("ysr", "cbcl")
samples_gender <- c("boys_cbcl", "girls_cbcl", "boys_ysr", "girls_ysr")
types          <- c("std")
 
#########################################################################
 
# TEST GENDER DIFFERENCE BY INTERACTION
 
interaction_rhs <- function(predictor_vars, control_vars, controls) {
  interact <- function(vars) paste0(vars, ":female_ctd", collapse = " + ")
  controls_no_female <- setdiff(control_vars, "female_ctd")
  paste(
    paste(predictor_vars, collapse = " + "),
    interact(predictor_vars),
    interact(controls_no_female),
    controls,
    sep = " + "
  )
}
 
#########################################################################
 
# RUN FOR BOTH EA4 AND EA3
 
for (predictor_type in c("EA4", "EA3")) {
 
  cat(sprintf("\n=== Running analysis for %s ===\n", predictor_type))
 
  # DEFINE PREDICTORS
  if (predictor_type == "EA4") {
    predictor_vars_m1 <- c("pgi_EA4")
    predictor_vars_m2 <- c("pgi_EA4", "pgi_sum_EA4")
  } else {
    predictor_vars_m1 <- c("pgi_EA3NonCog", "pgi_EA3Cog")
    predictor_vars_m2 <- c("pgi_EA3NonCog", "pgi_EA3Cog", "pgi_sum_EA3NonCog", "pgi_sum_EA3Cog")
  }
 
  predictors_m1 <- paste(predictor_vars_m1, collapse = " + ")
  predictors_m2 <- paste(predictor_vars_m2, collapse = " + ")
  rhs_m1 <- paste(predictors_m1, controls, sep = " + ")
  rhs_m2 <- paste(predictors_m2, controls, sep = " + ")
 
  #########################################################################
 
  # RUN REGRESSIONS AND EXPORT TO EXCEL
 
  models <- run_all_regressions(data_sources, raw_vars, rhs_m1, rhs_m2)
 
  file_paths <- list(
    cbcl = paste0(output_dir, "full_cbcl_", predictor_type, ".xlsx"),
    ysr  = paste0(output_dir, "full_ysr_",  predictor_type, ".xlsx")
  )
 
  for (source_name in names(data_sources)) {
    export_models_to_excel(collect_models_for_export(models, source_name, raw_vars), file_paths[[source_name]])
  }
 
  results_df <- extract_results(models, raw_vars, predictor_vars_m1, types, data_sources)
 
  #########################################################################
 
  # GENERATE FIGURES
 
  generate_pgi_plots(
    results_df = results_df, samples = samples, types = types,
    terms = predictor_vars_m1, output_dir = output_dir,
    colors = if (predictor_type == "EA4") c("EA PGI" = "mediumpurple3")
             else c("EA Cog PGI" = "lightskyblue3", "EA NonCog PGI" = "orchid4"),
    title_suffix = predictor_type
  )
 
  #########################################################################
 
  # WALD TEST
 
  cbcl_vs_ysr_results <- list()
 
  for (outcome in std_vars) {
    for (m in c("m1", "m2")) {
      m_cbcl <- models[["cbcl"]][[outcome]][[m]]
      m_ysr  <- models[["ysr"]][[outcome]][[m]]
      if (is.null(m_cbcl) || is.null(m_ysr)) next
      if (length(intersect(names(coef(m_cbcl)), names(coef(m_ysr)))) == 0) next
      cbcl_vs_ysr_results[[paste0(outcome, "_", m)]] <- compare_coefficients_wald(
        m_cbcl, m_ysr, "CBCL", "YSR", outcome, model_name = toupper(m))
    }
  }
 
  cbcl_vs_ysr_results_df <- bind_rows(cbcl_vs_ysr_results)
 
  cat(sprintf("\nTotal: %d | Significant: %d (%.1f%%)\n",
              nrow(cbcl_vs_ysr_results_df),
              sum(cbcl_vs_ysr_results_df$significant),
              100 * mean(cbcl_vs_ysr_results_df$significant)))
 
  wald_wb <- createWorkbook()
  addWorksheet(wald_wb, "CBCL_vs_YSR")
  writeData(wald_wb, "CBCL_vs_YSR", cbcl_vs_ysr_results_df)
 
  if (sum(cbcl_vs_ysr_results_df$significant) > 0) {
    addWorksheet(wald_wb, "Significant_Only")
    writeData(wald_wb, "Significant_Only", cbcl_vs_ysr_results_df %>% filter(significant))
  }
 
  saveWorkbook(wald_wb, paste0(output_dir, "wald_test_results_", predictor_type, ".xlsx"), overwrite = TRUE)
  cat("Results saved!\n")
 
  #########################################################################
 
  # TEST GENDER DIFFERENCE BY INTERACTION
 
  rhs_m1_interact <- interaction_rhs(predictor_vars_m1, control_vars, controls)
  rhs_m2_interact <- interaction_rhs(predictor_vars_m2, control_vars, controls)
 
  models_interact <- run_all_regressions(data_sources, raw_vars, rhs_m1_interact, rhs_m2_interact)
 
  file_paths_interact <- list(
    cbcl = paste0(output_dir, "interaction_cbcl_", predictor_type, ".xlsx"),
    ysr  = paste0(output_dir, "interaction_ysr_",  predictor_type, ".xlsx")
  )
 
  for (source_name in names(data_sources)) {
    export_models_to_excel(collect_models_for_export(models_interact, source_name, raw_vars), file_paths_interact[[source_name]])
  }
 
  #########################################################################
 
  # RUN REGRESSIONS BY GENDER AND EXPORT TO EXCEL
 
  models_gender <- run_all_regressions(data_sources_gender, raw_vars, rhs_m1, rhs_m2)
 
  file_paths_gender <- list(
    boys_cbcl  = paste0(output_dir, "boys_cbcl_",  predictor_type, ".xlsx"),
    boys_ysr   = paste0(output_dir, "boys_ysr_",   predictor_type, ".xlsx"),
    girls_cbcl = paste0(output_dir, "girls_cbcl_", predictor_type, ".xlsx"),
    girls_ysr  = paste0(output_dir, "girls_ysr_",  predictor_type, ".xlsx")
  )
 
  for (source_name in names(data_sources_gender)) {
    export_models_to_excel(collect_models_for_export(models_gender, source_name, raw_vars), file_paths_gender[[source_name]])
  }
 
  results_df_gender <- extract_results(models_gender, raw_vars, predictor_vars_m1, types, data_sources_gender)
 
  #########################################################################
 
  # GENERATE FIGURES BY GENDER
 
  generate_pgi_plots(
    results_df = results_df_gender, samples = samples_gender, types = types,
    terms = predictor_vars_m1, output_dir = output_dir,
    colors = if (predictor_type == "EA4") c("EA PGI" = "mediumpurple3")
             else c("EA Cog PGI" = "lightskyblue3", "EA NonCog PGI" = "orchid4"),
    title_suffix = predictor_type, file_suffix = "_sex"
  )
 
} # end for loop
 
#########################################################################
 
# END