#########################################################################
# Date: November 2024														
# Description: Code for OLS regressions on 				
#########################################################################

# Load necessary R module and libraries
module load RPlus
R

# Load libraries
library(data.table)
library(dplyr)
library(fixest)
library(openxlsx)
library(broom)
library(ggplot2)
library(car)

#########################################################################

# DEFINE FUNCTIONS

# Function to abbreviate model names for exporting to excel
shorten_model_name <- function(model_name) {
  # Initial replacements to abbreviate terms
  short_model_name <- model_name
  short_model_name <- gsub("Internalizing|internalising", "Int", short_model_name)
  short_model_name <- gsub("Externalizing|externalising", "Ext", short_model_name)
  short_model_name <- gsub("Aggressive|aggressive", "Agr", short_model_name)
  short_model_name <- gsub("Anxious|anxious", "Anx", short_model_name)
  short_model_name <- gsub("Attention|attention", "Att", short_model_name)
  short_model_name <- gsub("Delinquent|delinquent", "Del", short_model_name)
  short_model_name <- gsub("Social|social", "Soc", short_model_name)
  short_model_name <- gsub("Somatic|somatic", "Som", short_model_name)
  short_model_name <- gsub("Thought|thought", "Tht", short_model_name)
  short_model_name <- gsub("Withdrawn|withdrawn", "Wth", short_model_name)
  short_model_name <- gsub("_std", "Std", short_model_name)  # Abbreviate 'std'
  short_model_name <- gsub("Model", "M", short_model_name)   # Abbreviate 'Model'
  # Remove any remaining underscores
  short_model_name <- gsub("_", "", short_model_name)
  # Ensure length constraint
  return(substr(trimws(short_model_name), 1, 31))
}

# Function to process models and export results to Excel
process_feols_model_to_excel <- function(models, model_names, file_name) {
  wb <- createWorkbook()
  for (i in seq_along(models)) {
    model <- models[[i]]
	short_model_name <- shorten_model_name(model_names[[i]])
    tidy_results <- tidy(model)
    summary_stats <- data.frame(
      term = c("AIC", "BIC", "Adjusted R2", "Sample Size"),
      estimate = c(AIC(model), BIC(model), glance(model)$adj.r.squared, nobs(model))
    )
    final_results <- bind_rows(tidy_results, summary_stats)
    addWorksheet(wb, short_model_name)
    writeData(wb, short_model_name, final_results)
  }
  saveWorkbook(wb, file_name, overwrite = TRUE)
}

###

# Extract regression results and combine them into a single data frame for making figures
extract_results <- function(data_sources, raw_vars, predictors, types) {
  results_list <- list()
  for (source_name in names(data_sources)) {
    for (var in raw_vars) {
      for (type in types) {
        # Model names
        m1_name <- paste0(var, "_", type, "_m1_", source_name)
        m2_name <- paste0(var, "_", type, "_m2_", source_name)
        # Check if the objects exist in the environment
        if (exists(m1_name) && exists(m2_name)) {
          m1 <- get(m1_name)
          m2 <- get(m2_name)
          # Extract relevant data directly from feols objects
          m1_results <- data.frame(
            term = names(coef(m1)),                      # Predictor names
            estimate = coef(m1),                        # Coefficient estimates
            std.error = sqrt(diag(vcov(m1))),           # Standard errors
            conf.low = coef(m1) - 2.58 * sqrt(diag(vcov(m1))), # CI lower bound
            conf.high = coef(m1) + 2.58 * sqrt(diag(vcov(m1))), # CI upper bound
            model = "m1",
            predictor_set = ifelse("ea4_pgi" %in% predictors, "ea4", "ea3")
          )
          m2_results <- data.frame(
            term = names(coef(m2)),
            estimate = coef(m2),
            std.error = sqrt(diag(vcov(m2))),
            conf.low = coef(m2) - 2.58 * sqrt(diag(vcov(m2))),
            conf.high = coef(m2) + 2.58 * sqrt(diag(vcov(m2))),
            model = "m2",
            predictor_set = ifelse("ea4_pgi" %in% predictors, "ea4", "ea3")
          )
          # Combine and add metadata
          combined <- bind_rows(m1_results, m2_results) %>%
            mutate(
              outcome = var,
              source = source_name,
              type = type,
              significant = conf.low * conf.high > 0 # Confidence intervals excluding 0
            )
          results_list[[paste0(var, "_", type, "_", source_name)]] <- combined
        }
      }
    }
  }
  # Combine all results
  return(bind_rows(results_list))
}

###

generate_pgi_plots <- function(results_df, samples, types, terms, output_dir, colors, title_suffix, ylim) {
  
  outcome_labels <- c(
    "attention" = "Attention", "externalising" = "Externalizing",
    "aggressive" = "Aggressive", "delinquent" = "Rule-Breaking",
    "internalising" = "Internalizing", "anxious" = "Anxious",
    "somatic" = "Somatic", "withdrawn" = "Withdrawn",
    "social" = "Social", "thought" = "Thought"
  )
  
  predictor_labels <- c(
    "ea3cog_pgi" = "EA Cog PGI",
    "ea3noncog_pgi" = "EA NonCog PGI",
    "ea4_pgi" = "EA PGI"
  )
  
  for (sample in samples) {
    for (type in types) {
      
      plot_df <- results_df %>% 
        filter(source == sample, type == !!type, term %in% terms, model %in% c("m1", "m2")) %>%
        distinct(outcome, model, term, .keep_all = TRUE) %>%
        mutate(
          linetype = ifelse(significant, "solid", "dashed"),
          predictor_label = predictor_labels[term],
          outcome_label = factor(outcome_labels[outcome], 
                                levels = c("Anxious", "Withdrawn", "Somatic", "Internalizing", 
                                          "Social", "Thought", "Attention", "Externalizing", 
                                          "Rule-Breaking", "Aggressive"))
        ) %>%
        filter(!is.na(predictor_label)) %>%
        group_by(outcome) %>%
        mutate(any_significant = any(significant)) %>%
        ungroup()
      
      for (sig_status in c(TRUE, FALSE)) {
        
        df <- plot_df %>% filter(any_significant == sig_status)
        
        if (nrow(df) == 0) next
        
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
          labs(y = "Estimate for offspring PGI", x = NULL, 
               color = "Predictor", shape = "Model") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
            axis.text = element_text(size = 11),
            axis.title = element_text(size = 11),
            legend.text = element_text(size = 11),
            legend.title = element_text(size = 11),
            legend.position = "bottom"
          )
        
        sig_label <- ifelse(sig_status, "significant", "insignificant")
        file_name <- paste0(output_dir, sample, "_", title_suffix, "_", type, "_", sig_label, ".png")
        ggsave(file_name, plot, width = 12, height = 6)
      }
    }
  }
}

###

compare_coefficients_wald <- function(model1, model2, sample1_name, sample2_name, outcome, 
                                      model_name = "M1", 
                                      alpha = 0.05) {
  # Get predictor names for both models
  predictors1 <- names(coef(model1))
  predictors2 <- names(coef(model2))
  
  # Only keep predictors that exist in BOTH models
  common_predictors <- intersect(predictors1, predictors2)
  
  # Initialize results list
  results <- list()
  
  for (var_name in common_predictors) {
    # Extract coefficients and standard errors
    coef1 <- coef(model1)[var_name]
    coef2 <- coef(model2)[var_name]
    
    se1 <- sqrt(diag(vcov(model1)))[var_name]
    se2 <- sqrt(diag(vcov(model2)))[var_name]
    
    # Compute difference, standard error, and z-score
    diff <- coef1 - coef2
    se_diff <- sqrt(se1^2 + se2^2)
    z_value <- diff / se_diff
    p_value <- 2 * (1 - pnorm(abs(z_value)))  # Two-tailed test
    
    # Compute confidence intervals for the difference
    ci_lower <- diff - 1.96 * se_diff
    ci_upper <- diff + 1.96 * se_diff
    
    # Store results in a data frame
    results[[var_name]] <- data.frame(
      outcome = outcome,
      model = model_name,
      sample1 = sample1_name,
      sample2 = sample2_name,
      predictor = var_name, 
      coef_sample1 = coef1,
      se_sample1 = se1,
      coef_sample2 = coef2,
      se_sample2 = se2,
      diff = diff,
      se_diff = se_diff,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      z_value = z_value, 
      p_value = p_value,
      significant = p_value < alpha,
      sig_stars = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        p_value < 0.05 ~ "*",
        TRUE ~ ""
      )
    )
  }
  
  results_df <- bind_rows(results)
  
  # Sort by p-value (most significant first)
  results_df <- results_df %>% 
    arrange(p_value) %>%
    mutate(
      # Format p-values for better readability
      p_formatted = case_when(
        p_value < 0.001 ~ "<0.001",
        TRUE ~ sprintf("%.3f", p_value)
      ),
      # Create a comparison label for clarity
      comparison = sprintf("%s: %s vs %s", 
                          model_name, sample1_name, sample2_name)
    )
  
  # Print summary of significant differences
  n_sig <- sum(results_df$significant)
  cat("\n========================================\n")
  cat("Coefficient Comparison Summary\n")
  cat("========================================\n")
  cat(sprintf("Outcome: %s\n", outcome))
  cat(sprintf("Model: %s\n", model_name))
  cat(sprintf("Comparing samples: %s vs %s\n", sample1_name, sample2_name))
  cat(sprintf("Total predictors compared: %d\n", nrow(results_df)))
  cat(sprintf("Significant differences (α=%.2f): %d\n", alpha, n_sig))
  cat("========================================\n\n")
  
  if (n_sig > 0) {
    cat("Significant differences:\n")
    sig_results <- results_df %>% 
      filter(significant) %>%
      select(predictor, coef_sample1, coef_sample2, diff, ci_lower, ci_upper, p_formatted, sig_stars)
    
    # Rename columns for clarity in output
    colnames(sig_results) <- c("Predictor", 
                               sample1_name, 
                               sample2_name, 
                               "Difference", "CI_Lower", "CI_Upper", "p-value", "Sig")
    print(sig_results, row.names = FALSE)
    cat("\n")
  }
  
  return(results_df)
}

#########################################################################

# PREPARE DATA

# Load dataset
final_df <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/dfs/final_df.csv", sep=",", header=TRUE) %>%
  as.data.frame()
  
covariates <- c("age_at_inclusion", "year_of_birth", "wave")
final_df <- final_df %>%
  group_by(source) %>%
  mutate(across(all_of(covariates), ~ (. - mean(.,na.rm=TRUE)) / sd(.,na.rm=TRUE), .names = "{.col}_std")) %>%
  ungroup()
final_df <- final_df %>%
  mutate(female_ctd = female - mean(female, na.rm = TRUE))

# Filter data by source type
final_df_cbcl <- final_df %>% filter(source == "cbcl")
final_df_ysr <- final_df %>% filter(source == "ysr")

# Output directory
output_dir <- "/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/results/" # Change to your desired directory

# Split data into boys and girls
final_df_boys <- final_df %>% filter(female == 0)
final_df_girls <- final_df %>% filter(female == 1)

# Define data sources for boys and girls
data_sources_gender <- list(
  boys_cbcl = final_df_boys %>% filter(source == "cbcl"),
  boys_ysr = final_df_boys %>% filter(source == "ysr"),
  girls_cbcl = final_df_girls %>% filter(source == "cbcl"),
  girls_ysr = final_df_girls %>% filter(source == "ysr")
)

#########################################################################

# DEFINE ALL VARIABLES USED 

## Should I include also sex-age interaction? For testing gender difference yes

# Define data variables
raw_vars <- c("aggressive", "anxious", "attention", "delinquent", "social", "somatic", "thought", "withdrawn", "externalising", "internalising")
std_vars <- paste0(raw_vars, "_std")

# Define control variables
control_vars <- c("female_ctd","age_at_inclusion_std", "year_of_birth_std", "wave_std", 
                  "pc1", "pc2", "pc3", "pc4", "pc5", "pc6", "pc7", "pc8", "pc9", "pc10")
controls <- paste(control_vars, collapse = " + ")

# Define datasources
data_sources <- list(cbcl = final_df_cbcl, ysr = final_df_ysr)

# Define samples, and types for figures
samples <- c("ysr", "cbcl")
types <- c("raw", "std")

#########################################################################

# DEFINE PREDICTORS

# EA4
predictor_vars_m1 <- c("ea4_pgi")
predictor_vars_m2 <- c("ea4_pgi", "ea4_pgi_sum")
# EA3s
# predictor_vars_m1 <- c("ea3noncog_pgi", "ea3cog_pgi")
# predictor_vars_m2 <- c("ea3noncog_pgi", "ea3cog_pgi", "ea3noncog_pgi_sum", "ea3cog_pgi_sum")

# Formula
predictors_m1 <- paste(predictor_vars_m1, collapse = " + ")
predictors_m2 <- paste(predictor_vars_m2, collapse = " + ")

# Set predictor type based on predictors
predictor_type <- ifelse("ea4_pgi" %in% predictor_vars_m1, "EA4", "EA3")

# RUN REGRESSIONS AND EXPORT TO EXCEL

# Loop through each source for raw standardized and categorical variables
for (source_name in names(data_sources)) {
  data <- data_sources[[source_name]]
  for (var in raw_vars) {
    m1_name <- paste0(var, "_m1_", source_name)
    m2_name <- paste0(var, "_m2_", source_name)
    assign(m1_name, feols(as.formula(paste(var, "~", predictors_m1, "+", controls)), data = data, cluster = "fam_id"))
    assign(m2_name, feols(as.formula(paste(var, "~", predictors_m2, "+", controls)), data = data, cluster = "fam_id"))

    std_var <- paste0(var, "_std")
    std_m1_name <- paste0(std_var, "_m1_", source_name)
    std_m2_name <- paste0(std_var, "_m2_", source_name)
    assign(std_m1_name, feols(as.formula(paste(std_var, "~", predictors_m1, "+", controls)), data = data, cluster = "fam_id"))
    assign(std_m2_name, feols(as.formula(paste(std_var, "~", predictors_m2, "+", controls)), data = data, cluster = "fam_id"))
	  }
}

# Set file paths with predictor_type
file_paths <- list(
  cbcl = paste0("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/results/full_cbcl_", predictor_type, ".xlsx"),
  ysr = paste0("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/results/full_ysr_", predictor_type, ".xlsx")
)

for (source_name in names(data_sources)) {
  models <- list()
  model_names <- c()
  for (var in raw_vars) {
    m1_name <- paste0(var, "_m1_", source_name)
    m2_name <- paste0(var, "_m2_", source_name)
    std_m1_name <- paste0(var, "_std_m1_", source_name)
    std_m2_name <- paste0(var, "_std_m2_", source_name)
    models[[m1_name]] <- get(m1_name)
    models[[m2_name]] <- get(m2_name)
    models[[std_m1_name]] <- get(std_m1_name)
    models[[std_m2_name]] <- get(std_m2_name)
    model_names <- c(model_names, m1_name, m2_name, std_m1_name, std_m2_name)
  }
  process_feols_model_to_excel(models, model_names, file_paths[[source_name]])
}

# Extract results for figures
results_df <- extract_results(data_sources, raw_vars, predictor_vars_m1, types)

#########################################################################

# GENERATE FIGURES

# EA4
generate_pgi_plots(
  results_df = results_df,
  samples = samples,
  types = types,
  terms = c("ea4_pgi"),
  output_dir = output_dir,
  colors = c("EA PGI" = "mediumpurple3"),
  title_suffix = "EA4",
  ylim = c(-0.2,0.15)
)

# EA3
generate_pgi_plots(
  results_df = results_df,
  samples = samples,
  types = types,
  terms = c("ea3cog_pgi", "ea3noncog_pgi"),
  output_dir = output_dir,
  colors = c(
    "EA Cog PGI" = "lightskyblue3",
    "EA NonCog PGI" = "orchid4"
  ),
  title_suffix = "EA3",
  ylim = c(-0.2,0.15)
)

#########################################################################

# WALD TEST
cbcl_vs_ysr_results <- list()

for (outcome in std_vars) {
  cbcl_model_m1 <- paste0(outcome, "_m1_cbcl")
  ysr_model_m1 <- paste0(outcome, "_m1_ysr")
  cbcl_model_m2 <- paste0(outcome, "_m2_cbcl")
  ysr_model_m2 <- paste0(outcome, "_m2_ysr")
  
  # Compare m1 models
  if (exists(cbcl_model_m1) && exists(ysr_model_m1)) {
    model1 <- get(cbcl_model_m1)
    model2 <- get(ysr_model_m1)
    if (length(intersect(names(coef(model1)), names(coef(model2)))) > 0) {
      cbcl_vs_ysr_results[[paste0(outcome, "_m1")]] <- compare_coefficients_wald(
        model1, model2, "CBCL", "YSR", outcome, model_name = "M1")
    }
  }
  
  # Compare m2 models
  if (exists(cbcl_model_m2) && exists(ysr_model_m2)) {
    model1 <- get(cbcl_model_m2)
    model2 <- get(ysr_model_m2)
    if (length(intersect(names(coef(model1)), names(coef(model2)))) > 0) {
      cbcl_vs_ysr_results[[paste0(outcome, "_m2")]] <- compare_coefficients_wald(
        model1, model2, "CBCL", "YSR", outcome, model_name = "M2")
    }
  }
}

cbcl_vs_ysr_results_df <- bind_rows(cbcl_vs_ysr_results)

# Summary stats
cat(sprintf("\nTotal: %d | Significant: %d (%.1f%%)\n", 
            nrow(cbcl_vs_ysr_results_df),
            sum(cbcl_vs_ysr_results_df$significant),
            100 * mean(cbcl_vs_ysr_results_df$significant)))

# Create Excel workbook
wald_test <- createWorkbook()
addWorksheet(wald_test, "CBCL_vs_YSR") 
writeData(wald_test, "CBCL_vs_YSR", cbcl_vs_ysr_results_df)

if (sum(cbcl_vs_ysr_results_df$significant) > 0) {
  addWorksheet(wald_test, "Significant_Only")
  writeData(wald_test, "Significant_Only", cbcl_vs_ysr_results_df %>% filter(significant))
}

# saveWorkbook(wald_test, "/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/results/wald_test_results_EA4.xlsx", overwrite = TRUE)
# cat("Results saved!\n")
saveWorkbook(wald_test, "/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/results/wald_test_results_EA3.xlsx", overwrite = TRUE)
cat("Results saved!\n")

#########################################################################

# TEST GENDER DIFFERENCE IN GENETIC EFFECT BY INTRODUCING AN INTERACTION

# Create interaction terms for predictors
interaction_predictors_m1 <- paste0(predictor_vars_m1, ":female_ctd", collapse = " + ")
interaction_predictors_m2 <- paste0(predictor_vars_m2, ":female_ctd", collapse = " + ")

# Create interaction terms for controls (excluding female_ctd itself)
control_vars_no_female <- setdiff(control_vars, "female_ctd")
interaction_controls <- paste0(control_vars_no_female, ":female_ctd", collapse = " + ")

# Combine all interaction terms
interaction_terms_m1 <- paste(interaction_predictors_m1, interaction_controls, sep = " + ")
interaction_terms_m2 <- paste(interaction_predictors_m2, interaction_controls, sep = " + ")

# RUN REGRESSIONS AND EXPORT TO EXCEL

# Loop through each source for raw standardized and categorical variables
for (source_name in names(data_sources)) {
  data <- data_sources[[source_name]]
  for (var in raw_vars) {
    m1_name <- paste0(var, "_m1_", source_name)
    m2_name <- paste0(var, "_m2_", source_name)
    assign(m1_name, feols(as.formula(paste(var, "~", predictors_m1, "+", interaction_terms_m1, "+", controls)), data = data, cluster = "fam_id"))
    assign(m2_name, feols(as.formula(paste(var, "~", predictors_m2, "+", interaction_terms_m2, "+", controls)), data = data, cluster = "fam_id"))

    std_var <- paste0(var, "_std")
    std_m1_name <- paste0(std_var, "_m1_", source_name)
    std_m2_name <- paste0(std_var, "_m2_", source_name)
    assign(std_m1_name, feols(as.formula(paste(std_var, "~", predictors_m1, "+", interaction_terms_m1, "+", controls)), data = data, cluster = "fam_id"))
    assign(std_m2_name, feols(as.formula(paste(std_var, "~", predictors_m2, "+", interaction_terms_m2, "+", controls)), data = data, cluster = "fam_id"))
	  }
}

# Set file paths with predictor_type
file_paths <- list(
  cbcl = paste0("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/results/interaction_cbcl_", predictor_type, ".xlsx"),
  ysr = paste0("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/results/interaction_ysr_", predictor_type, ".xlsx")
)

for (source_name in names(data_sources)) {
  models <- list()
  model_names <- c()
  for (var in raw_vars) {
    m1_name <- paste0(var, "_m1_", source_name)
    m2_name <- paste0(var, "_m2_", source_name)
    std_m1_name <- paste0(var, "_std_m1_", source_name)
    std_m2_name <- paste0(var, "_std_m2_", source_name)
    models[[m1_name]] <- get(m1_name)
    models[[m2_name]] <- get(m2_name)
    models[[std_m1_name]] <- get(std_m1_name)
    models[[std_m2_name]] <- get(std_m2_name)
    model_names <- c(model_names, m1_name, m2_name, std_m1_name, std_m2_name)
  }
  process_feols_model_to_excel(models, model_names, file_paths[[source_name]])
}

# Extract results for figures
results_df <- extract_results(data_sources, raw_vars, predictor_vars_m1, types)

#########################################################################

# Updated file paths to avoid overwriting
file_paths_gender <- list(
  boys_cbcl = paste0("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/results/boys_cbcl_", predictor_type, ".xlsx"),
  boys_ysr = paste0("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/results/boys_ysr_", predictor_type, ".xlsx"),
  girls_cbcl = paste0("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/results/girls_cbcl_", predictor_type, ".xlsx"),
  girls_ysr = paste0("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/results/girls_ysr_", predictor_type, ".xlsx")
)

# RUN REGRESSIONS BY GENDER AND EXPORT TO EXCEL

# Loop through each source for raw standardized and categorical variables
for (source_name in names(data_sources_gender)) {
  data <- data_sources_gender[[source_name]]
  for (var in raw_vars) {
    m1_name <- paste0(var, "_m1_", source_name)
    m2_name <- paste0(var, "_m2_", source_name)
    assign(m1_name, feols(as.formula(paste(var, "~", predictors_m1, "+", controls)), data = data, cluster = "fam_id"))
    assign(m2_name, feols(as.formula(paste(var, "~", predictors_m2, "+", controls)), data = data, cluster = "fam_id"))

    std_var <- paste0(var, "_std")
    std_m1_name <- paste0(std_var, "_m1_", source_name)
    std_m2_name <- paste0(std_var, "_m2_", source_name)
    assign(std_m1_name, feols(as.formula(paste(std_var, "~", predictors_m1, "+", controls)), data = data, cluster = "fam_id"))
    assign(std_m2_name, feols(as.formula(paste(std_var, "~", predictors_m2, "+", controls)), data = data, cluster = "fam_id"))
  }
}

for (source_name in names(data_sources_gender)) {
  models <- list()
  model_names <- c()
  for (var in raw_vars) {
    m1_name <- paste0(var, "_m1_", source_name)
    m2_name <- paste0(var, "_m2_", source_name)
    std_m1_name <- paste0(var, "_std_m1_", source_name)
    std_m2_name <- paste0(var, "_std_m2_", source_name)
    models[[m1_name]] <- get(m1_name)
    models[[m2_name]] <- get(m2_name)
    models[[std_m1_name]] <- get(std_m1_name)
    models[[std_m2_name]] <- get(std_m2_name)
    model_names <- c(model_names, m1_name, m2_name, std_m1_name, std_m2_name)
  }
  process_feols_model_to_excel(models, model_names, file_paths_gender[[source_name]])
}

# DEFINE ITEMS FOR FIGURES BY GENDER

# Redefine samples
samples_gender <- c("boys_cbcl","girls_cbcl","boys_ysr","girls_ysr")

# Extract gender results
results_df_gender <- extract_results(data_sources_gender, raw_vars, predictor_vars_m1, types)

#########################################################################

# GENERATE FIGURES

# EA4
generate_pgi_plots(
  results_df = results_df_gender,
  samples = samples_gender,
  types = types,
  terms = c("ea4_pgi"),
  output_dir = output_dir,
  colors = c("EA PGI" = "mediumpurple3"),
  title_suffix = "EA4",
  ylim = c(-0.35,0.25)
)

# EA3
generate_pgi_plots(
  results_df = results_df_gender,
  samples = samples_gender,
  types = types,
  terms = c("ea3cog_pgi", "ea3noncog_pgi"),
  output_dir = output_dir,
  colors = c(
    "EA Cog PGI" = "lightskyblue3",
    "EA NonCog PGI" = "orchid4"
  ),
  title_suffix = "EA3",
  ylim = c(-0.35,0.25)
)

#########################################################################

# END