#########################################################################
# Date: November 2024														
# Description: Code for OLS regressions on 				
#########################################################################

# Load necessary R module and libraries
module load R/4.2.2-foss-2022a-bare
R

# Import required libraries
library(data.table)  # Efficient CSV handling
library(dplyr)       # Data manipulation
library(fixest)      # Clustered standard error OLS regression
library(stargazer)   # Exporting tables
library(openxlsx)    # Exporting to Excel
library(broom)       # Tidying regression outputs
library(ggplot2)     # Plotting

#########################################################################

# PREPARE DATA

# Load dataset
final_df <- fread("/groups/umcg-lifelines/tmp01/projects/ov21_0226/lalajaasko/dfs/final_df.csv", sep=",", header=TRUE) %>%
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
output_dir <- "/groups/umcg-lifelines/tmp01/projects/ov21_0226/lalajaasko/results/" # Change to your desired directory

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
  for (sample in samples) {
    sample_df <- results_df %>% 
      filter(source == sample, term %in% terms, model %in% c("m1", "m2"))
    
    for (type in types) {
      type_df <- sample_df %>% filter(type == type)
      
      type_df <- type_df %>% 
        group_by(outcome, model, term) %>% 
        slice(1) %>%  # Ensure no duplicates
        ungroup()
      
      type_df <- type_df %>% 
        mutate(linetype = ifelse(significant, "solid", "dashed"),
               predictor_label = case_when(
                 term == "ea3cog_pgi" ~ "EA Cog PGI",
                 term == "ea3noncog_pgi" ~ "EA NonCog PGI",
                 term == "ea4_pgi" ~ "EA PGI"
               ),
               outcome_label = factor(case_when(
                 outcome == "attention" ~ "Attention Problems",
                 outcome == "externalising" ~ "Externalizing Problem Behaviors",
                 outcome == "aggressive" ~ "Aggressive Behavior",
                 outcome == "delinquent" ~ "Rule-Breaking Behavior",
                 outcome == "internalising" ~ "Internalizing Problem Behaviors",
                 outcome == "anxious" ~ "Anxious",
                 outcome == "somatic" ~ "Somatic Complaints",
                 outcome == "withdrawn" ~ "Withdrawn",
                 outcome == "social" ~ "Social Problems",
                 outcome == "thought" ~ "Thought Problems",
                 TRUE ~ as.character(outcome)  # Ensure proper labeling
               ), levels = c("Attention Problems", "Externalizing Problem Behaviors", "Aggressive Behavior", "Rule-Breaking Behavior", "Internalizing Problem Behaviors", "Anxious", "Somatic Complaints", "Withdrawn", "Social Problems", "Thought Problems"))) %>% 
        filter(!is.na(predictor_label))  # Keep only necessary predictors
      
      if (nrow(type_df) > 0) {
        plot <- ggplot(type_df, aes(
          x = outcome_label,
          y = estimate,
          color = predictor_label,
          shape = model,
          linetype = linetype
        )) +
          geom_point(size = 3, position = position_dodge(width = 0.6)) +
          geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                        width = 0.3, position = position_dodge(width = 0.6)) +
          geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 0.5) +
          scale_color_manual(values = colors) +
          scale_shape_manual(values = c("m1" = 16, "m2" = 17)) +
          scale_linetype_identity() +
          scale_y_continuous(limits = ylim) +  # Use ylim as a variable
          labs(
            y = "Estimate for offspring PGI", 
            color = "Predictor",
            shape = "Model"
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 10), 
            legend.position = "bottom"  # Move legend below
          ) + labs(x=NULL)
        
        file_name <- paste0(output_dir, sample, "_", title_suffix, "_", type, ".png")
        ggsave(file_name, plot, width = 12, height = 6)
      }
    }
  }
}

#########################################################################

# DEFINE ALL VARIABLES USED 

## Should I include also sex-age interaction?

# Define control variables
control_vars <- c("female_ctd","age_at_inclusion_std", "year_of_birth_std", "wave_std", 
                  "pc1", "pc2", "pc3", "pc4", "pc5", "pc6", "pc7", "pc8", "pc9", "pc10")
controls <- paste(control_vars, collapse = " + ")

# Define data variables
raw_vars <- c("aggressive", "anxious", "attention", "delinquent", "social", "somatic", "thought", "withdrawn", "externalising", "internalising")
std_vars <- paste0(raw_vars, "_std")

# Define datasources
data_sources <- list(cbcl = final_df_cbcl, ysr = final_df_ysr)

# Define samples, and types for figures
samples <- c("ysr", "cbcl")
types <- c("raw", "std")

#########################################################################

# DEFINE PREDICTORS

# EA4
# predictor_vars_m1 <- c("ea4_pgi")
# predictor_vars_m2 <- c("ea4_pgi", "ea4_pgi_sum")
# EA3s
 predictor_vars_m1 <- c("ea3noncog_pgi", "ea3cog_pgi")
 predictor_vars_m2 <- c("ea3noncog_pgi", "ea3cog_pgi", "ea3noncog_pgi_sum", "ea3cog_pgi_sum")

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
  cbcl = paste0("/groups/umcg-lifelines/tmp01/projects/ov21_0226/lalajaasko/results/full_cbcl_", predictor_type, ".xlsx"),
  ysr = paste0("/groups/umcg-lifelines/tmp01/projects/ov21_0226/lalajaasko/results/full_ysr_", predictor_type, ".xlsx")
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

# TEST GENDER DIFFERENCE IN GENETIC EFFECT BY INTRODUCING AN INTERACTION

# Define interaction terms
interaction_terms_m1 <- paste0(predictor_vars_m1, ":female_ctd", collapse = " + ")
interaction_terms_m2 <- paste0(predictor_vars_m2, ":female_ctd", collapse = " + ")

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
  cbcl = paste0("/groups/umcg-lifelines/tmp01/projects/ov21_0226/lalajaasko/results/interaction_cbcl_", predictor_type, ".xlsx"),
  ysr = paste0("/groups/umcg-lifelines/tmp01/projects/ov21_0226/lalajaasko/results/interaction_ysr_", predictor_type, ".xlsx")
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
  boys_cbcl = paste0("/groups/umcg-lifelines/tmp01/projects/ov21_0226/lalajaasko/results/boys_cbcl_", predictor_type, ".xlsx"),
  boys_ysr = paste0("/groups/umcg-lifelines/tmp01/projects/ov21_0226/lalajaasko/results/boys_ysr_", predictor_type, ".xlsx"),
  girls_cbcl = paste0("/groups/umcg-lifelines/tmp01/projects/ov21_0226/lalajaasko/results/girls_cbcl_", predictor_type, ".xlsx"),
  girls_ysr = paste0("/groups/umcg-lifelines/tmp01/projects/ov21_0226/lalajaasko/results/girls_ysr_", predictor_type, ".xlsx")
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

# WALD TEST

library(car)

compare_coefficients_wald <- function(model1, model2, sample1_name, sample2_name, outcome) {
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
    
    # Store results in a data frame
    results[[var_name]] <- data.frame(
      outcome = outcome,
      predictor = var_name, 
      coef_sample1 = coef1, 
      coef_sample2 = coef2, 
      diff = diff, 
      z_value = z_value, 
      p_value = p_value,
      sample1 = sample1_name,
      sample2 = sample2_name
    )
  }
  
  return(bind_rows(results))  # Combine list into a single data frame
}

# Initialize an empty list to store results
cbcl_vs_ysr_results <- list()

# Loop over each outcome variable
for (outcome in std_vars) {
  # Dynamically construct model names
  cbcl_model_m1 <- paste0(outcome, "_m1_cbcl")
  ysr_model_m1 <- paste0(outcome, "_m1_ysr")
  
  cbcl_model_m2 <- paste0(outcome, "_m2_cbcl")
  ysr_model_m2 <- paste0(outcome, "_m2_ysr")

  # Compare m1 models (CBCL vs. YSR)
  if (exists(cbcl_model_m1) && exists(ysr_model_m1)) {
    model1 <- get(cbcl_model_m1)
    model2 <- get(ysr_model_m1)

    # Ensure they have common predictors before running the test
    if (length(intersect(names(coef(model1)), names(coef(model2)))) > 0) {
      cbcl_vs_ysr_results[[paste0(outcome, "_m1")]] <- compare_coefficients_wald(
        model1, model2, "CBCL", "YSR", outcome
      )
    }
  }
  
  # Compare m2 models (CBCL vs. YSR)
  if (exists(cbcl_model_m2) && exists(ysr_model_m2)) {
    model1 <- get(cbcl_model_m2)
    model2 <- get(ysr_model_m2)

    # Ensure they have common predictors before running the test
    if (length(intersect(names(coef(model1)), names(coef(model2)))) > 0) {
      cbcl_vs_ysr_results[[paste0(outcome, "_m2")]] <- compare_coefficients_wald(
        model1, model2, "CBCL", "YSR", outcome
      )
    }
  }
}

# Combine all results into a single data frame
cbcl_vs_ysr_results_df <- bind_rows(cbcl_vs_ysr_results)

# Create an Excel workbook for the test results
wald_test <- createWorkbook()

# Add worksheet for CBCL vs. YSR results
addWorksheet(wald_test, "CBCL_vs_YSR") 
writeData(wald_test, "CBCL_vs_YSR", cbcl_vs_ysr_results_df)

# Save the workbook
saveWorkbook(wald_test, "/groups/umcg-lifelines/tmp01/projects/ov21_0226/lalajaasko/results/wald_test_results.xlsx", overwrite = TRUE)

#########################################################################

# CHOW TEST(?)

# Function to perform coefficient comparison (Z-test)
compare_coefficients <- function(model1, model2, model1_name, model2_name) {
  coef1 <- coef(model1)
  coef2 <- coef(model2)
  se1 <- sqrt(diag(vcov(model1)))
  se2 <- sqrt(diag(vcov(model2)))

  # Ensure both models have the same terms
  common_terms <- intersect(names(coef1), names(coef2))

  results <- data.frame(
    term = common_terms,
    estimate1 = coef1[common_terms],
    estimate2 = coef2[common_terms],
    std.error1 = se1[common_terms],
    std.error2 = se2[common_terms]
  )

  # Compute Z-scores
  results <- results %>%
    mutate(
      z_score = (estimate1 - estimate2) / sqrt(std.error1^2 + std.error2^2),
      p_value = 2 * (1 - pnorm(abs(z_score))) # Two-tailed test
    )

  results <- results %>%
    mutate(
      significant = p_value < 0.05
    )

  results <- results %>%
    rename(
      !!paste0("Estimate_", model1_name) := estimate1,
      !!paste0("Estimate_", model2_name) := estimate2,
      !!paste0("StdError_", model1_name) := std.error1,
      !!paste0("StdError_", model2_name) := std.error2
    )

  return(results)
}

# Initialize list to store CBCL vs YSR results
cbcl_vs_ysr_results <- list()

# Loop over each outcome variable
for (outcome in std_vars) {
  # Construct model names dynamically
  cbcl_model_m1 <- paste0(outcome, "_m1_cbcl")
  ysr_model_m1 <- paste0(outcome, "_m1_ysr")
  
  cbcl_model_m2 <- paste0(outcome, "_m2_cbcl")
  ysr_model_m2 <- paste0(outcome, "_m2_ysr")

  # Check if models exist before running the test
  if (exists(cbcl_model_m1) && exists(ysr_model_m1)) {
    cbcl_vs_ysr_results[[paste0(outcome, "_m1")]] <- compare_coefficients(
      get(cbcl_model_m1), get(ysr_model_m1), "CBCL", "YSR"
    )
  }
  
  if (exists(cbcl_model_m2) && exists(ysr_model_m2)) {
    cbcl_vs_ysr_results[[paste0(outcome, "_m2")]] <- compare_coefficients(
      get(cbcl_model_m2), get(ysr_model_m2), "CBCL", "YSR"
    )
  }
}

# Convert results to a data frame
cbcl_vs_ysr_results_df <- bind_rows(cbcl_vs_ysr_results, .id = "outcome")

# Create an Excel workbook for the Chow test results
chow_test <- createWorkbook()

# Add worksheet for CBCL vs. YSR results
addWorksheet(chow_test, "CBCL_vs_YSR") 
writeData(chow_test, "CBCL_vs_YSR", cbcl_vs_ysr_results_df)

# Save the workbook
saveWorkbook(chow_test, "/groups/umcg-lifelines/tmp01/projects/ov21_0226/lalajaasko/results/chow_test_results.xlsx", overwrite = TRUE)

#########################################################################
