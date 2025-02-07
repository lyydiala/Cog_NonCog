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

# Function to generate plots of results
generate_pgi_plots <- function(results_df, samples, types, terms, output_dir, colors, title_suffix) {
  for (sample in samples) {
    # Filter data for the current sample and terms
    sample_df <- results_df %>% 
      filter(source == sample, term %in% terms, model %in% c("m1", "m2"))
    
    for (type in types) {
      # Filter data for the current type
      type_df <- sample_df %>% filter(type == type)
      
      # Ensure no duplicates
      type_df <- type_df %>% 
        group_by(outcome, model, term) %>% 
        slice(1) %>%  # Select the first row per group
        ungroup()
      
      # Add linetype column based on significance
      type_df <- type_df %>% 
        mutate(linetype = ifelse(significant, "solid", "dashed"),
		predictor_label = case_when(
            term == "ea3cog_pgi" ~ "EA Cog PGI",
            term == "ea3noncog_pgi" ~ "EA Non-Cog PGI",
            term == "ea4_pgi" ~ "EA PGI",
            TRUE ~ term  # Fallback for other terms
			))
      
      # Create the plot if there is data
      if (nrow(type_df) > 0) {
        plot <- ggplot(type_df, aes(
          x = outcome,
          y = estimate,
          color = predictor_label,
          shape = model,
          linetype = linetype
        )) +
          geom_point(size = 3, position = position_dodge(width = 0.5)) +
          geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                        width = 0.2, position = position_dodge(width = 0.5)) +
          geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 0.5) +
		  scale_color_manual(values = colors) +
          scale_shape_manual(values = c("m1" = 16, "m2" = 17)) +
          scale_linetype_identity() +
          labs(
            x = "Behavior Variables",
            y = "Estimate for offspring PGI",
            color = "Predictor",
            shape = "Model",
            linetype = "Significance"
          ) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        # Save the plot
        file_name <- paste0(output_dir, sample, "_", title_suffix, "_", type, ".png")
        ggsave(file_name, plot, width = 10, height = 6)
      }
    }
  }
}

#########################################################################

# DEFINE VARIABLES USED 

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

# Define samples, and types for figures
samples <- c("ysr", "cbcl")
types <- c("raw", "std")

#########################################################################

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
  title_suffix = "EA4"
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
  title_suffix = "EA3"
)

#########################################################################

# RESULTS BY GENDER

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

# Updated file paths to avoid overwriting
file_paths_gender <- list(
  boys_cbcl = paste0("/groups/umcg-lifelines/tmp01/projects/ov21_0226/lalajaasko/results/boys_cbcl_", predictor_type, ".xlsx"),
  boys_ysr = paste0("/groups/umcg-lifelines/tmp01/projects/ov21_0226/lalajaasko/results/boys_ysr_", predictor_type, ".xlsx"),
  girls_cbcl = paste0("/groups/umcg-lifelines/tmp01/projects/ov21_0226/lalajaasko/results/girls_cbcl_", predictor_type, ".xlsx"),
  girls_ysr = paste0("/groups/umcg-lifelines/tmp01/projects/ov21_0226/lalajaasko/results/girls_ysr_", predictor_type, ".xlsx")
)

#########################################################################

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

#########################################################################

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
  title_suffix = "EA4"
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
  title_suffix = "EA3"
)

#########################################################################


#########################################################################

# Define file paths based on predictor type for saving Excel files
file_paths <- list(
  cbcl = paste0("/groups/umcg-lifelines/tmp01/projects/ov21_0226/lalajaasko/results/comparison_", predictor_type, "_trio_cbcl.xlsx"),
  ysr = paste0("/groups/umcg-lifelines/tmp01/projects/ov21_0226/lalajaasko/results/comparison_", predictor_type, "_trio_ysr.xlsx")
)

# Function to compare coefficients between m1 and m2 and save results to Excel
save_coefficient_comparisons <- function(results_list, file_name) {
  wb <- createWorkbook()
  for (name in names(results_list)) {
    # Shorten the sheet name to comply with Excel's 31-character limit
    short_name <- shorten_model_name(name)
    addWorksheet(wb, short_name)
    writeData(wb, short_name, results_list[[name]])
  }
  saveWorkbook(wb, file_name, overwrite = TRUE)
}

# Loop through each source and outcome, comparing m1 and m2 models and saving results
for (source_name in names(data_sources)) {
  data <- data_sources[[source_name]]
  results_list <- list()  # Reset results list for each source
  for (var in raw_vars) {
    # Define model names for raw, std, and categorical versions
    m1_name <- paste0(var, "_m1_", source_name)
    m2_name <- paste0(var, "_m2_", source_name)
    std_m1_name <- paste0(var, "_std_m1_", source_name)
    std_m2_name <- paste0(var, "_std_m2_", source_name)
    cat_m1_name <- paste0(var, "_cat_m1_", source_name)
    cat_m2_name <- paste0(var, "_cat_m2_", source_name)
    
    # Raw variable models comparison
    model1 <- get(m1_name)
    model2 <- get(m2_name)
    results_list[[paste0(m1_name, "v", m2_name)]] <- compare_coefficients(model1, model2)
    
    # Standardized variable models comparison
    std_model1 <- get(std_m1_name)
    std_model2 <- get(std_m2_name)
    results_list[[paste0(std_m1_name, "v", std_m2_name)]] <- compare_coefficients(std_model1, std_model2)
    
    # Categorical variable models comparison
    cat_model1 <- get(cat_m1_name)
    cat_model2 <- get(cat_m2_name)
    results_list[[paste0(cat_m1_name, "v", cat_m2_name)]] <- compare_coefficients(cat_model1, cat_model2)
  }
  
  # Save the results list to the appropriate file path
  save_coefficient_comparisons(results_list, file_paths[[source_name]])
}

#########################################################################
