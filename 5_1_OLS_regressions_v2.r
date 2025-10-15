#########################################################################
# Date: June 2025														
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

# Output directory
output_dir <- "/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/results_v2/" # Change to your desired directory

#########################################################################

# Define function to process models and export results to excel

feols_to_excel <- function(models, model_names, file_name) {
  wb <- createWorkbook()
  all_results <- list()  # Collect all tidy results here
  
  for (i in seq_along(models)) {
    model <- models[[i]]
    model_name <- model_names[[i]]
    
    tidy_results <- tidy(model)
    summary_stats <- data.frame(
      term = c("Adjusted R2", "Sample size"),
      estimate = c(glance(model)$adj.r.squared, nobs(model))
    )
    
    final_results <- bind_rows(tidy_results, summary_stats)
    addWorksheet(wb, model_name)
    writeData(wb, model_name, final_results)
    
    all_results[[model_name]] <- final_results  # Save under model name
  }
  
  saveWorkbook(wb, file_name, overwrite = TRUE)
  return(all_results)  # Return all model results
}

# Define variables


# Run regressions through all outcome variables with controls
EAfull <- list()
for (var_list in outcome_vars) {
	models <- list() 
	model_names <- c()
	subset_name <- strsplit(var_list[1],"_")[[1]][1]
	for (var in var_list) {
		m1_name <- paste0(var,"_m1")
		m2_name <- paste0(var,"_m2")
		assign(m1_name, feols(as.formula(paste(var,"~",predictors_m1,"+",controls"|"fe)),data=cito_trios,cluster=fam_id_cbs)
		assign(m2_name, feols(as.formula(paste(var,"~",predictors_m2,"+",controls"|"fe)),data=cito_trios,cluster=fam_id_cbs)
		models[[m1_name]] <- get(m1_name)
		models[[m2_name]] <- get(m2_name)
		model_names <- c(model_names, m1_name, m2_name)
	}
	all_results <- feols_to_excel(models, model_names, paste0(output_dir,"EAfull_",subset_name,".xlsx"))
	EAfull[[subset_name]] <- all_results
	rm(models)
}


# Plots

library(dplyr)
library(ggplot2)

# Filter to one sample and type
type_df <- results_df %>%
  filter(source == sample, 
         type == type, 
         term %in% terms, 
         model %in% c("m1", "m2")) %>%
  group_by(outcome, model, term) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    linetype = ifelse(significant, "solid", "dashed"),
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
      TRUE ~ as.character(outcome)
    ), levels = c(
      "Attention Problems", "Externalizing Problem Behaviors", "Aggressive Behavior",
      "Rule-Breaking Behavior", "Internalizing Problem Behaviors", "Anxious",
      "Somatic Complaints", "Withdrawn", "Social Problems", "Thought Problems"
    ))
  ) %>%
  filter(!is.na(predictor_label))


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
  scale_y_continuous(limits = ylim) +
  labs(
    y = "Estimate for offspring PGI",
    color = "Predictor",
    shape = "Model"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "bottom"
  ) +
  labs(x = NULL)

print(plot)
