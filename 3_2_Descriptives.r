library(dplyr)
library(data.table)
library(ggplot2)
library(ggridges)
library(tidyr)
library(writexl)

#########################################################################

# Full data frame
final_df <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/OUTPUT/data/final_df.csv", 
	sep=",", header=TRUE)

# Define output directory 
output_dir <- "/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/OUTPUT/stats/"

#########################################################################

## HISTOGRAM: DATE OF BIRTH BY SOURCE

overlay_histogram_date <- function(df, date_column, file_name) {
  ggplot(df, aes(x = .data[[date_column]])) +
    geom_histogram(aes(fill = source), color = "black", alpha = 0.5, binwidth = 365) +
    scale_fill_manual(values = c("cbcl" = "green", "ysr" = "purple")) +
	scale_x_date(
      date_labels = "%Y",  
      breaks = scales::breaks_pretty(n = 10)
    ) +
    labs(
      x = "Date of Birth",
      y = "Count",
      fill = "Source"
    ) +
    theme_minimal(base_size = 12) -> plot
  
  # Save the plot
  ggsave(paste0(output_dir, file_name), plot, width = 7, height = 7)
}

# Overlayed histograms for offspring, mother, and father
overlay_histogram_date(final_df, "date_of_birth", "hist_age_offspring.png")
overlay_histogram_date(final_df, "date_of_birth_m", "hist_age_mom.png")
overlay_histogram_date(final_df, "date_of_birth_f", "hist_age_dad.png")

#########################################################################

## HISTOGRAM: NUMBER OF CHILDREN AND AGE AT INCLUSION BY SOURCE

overlay_histogram_num <- function(df, num_column, file_name, x_label) {
  ggplot(df, aes(x = .data[[num_column]], fill = source)) +
    geom_histogram(binwidth = 1, color = "black", alpha = 0.5) +
    scale_fill_manual(values = c("cbcl" = "green", "ysr" = "purple")) +
	scale_x_continuous(
	  breaks = scales::breaks_pretty(n = 5)
    ) +
    labs(
      x = paste0(x_label),
      y = "Count",
      fill = "Source"
    ) +
    theme_minimal(base_size = 12) -> plot
  
  # Save the plot
  ggsave(paste0(output_dir, file_name), plot, width = 7, height = 7)
}

# Overlayed histograms for offspring, mother, and father
overlay_histogram_num(final_df, "n_child_m", "hist_n_child_m.png", "Number of Children per Mother")
overlay_histogram_num(final_df, "n_child_f", "hist_n_child_f.png", "Number of Children per Father")
overlay_histogram_num(final_df, "age_at_inclusion", "hist_age_at_incl_offspring.png", "Age at inclusion")

#########################################################################

## DENSITY PLOTS FOR BEHAVIORAL VARIABLES

behaviors <- c("aggressive", "anxious", "attention", "delinquent", "social", "somatic", "thought", "withdrawn","externalising", "internalising")
samples <- c("ysr", "cbcl")

# Loop over each sample
for (sample in samples) {
  
  # Filter data for the current sample
  sample_df <- final_df %>% filter(source == sample)

  # Loop over each behavioral variable
  for (behavior in behaviors) {
    
    # Define raw and standardized column names
    raw_col <- behavior
    std_col <- paste0(behavior, "_std")
    
    # Filter out NA values in both raw and standardized columns
    sample_df_filtered <- sample_df %>%
      filter(!is.na(.data[[raw_col]]), !is.na(.data[[std_col]]))
    
    # Create the density plot
    density_plot <- ggplot(sample_df_filtered) +
      geom_density(aes(x = .data[[raw_col]], fill = "Raw"), alpha = 0.4) +
      geom_density(aes(x = .data[[std_col]], fill = "Standardised"), alpha = 0.4, linetype = "dotted") +
      scale_fill_manual(values = c("Raw" = "blue", "Standardised" = "orange")) +
      labs(x = "Score", y = "Density") +
      theme_minimal() +
	  theme(
	  	axis.text.x = element_text(size = 12),
		axis.text.y = element_text(size = 12),
		axis.title.y = element_text(size = 12),
		legend.text = element_text(size = 12),
		legend.title = element_text(size = 12),
		)
    
    # Save the plot
    filename <- paste0(output_dir, "density_", sample, "_", behavior, ".png")
    ggsave(filename, density_plot, width = 7, height = 7)
  }
}

## DENSITY PLOTS FOR PGI VARIABLES

# Define the PGI variables to plot (one plot per trait x method combination)
pgi_vars     <- c("pgi_EA3Cog_SBayesR",  "pgi_EA3Cog_SBayesRC",
                  "pgi_EA3NonCog_SBayesR", "pgi_EA3NonCog_SBayesRC",
                  "pgi_EA4_SBayesR",      "pgi_EA4_SBayesRC")
pgi_sum_vars <- c("pgi_sum_EA3Cog_SBayesR",  "pgi_sum_EA3Cog_SBayesRC",
                  "pgi_sum_EA3NonCog_SBayesR", "pgi_sum_EA3NonCog_SBayesRC",
                  "pgi_sum_EA4_SBayesR",      "pgi_sum_EA4_SBayesRC")
pgi_labels     <- c("Offspring EA Cognitive PGI (SBayesR)",    "Offspring EA Cognitive PGI (SBayesRC)",
                    "Offspring EA Non-Cognitive PGI (SBayesR)", "Offspring EA Non-Cognitive PGI (SBayesRC)",
                    "Offspring EA PGI (SBayesR)",               "Offspring EA PGI (SBayesRC)")
pgi_sum_labels <- c("Parental EA Cognitive PGI Decile (SBayesR)",    "Parental EA Cognitive PGI Decile (SBayesRC)",
                    "Parental EA Non-Cognitive PGI Decile (SBayesR)", "Parental EA Non-Cognitive PGI Decile (SBayesRC)",
                    "Parental EA PGI Decile (SBayesR)",               "Parental EA PGI Decile (SBayesRC)")

# Loop over each sample and each PGI variable
for (sample in samples) {
  for (i in seq_along(pgi_vars)) {
    pgi_var     <- pgi_vars[i]
    pgi_sum_var <- pgi_sum_vars[i]
    pgi_label     <- pgi_labels[i]
    pgi_sum_label <- pgi_sum_labels[i]
    
    # Filter data for the current sample and create deciles for the parental PGI variable
    sample_df <- final_df %>%
      filter(source == sample) %>%
      mutate(decile_var = ntile(.data[[pgi_sum_var]], 10))
    
    # Create the density ridge plot
    density_plot <- ggplot(sample_df, aes(x = .data[[pgi_var]], y = as.factor(decile_var), fill = as.numeric(decile_var))) +
      geom_density_ridges(scale = 3, rel_min_height = 0.01) +
      scale_y_discrete(name = pgi_sum_label) +
      scale_x_continuous(name = pgi_label) +
      scale_fill_gradient(low = "darkgreen", high = "lightgreen") +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12)
      )
    
    # Save the plot
    filename <- paste0(output_dir, "density_", sample, "_", pgi_var, ".png")
    ggsave(filename, density_plot, width = 7, height = 7)
  }
}

#########################################################################

## SUMMARY STATISTICS

# Define variables and samples
variables_raw <- c("aggressive", "anxious", "attention", "delinquent", "social", "somatic", "thought", "withdrawn", "externalising", "internalising")
variables_std <- paste0(variables_raw, "_std") 
samples <- c("ysr", "cbcl")

# Function to calculate summary statistics for continuous variables
calculate_summary_stats <- function(data, vars) {
  data %>%
    select(all_of(vars)) %>%
    summarize(
      across(
        everything(),
        list(
          Min = ~ min(., na.rm = TRUE),
          Median = ~ median(., na.rm = TRUE),
          Mean = ~ mean(., na.rm = TRUE),
          Max = ~ max(., na.rm = TRUE),
          SD = ~ sd(., na.rm = TRUE),
          Missing = ~ sum(is.na(.))
        ),
        .names = "{.fn}_{.col}"
      )
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = c("Statistic", "Variable"),
      names_sep = "_",
      values_to = "Value"
    ) %>%
    pivot_wider(
      names_from = Statistic,
      values_from = Value,
    )
}

calculate_summary_stats_by_female <- function(data, vars) {

  # Calculate summary statistics grouped by gender
  stats <- data %>%
    select(all_of(vars), female) %>%
    group_by(female) %>%
    summarize(
      across(
        all_of(vars),
        list(
          Min = ~ min(., na.rm = TRUE),
          Median = ~ median(., na.rm = TRUE),
          Mean = ~ mean(., na.rm = TRUE),
          Max = ~ max(., na.rm = TRUE),
          SD = ~ sd(., na.rm = TRUE),
          Missing = ~ sum(is.na(.))
        ),
        .names = "{.fn}.{.col}"
      ),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = -female,
      names_to = c("Statistic", "Variable"),
      names_sep = "\\.",
      values_to = "Value"
    ) %>%
    pivot_wider(
      names_from = female,
      values_from = Value,
      names_prefix = "Female_"
    )
  
  # Perform t-tests for each variable
  test_results <- vars %>%
    purrr::map_df(~ {
      var_name <- .x
      ttest <- t.test(data[[var_name]] ~ data$female)
      tibble(
        Variable = var_name,
        TestStatistic = ttest$statistic,
        PValue = ttest$p.value
      )
    })
  
  # Combine stats and test results
  result <- stats %>%
    pivot_wider(
      names_from = Statistic,
      values_from = starts_with("Female_"),
      names_sep = "_"
    ) %>%
    left_join(test_results, by = "Variable")
  
  return(result)
}

# Initialize the summary_list as an empty list
summary_list <- list()

# Loop over each sample and variable type
for (sample in samples) {
  sample_df <- final_df %>% filter(source == sample)
  
  # Calculate summary statistics for raw variables
  summary_raw <- calculate_summary_stats(sample_df, variables_raw)
  summary_list[[paste(sample, "Raw")]] <- summary_raw
  
  # Calculate summary statistics for standardized variables
  summary_std <- calculate_summary_stats(sample_df, variables_std)
  summary_list[[paste(sample, "Standardized")]] <- summary_std
    
  # Calculate summary statistics for raw variables by female
  summary_raw_female <- calculate_summary_stats_by_female(sample_df, variables_raw)
  summary_list[[paste(sample, "Raw by female")]] <- summary_raw_female
  
  # Calculate summary statistics for standardized variables by female
  summary_std_female <- calculate_summary_stats_by_female(sample_df, variables_std)
  summary_list[[paste(sample, "Standardized by female")]] <- summary_std_female
  }

# Define the output file path
output_file <- file.path(output_dir, "summary_statistics_by_sample.xlsx")

# Export all summary statistics to Excel with different sheets for each sample and type
write_xlsx(summary_list, path = output_file)

#########################################################################

# END