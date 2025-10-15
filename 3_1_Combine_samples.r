#########################################################################
# Date: September 2024														
# Description: Code to combine data and define final sample								
#########################################################################

module load RPlus
R
library(data.table)
library(dplyr) 

#########################################################################

## Load relevant dataframes to R

# CBCL
cbcl_w1 <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/dfs/cbcl_w1.csv", 
	sep=",", header=TRUE)
cbcl_w1 <- as.data.frame(cbcl_w1)
cbcl_w2 <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/dfs/cbcl_w2.csv", 
	sep=",", header=TRUE)
cbcl_w2 <- as.data.frame(cbcl_w2)
cbcl_w3 <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/dfs/cbcl_w3.csv", 
	sep=",", header=TRUE)
cbcl_w3 <- as.data.frame(cbcl_w3)

# YSR
ysr_w1 <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/dfs/ysr_w1.csv", 
	sep=",", header=TRUE)
ysr_w1 <- as.data.frame(ysr_w1)
ysr_w2 <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/dfs/ysr_w2.csv", 
	sep=",", header=TRUE)
ysr_w2 <- as.data.frame(ysr_w2)
ysr_w3 <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/dfs/ysr_w3.csv", 
	sep=",", header=TRUE)
ysr_w3 <- as.data.frame(ysr_w3)

# FAD 
fad <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/dfs/fad.csv",
	sep=",", header=TRUE)
fad <- as.data.frame(fad)

# Within-family EA-PGI
within_fam_ea_pgi <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/dfs/within_fam_ea_pgi.csv", 
	sep=",", header=TRUE)
within_fam_ea_pgi <- as.data.frame(within_fam_ea_pgi)

#########################################################################

## COMBINE BEHAVIOR DATA SETS

# Add a column to each dataset indicating its source
cbcl_w1 <- cbcl_w1 %>% mutate(source = "cbcl", wave = 1)
cbcl_w2 <- cbcl_w2 %>% mutate(source = "cbcl", wave = 2)
cbcl_w3 <- cbcl_w3 %>% mutate(source = "cbcl", wave = 3)
ysr_w1 <- ysr_w1 %>% mutate(source = "ysr", wave = 1)
ysr_w2 <- ysr_w2 %>% mutate(source = "ysr", wave = 2)
ysr_w3 <- ysr_w3 %>% mutate(source = "ysr", wave = 3)

# Combine all datasets into one dataframe
behavior <- bind_rows(cbcl_w1, cbcl_w2, cbcl_w3, ysr_w1, ysr_w2, ysr_w3)
print(nrow(behavior)) #22205
length(unique(behavior$project_pseudo_id)) #13258

# Create grouped scales: internalising and externalising
behavior <- behavior %>%
  # Ensure the selected variables are numeric before applying rowSums
  mutate(across(c(withdrawn, somatic, anxious, social, thought, attention, delinquent, aggressive), as.numeric)) %>% 
  mutate(internalising = rowSums(across(c(anxious, somatic, withdrawn)), na.rm = TRUE),
    externalising = rowSums(across(c(aggressive, delinquent)), na.rm = TRUE))
	
# Save data frame
write.table(behavior,"/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/dfs/behavior.csv",
	sep=",",row.names=FALSE,quote=FALSE)
	
#########################################################################
	
## COMBINE DATASETS

# BEHAVIOR DATA SET WITH PGI INFORMATION

combined_df <- behavior %>%
	inner_join(within_fam_ea_pgi, by = c("project_pseudo_id" = "id")) %>%
	rename(id = project_pseudo_id)
print(nrow(combined_df)) #21927
length(unique(combined_df$id)) #13024

#########################################################################
	
## SAMPLE SIZE

# Only those with genetic information
combined_df <- combined_df %>%
	filter(!is.na(ea4_pgi))
print(nrow(combined_df)) #8824
length(unique(combined_df$id)) #4559

# Only those with parental genetic information
combined_df <- combined_df %>%
	filter(!is.na(ea4_pgi_sum))
print(nrow(combined_df)) #7692
length(unique(combined_df$id)) #3960

# Of which none imputed
imputed_0 <- combined_df %>%
	filter(is.na(ea4_pgi_f_imp),is.na(ea4_pgi_m_imp))
print(nrow(imputed_0)) #1473
length(unique(imputed_0$id)) #764

# Of which mother imputed
imputed_m <- combined_df %>%
	filter(is.na(ea4_pgi_f_imp),!is.na(ea4_pgi_m_imp))
print(nrow(imputed_m)) #2080
length(unique(imputed_m$id)) #1088

# Of which father imputed
imputed_f <- combined_df %>%
	filter(!is.na(ea4_pgi_f_imp),is.na(ea4_pgi_m_imp))
print(nrow(imputed_f)) # 2407
length(unique(imputed_f$id)) #1226

# Of which both imputed
imputed_2 <- combined_df %>%
	filter(!is.na(ea4_pgi_f_imp),!is.na(ea4_pgi_m_imp))
print(nrow(imputed_2)) #1732
length(unique(imputed_2$id)) #882

# Count frequency of each individual in the final sample
id_freq <- table(combined_df$id)

# Number of individuals observed only once
n_once <- sum(id_freq == 1)

# Number of individuals observed more than once
n_more_than_once <- sum(id_freq > 1)

# Total unique individuals
n_unique <- length(id_freq)

# Proportions
prop_once <- n_once / n_unique
prop_more_than_once <- n_more_than_once / n_unique

# Output
cat("Observed only once:", n_once, "\n")
cat("Observed more than once:", n_more_than_once, "\n")
cat("Proportion observed only once:", round(prop_once, 3), "\n")
cat("Proportion observed more than once:", round(prop_more_than_once, 3), "\n")

# Count how many individuals are observed more than twice
n_more_than_twice <- sum(id_freq > 2)

# Also get the proportion
prop_more_than_twice <- n_more_than_twice / length(id_freq)

# Output
cat("Observed more than twice:", n_more_than_twice, "\n")
cat("Proportion observed more than twice:", round(prop_more_than_twice, 3), "\n")

# Save data frame
write.table(combined_df,"/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/dfs/combined_df.csv",
	sep=",",row.names=FALSE,quote=FALSE)
	
#########################################################################

## CHECK OVERLAP IN IDS: UNDERSTANDING REPEATED OBSERVATIONS

# Combine all IDs into a single vector and count their occurences
all_ids <- c(cbcl_w1$project_pseudo_id, cbcl_w2$project_pseudo_id, cbcl_w3$project_pseudo_id,
    ysr_w1$project_pseudo_id, ysr_w2$project_pseudo_id, ysr_w3$project_pseudo_id)
id_counts <- table(all_ids)

# Identify individuals who appear only once and count their number
unique_ids <- names(id_counts[id_counts == 1])
print(length(unique_ids)) #5419

# Count the number of unique individuals in each dataset
unique_individuals_by_dataset <- data.frame(
  Dataset = c("cbcl_w1", "cbcl_w2", "cbcl_w3", "ysr_w1", "ysr_w2", "ysr_w3"),
  Unique_Count = c(
    sum(cbcl_w1$project_pseudo_id %in% unique_ids),
    sum(cbcl_w2$project_pseudo_id %in% unique_ids),
    sum(cbcl_w3$project_pseudo_id %in% unique_ids),
    sum(ysr_w1$project_pseudo_id %in% unique_ids),
    sum(ysr_w2$project_pseudo_id %in% unique_ids),
    sum(ysr_w3$project_pseudo_id %in% unique_ids)
  )
)

# Define datasets and labels
datasets <- list(cbcl_w1, cbcl_w2, cbcl_w3, ysr_w1, ysr_w2, ysr_w3)
dataset_names <- c("cbcl_w1", "cbcl_w2", "cbcl_w3", "ysr_w1", "ysr_w2", "ysr_w3")
# Generate all pairwise combinations of datasets
combinations <- expand.grid(dataset1 = dataset_names, dataset2 = dataset_names, stringsAsFactors = FALSE)
# Filter out self-comparisons and keep only unique pairs
combinations <- combinations[combinations$dataset1 < combinations$dataset2, ]
# Calculate overlap counts
overlap_counts <- mapply(function(d1, d2) {
  length(intersect(datasets[[which(dataset_names == d1)]]$project_pseudo_id, 
                   datasets[[which(dataset_names == d2)]]$project_pseudo_id))
}, combinations$dataset1, combinations$dataset2)

# Create dataframe summarizing overlaps
overlap_summary <- data.frame(
  Comparison = paste(combinations$dataset1, "&", combinations$dataset2),
  Overlap_Count = overlap_counts
)

# Generate all possible 3-way combinations of datasets
combinations_3way <- combn(dataset_names, 3, simplify = FALSE)
# Calculate overlap counts for each 3-way combination
overlap_counts_3way <- sapply(combinations_3way, function(combo) {
  length(Reduce(intersect, list(
    datasets[[which(dataset_names == combo[1])]]$project_pseudo_id, 
    datasets[[which(dataset_names == combo[2])]]$project_pseudo_id, 
    datasets[[which(dataset_names == combo[3])]]$project_pseudo_id
  )))
})
# Create the detailed summary dataframe
detailed_overlap_summary <- data.frame(
  Comparison = sapply(combinations_3way, paste, collapse = " & "),
  Overlap_Count = overlap_counts_3way
)

# View summaries
print(unique_individuals_by_dataset)
print(overlap_summary)
print(detailed_overlap_summary)

#########################################################################
'
## REMOVING REPEATED OBSERVATIONS VERSION 1

# Done on full behavior sample first to check if it works correctly

# First prioritize YSR across all waves, then keep the earliest available observation
behavior_v1 <- behavior %>%
  # Group by individual to prioritize YSR over CBCL across all waves
  group_by(project_pseudo_id) %>%
  # Arrange by source (prioritizing YSR) and wave (earliest wave first)
  arrange(factor(source, levels = c("ysr", "cbcl")), wave) %>%
  # Keep YSR if available, otherwise CBCL, and the earliest observation across waves
  slice(1) %>%
  ungroup()

table(behavior_v1$source, behavior_v1$wave)
# ysr_w1: 3953 (no different from full sample from original wave)
# ysr_w2: 1640 = 2621 - 981 
#	i.e. full(ysr_w2) - overlap(ysr_w1_w2)
# ysr_w3: 465 (no different from full sample from original wave)
# cbcl_w1: 3678 = 9172 - 3727 - 2541 + 943 - 169
#	i.e. full(cbcl_w1) - overlap(cbcl_w1_ysr_w1) - overlap(cbcl_w1_ysr_w2) + overlap(cbcl_w1_ysr_w1_w2) - overlap(cbcl_w1_ysr_w3)
# cbcl_w2: 3438 = 5526 - 1825 - 420 + 157
#	i.e. full(cbcl_w2) - overlap(cbcl_w1_w2) - overlap(cbcl_w2_ysr_w3) + overlap(cbcl_w1_w1_ysr_w3)
# cbcl_w3: 84 = 468 - 384 (i.e. full cbcl_w3 - overlap with cbcl_w2)

print(nrow(behavior_v1)) #13258
length(unique(behavior_v1$project_pseudo_id)) #13258

#########################################################################

## REPEAT FOR combined_df

# First prioritize YSR across all waves, then keep the earliest available observation
df_v1 <- combined_df %>%
  # Group by individual to prioritize YSR over CBCL across all waves
  group_by(id) %>%
  arrange(factor(source, levels = c("ysr_w1", "ysr_w2", "ysr_w3", "cbcl_w1", "cbcl_w2", "cbcl_w3")), wave) %>%
  slice(1) %>%  # Keep YSR if available, otherwise CBCL, then the earliest observation
  ungroup()

print(nrow(df_v1)) #3813
length(unique(df_v1$id)) #3813

#########################################################################

# END