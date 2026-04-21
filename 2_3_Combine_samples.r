library(data.table)
library(dplyr) 

#########################################################################

## Load relevant dataframes to R

# CBCL
cbcl_w1 <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/OUTPUT/data/cbcl_w1.csv", 
	sep=",", header=TRUE)
cbcl_w1 <- as.data.frame(cbcl_w1)
cbcl_w2 <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/OUTPUT/data/cbcl_w2.csv", 
	sep=",", header=TRUE)
cbcl_w2 <- as.data.frame(cbcl_w2)
cbcl_w3 <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/OUTPUT/data/cbcl_w3.csv", 
	sep=",", header=TRUE)
cbcl_w3 <- as.data.frame(cbcl_w3)

# YSR
ysr_w1 <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/OUTPUT/data/ysr_w1.csv", 
	sep=",", header=TRUE)
ysr_w1 <- as.data.frame(ysr_w1)
ysr_w2 <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/OUTPUT/data/ysr_w2.csv", 
	sep=",", header=TRUE)
ysr_w2 <- as.data.frame(ysr_w2)
ysr_w3 <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/OUTPUT/data/ysr_w3.csv", 
	sep=",", header=TRUE)
ysr_w3 <- as.data.frame(ysr_w3)

# Within-family EA-PGI
within_fam_ea_pgi <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/OUTPUT/data/within_fam_ea_pgi.csv", 
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
write.table(behavior,"/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/OUTPUT/data/behavior.csv",
	sep=",",row.names=FALSE,quote=FALSE)
	
#########################################################################
	
## COMBINE DATASETS

# BEHAVIOR DATA SET WITH PGI INFORMATION

combined_df <- behavior %>%
	inner_join(within_fam_ea_pgi, by = c("project_pseudo_id" = "id")) %>%
	rename(id = project_pseudo_id)
print(nrow(combined_df)) #7787 (old 21927)
length(unique(combined_df$id)) #4008 (old 13024)

#########################################################################

## SAMPLE SIZE

# Load linkage files to determine parental genotyping status
# (parents not in linkage files = imputed via sibling information)
linkage_gsa  <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/dataset_order_202207/linkage_files/OV21_00226_gsa_linkage_file.txt")
linkage_cyto <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/dataset_order_202207/linkage_files/OV21_00226_cytosnp_linkage_file.txt")
linkage_affy <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/dataset_order_202207/linkage_files/affymetrix_linkage_file-v1_ov21_00226.csv")

genotyped_ids <- unique(c(
    linkage_gsa$project_pseudo_id,
    linkage_cyto$project_pseudo_id,
    linkage_affy[[1]]
))

# Flag parental genotyping status
combined_df <- combined_df %>%
    mutate(
        mom_genotyped = mom_id %in% genotyped_ids,
        dad_genotyped = dad_id %in% genotyped_ids
    )

# Parental imputation breakdown
n_total        <- nrow(combined_df)
n_none_imputed <- sum( combined_df$mom_genotyped &  combined_df$dad_genotyped)
n_mom_imputed  <- sum(!combined_df$mom_genotyped &  combined_df$dad_genotyped)
n_dad_imputed  <- sum( combined_df$mom_genotyped & !combined_df$dad_genotyped)
n_both_imputed <- sum(!combined_df$mom_genotyped & !combined_df$dad_genotyped)
n_any_imputed  <- n_total - n_none_imputed

cat("Total observations:               ", n_total, "\n") #7787
cat("No imputation (both genotyped):   ", n_none_imputed, sprintf("(%.1f%%)\n", 100 * n_none_imputed / n_total)) #1497 (19.2%)
cat("At least one parent imputed:      ", n_any_imputed,  sprintf("(%.1f%%)\n", 100 * n_any_imputed  / n_total)) #6290 (80.8%)
cat("Only mom imputed:                 ", n_mom_imputed,  sprintf("(%.1f%%)\n", 100 * n_mom_imputed  / n_total)) #2120 (27.2%)
cat("Only dad imputed:                 ", n_dad_imputed,  sprintf("(%.1f%%)\n", 100 * n_dad_imputed  / n_total)) #2433 (31.2%)
cat("Both parents imputed:             ", n_both_imputed, sprintf("(%.1f%%)\n", 100 * n_both_imputed / n_total)) #1737 (22.3%)

# Observation frequency per individual
id_freq           <- table(combined_df$id)
n_unique          <- length(id_freq)
n_once            <- sum(id_freq == 1)
n_more_than_once  <- sum(id_freq > 1)
n_more_than_twice <- sum(id_freq > 2)

cat("Unique individuals:                    ", n_unique, "\n") #4008
cat("Observed only once:                    ", n_once,            sprintf("(%.1f%%)\n", 100 * n_once            / n_unique)) #744 (18.6%)
cat("Observed more than once:               ", n_more_than_once,  sprintf("(%.1f%%)\n", 100 * n_more_than_once  / n_unique)) #3264 (81.4%)
cat("Observed more than twice:              ", n_more_than_twice, sprintf("(%.1f%%)\n", 100 * n_more_than_twice / n_unique)) #515 (12.8%)

# ASEBA source breakdown
source_counts <- table(combined_df$source)
cat("YSR observations:  ", source_counts["ysr"], "\n") #3176
cat("CBCL observations: ", source_counts["cbcl"], "\n") #4611

# Save data frame (drop helper columns before saving)
combined_df <- combined_df %>%
    select(-mom_genotyped, -dad_genotyped)

write.table(combined_df, "/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/OUTPUT/data/combined_df.csv",
    sep=",", row.names=FALSE, quote=FALSE)
	
# #########################################################################

# ## CHECK OVERLAP IN IDS: UNDERSTANDING REPEATED OBSERVATIONS

# # Combine all IDs into a single vector and count their occurences
# all_ids <- c(cbcl_w1$project_pseudo_id, cbcl_w2$project_pseudo_id, cbcl_w3$project_pseudo_id,
    # ysr_w1$project_pseudo_id, ysr_w2$project_pseudo_id, ysr_w3$project_pseudo_id)
# id_counts <- table(all_ids)

# # Identify individuals who appear only once and count their number
# unique_ids <- names(id_counts[id_counts == 1])
# print(length(unique_ids)) #5419

# # Count the number of unique individuals in each dataset
# unique_individuals_by_dataset <- data.frame(
  # Dataset = c("cbcl_w1", "cbcl_w2", "cbcl_w3", "ysr_w1", "ysr_w2", "ysr_w3"),
  # Unique_Count = c(
    # sum(cbcl_w1$project_pseudo_id %in% unique_ids),
    # sum(cbcl_w2$project_pseudo_id %in% unique_ids),
    # sum(cbcl_w3$project_pseudo_id %in% unique_ids),
    # sum(ysr_w1$project_pseudo_id %in% unique_ids),
    # sum(ysr_w2$project_pseudo_id %in% unique_ids),
    # sum(ysr_w3$project_pseudo_id %in% unique_ids)
  # )
# )

# # Define datasets and labels
# datasets <- list(cbcl_w1, cbcl_w2, cbcl_w3, ysr_w1, ysr_w2, ysr_w3)
# dataset_names <- c("cbcl_w1", "cbcl_w2", "cbcl_w3", "ysr_w1", "ysr_w2", "ysr_w3")
# # Generate all pairwise combinations of datasets
# combinations <- expand.grid(dataset1 = dataset_names, dataset2 = dataset_names, stringsAsFactors = FALSE)
# # Filter out self-comparisons and keep only unique pairs
# combinations <- combinations[combinations$dataset1 < combinations$dataset2, ]
# # Calculate overlap counts
# overlap_counts <- mapply(function(d1, d2) {
  # length(intersect(datasets[[which(dataset_names == d1)]]$project_pseudo_id, 
                   # datasets[[which(dataset_names == d2)]]$project_pseudo_id))
# }, combinations$dataset1, combinations$dataset2)

# # Create dataframe summarizing overlaps
# overlap_summary <- data.frame(
  # Comparison = paste(combinations$dataset1, "&", combinations$dataset2),
  # Overlap_Count = overlap_counts
# )

# # Generate all possible 3-way combinations of datasets
# combinations_3way <- combn(dataset_names, 3, simplify = FALSE)
# # Calculate overlap counts for each 3-way combination
# overlap_counts_3way <- sapply(combinations_3way, function(combo) {
  # length(Reduce(intersect, list(
    # datasets[[which(dataset_names == combo[1])]]$project_pseudo_id, 
    # datasets[[which(dataset_names == combo[2])]]$project_pseudo_id, 
    # datasets[[which(dataset_names == combo[3])]]$project_pseudo_id
  # )))
# })
# # Create the detailed summary dataframe
# detailed_overlap_summary <- data.frame(
  # Comparison = sapply(combinations_3way, paste, collapse = " & "),
  # Overlap_Count = overlap_counts_3way
# )

# # View summaries
# print(unique_individuals_by_dataset)
# print(overlap_summary)
# print(detailed_overlap_summary)

# #########################################################################

# ## REMOVING REPEATED OBSERVATIONS VERSION 1

# # Done on full behavior sample first to check if it works correctly

# # First prioritize YSR across all waves, then keep the earliest available observation
# behavior_v1 <- behavior %>%
  # # Group by individual to prioritize YSR over CBCL across all waves
  # group_by(project_pseudo_id) %>%
  # # Arrange by source (prioritizing YSR) and wave (earliest wave first)
  # arrange(factor(source, levels = c("ysr", "cbcl")), wave) %>%
  # # Keep YSR if available, otherwise CBCL, and the earliest observation across waves
  # slice(1) %>%
  # ungroup()

# table(behavior_v1$source, behavior_v1$wave)
# # ysr_w1: 3953 (no different from full sample from original wave)
# # ysr_w2: 1640 = 2621 - 981 
# #	i.e. full(ysr_w2) - overlap(ysr_w1_w2)
# # ysr_w3: 465 (no different from full sample from original wave)
# # cbcl_w1: 3678 = 9172 - 3727 - 2541 + 943 - 169
# #	i.e. full(cbcl_w1) - overlap(cbcl_w1_ysr_w1) - overlap(cbcl_w1_ysr_w2) + overlap(cbcl_w1_ysr_w1_w2) - overlap(cbcl_w1_ysr_w3)
# # cbcl_w2: 3438 = 5526 - 1825 - 420 + 157
# #	i.e. full(cbcl_w2) - overlap(cbcl_w1_w2) - overlap(cbcl_w2_ysr_w3) + overlap(cbcl_w1_w1_ysr_w3)
# # cbcl_w3: 84 = 468 - 384 (i.e. full cbcl_w3 - overlap with cbcl_w2)

# print(nrow(behavior_v1)) #13258
# length(unique(behavior_v1$project_pseudo_id)) #13258

# #########################################################################

# ## REPEAT FOR combined_df

# # First prioritize YSR across all waves, then keep the earliest available observation
# df_v1 <- combined_df %>%
  # # Group by individual to prioritize YSR over CBCL across all waves
  # group_by(id) %>%
  # arrange(factor(source, levels = c("ysr_w1", "ysr_w2", "ysr_w3", "cbcl_w1", "cbcl_w2", "cbcl_w3")), wave) %>%
  # slice(1) %>%  # Keep YSR if available, otherwise CBCL, then the earliest observation
  # ungroup()

# print(nrow(df_v1)) #3813
# length(unique(df_v1$id)) #3813

# #########################################################################

# # END