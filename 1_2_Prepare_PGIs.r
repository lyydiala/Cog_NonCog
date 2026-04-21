library(dplyr)
library(data.table)
library(lubridate)

#######################################################################

## IMPORT RELEVANT DATASETS

# Load family relations from Lifelines
family <- read.table("/groups/umcg-lifelines/tmp02/projects/ov21_0226/dataset_order_202207/sec_family_relations/sec_family_relations.txt",
	header=TRUE, sep="\t")
family <- as.data.frame(family)

# Load all ids from Lifelines
all <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/dataset_order_202207/results/global_summary.csv", 
	sep=",", header=TRUE)
all <- as.data.frame(all)

# Select relevant variables and rename
all <- all %>%
	select(project_pseudo_id, date_of_birth, date_of_inclusion) %>%
	rename(id = project_pseudo_id)

# Reformat birthday variable for easier calculation in downstream analyses
all$year_of_birth <- as.numeric(substr(all$date_of_birth, 1, 4))
all$month_of_birth <- as.numeric(substr(all$date_of_birth, 6, 7))

print(nrow(all)) #169719
length(unique(all$id)) #169719

# Load EApgis (tab-separated)
ea_pgi <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/INPUT/EApgis.txt", 
	sep="\t", header=TRUE)
ea_pgi <- as.data.frame(ea_pgi)

print(nrow(ea_pgi)) #43392

#########################################################################

## TRIO FAMILY STRUCTURE

# Merge family structure based on id
parent <- merge(all, family, by.x="id", by.y="PROJECT_PSEUDO_ID")

# Rename and reformat variables
parent <- parent %>% 
	rename(mom_id = MOTHER_PROJECT_PSEUDO_ID,
	dad_id = FATHER_PROJECT_PSEUDO_ID,
	fam_id = FAM_ID,
	partner_id = PARTNER_PROJECT_PSEUDO_ID) %>% 
	mutate(mom_id = ifelse(mom_id == 0, NA, mom_id),
	dad_id = ifelse(dad_id == 0, NA, dad_id))

print(nrow(parent)) #167781
length(unique(parent$id)) #167781
any(duplicated(parent$id))  # FALSE
print(sum(complete.cases(parent$mom_id))) #80090
print(sum(complete.cases(parent$dad_id))) #80090

parent <- parent %>%
	filter(!is.na(mom_id)) %>%
	filter(!is.na(dad_id))
print(nrow(parent)) #80090

#######################################################################

## FURTHER INFORMATION ABOUT PARENTS 

# Number of children per parent
mom <- parent %>%
	group_by(mom_id) %>%
	summarise(n_child_m = n_distinct(id)) %>%
	ungroup()
dad <- parent %>%
	group_by(dad_id) %>%
	summarise(n_child_f = n_distinct(id)) %>%
	ungroup()
  
print(nrow(mom)) #44598
length(unique(mom$mom_id)) #44598
print(nrow(dad)) #44703
length(unique(dad$dad_id)) #44703

# Age of parent
mom <- merge(mom, all, by.x="mom_id",by.y="id", all.x=TRUE)
mom <- mom %>%
	rename(date_of_birth_m = date_of_birth,
	year_of_birth_m = year_of_birth,
	month_of_birth_m = month_of_birth) %>%
	select(-date_of_inclusion)
dad <- merge(dad, all, by.x="dad_id",by.y="id", all.x=TRUE)
dad <- dad %>%
	rename(date_of_birth_f = date_of_birth,
	year_of_birth_f = year_of_birth,
	month_of_birth_f = month_of_birth) %>%
	select(-date_of_inclusion)
	
# Merge this information back into the parent dataset
parent <- parent %>%
	left_join(mom, by = "mom_id") %>%
	left_join(dad, by = "dad_id")

# Convert 'date_of_birth' (offspring) and 'date_of_birth_m' (mother) to Date objects
parent$date_of_birth <- ymd(paste0(parent$date_of_birth, "-01"))
parent$date_of_birth_m <- ymd(paste0(parent$date_of_birth_m, "-01"))
parent$date_of_birth_f <- ymd(paste0(parent$date_of_birth_f, "-01"))

# Calculate parents age at birth (years_at_birth_m)
parent$years_at_birth_m <- as.numeric(interval(parent$date_of_birth_m, parent$date_of_birth) / years(1))
parent$years_at_birth_f <- as.numeric(interval(parent$date_of_birth_f, parent$date_of_birth) / years(1))

#########################################################################

## MERGING FAMILY RELATIONS AND EA PGI

# Merge by project_pseudo_id (now available in ea_pgi via linkage)
within_fam_ea_pgi <- merge(parent, ea_pgi, by.x = "id", by.y = "IID")

print(nrow(within_fam_ea_pgi)) #42845

# Remove duplicates: keep row with fewest NAs in PGI columns
pgi_cols <- c(
	"pgi_EA3Cog_SBayesR",  "pgi_sum_EA3Cog_SBayesR",
	"pgi_EA3Cog_SBayesRC", "pgi_sum_EA3Cog_SBayesRC",
	"pgi_EA3NonCog_SBayesR",  "pgi_sum_EA3NonCog_SBayesR",
	"pgi_EA3NonCog_SBayesRC", "pgi_sum_EA3NonCog_SBayesRC",
	"pgi_EA4_SBayesR",  "pgi_sum_EA4_SBayesR",
	"pgi_EA4_SBayesRC", "pgi_sum_EA4_SBayesRC"
)

within_fam_ea_pgi <- within_fam_ea_pgi %>%
	mutate(n_na = rowSums(is.na(across(all_of(pgi_cols))))) %>%
	group_by(id) %>%
	slice_min(n_na, n = 1, with_ties = FALSE) %>%
	ungroup() %>%
	select(-n_na)

print(nrow(within_fam_ea_pgi)) #42845
print(sum(duplicated(within_fam_ea_pgi$id))) #0

# Final selection of variables
within_fam_ea_pgi <- within_fam_ea_pgi %>%
	select(
		# IDs
		id, mom_id, dad_id,

		# Demographics and family info
		GENDER1M2F, fam_id, partner_id,
		date_of_birth, year_of_birth, month_of_birth, date_of_inclusion,
		date_of_birth_m, year_of_birth_m, month_of_birth_m, n_child_m, years_at_birth_m,
		date_of_birth_f, year_of_birth_f, month_of_birth_f, n_child_f, years_at_birth_f,

		# Polygenic Indices
		pgi_EA3Cog_SBayesR,  pgi_sum_EA3Cog_SBayesR,
		pgi_EA3Cog_SBayesRC, pgi_sum_EA3Cog_SBayesRC,
		pgi_EA3NonCog_SBayesR,  pgi_sum_EA3NonCog_SBayesR,
		pgi_EA3NonCog_SBayesRC, pgi_sum_EA3NonCog_SBayesRC,
		pgi_EA4_SBayesR,  pgi_sum_EA4_SBayesR,
		pgi_EA4_SBayesRC, pgi_sum_EA4_SBayesRC,

		# PCs
		PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10)

# Save data frame
write.table(within_fam_ea_pgi, "/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/OUTPUT/data/within_fam_ea_pgi.csv",
        sep=",", row.names=FALSE, quote=FALSE)

#######################################################################

# SAMPLE SIZES WITH GENETIC INFORMATION

# Total number of trios including imputed values
print(sum(complete.cases(within_fam_ea_pgi[, c("pgi_EA4_SBayesR","pgi_sum_EA4_SBayesR")]))) #42845

#######################################################################

# END