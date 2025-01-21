#########################################################################
# Date: September 2024														
# Description: Working with behavior variables: 
# 	residualising, standardising, and creating clinical cutoffs					
#########################################################################

module load R/4.2.2-foss-2022a-bare
R
library(data.table)
library(dplyr)
library(lubridate)

#########################################################################

# Load full sample onto R
combined_df <- fread("/groups/umcg-lifelines/tmp01/projects/ov21_0226/lalajaasko/dfs/combined_df.csv", 
	sep=",", header=TRUE)

#########################################################################

## CREATING RELEVANT DUMMIES NEEDED

# Convert the character variables to dates using the first day of the month
final_df <- combined_df %>%
	mutate(
    date_of_inclusion = paste0(date_of_inclusion, "-01"),  # Add day '01' to each date
    date_of_inclusion = ymd(date_of_inclusion)  # Convert to Date format
	) %>%
  # Calculate age in years with decimal places
	mutate(age_at_inclusion = interval(date_of_birth, date_of_inclusion) / years(1)) %>%
	mutate(female = ifelse(GENDER1M2F == 2, 1, 0))

#########################################################################

## STANDARDISING PROBLEM BEHAVIOR VARIABLES

variables <- c("anxious", "withdrawn", "somatic", "social", "thought", "attention", "delinquent", "aggressive", "internalising", "externalising")

# Standardize each variable within each source
final_df <- final_df %>%
  group_by(source) %>%
  mutate(across(all_of(variables), ~ (. - mean(.,na.rm=TRUE)) / sd(.,na.rm=TRUE), .names = "{.col}_std")) %>%
  ungroup()

#######################################################################

# Final selection of variables including PC variables at the end
final_df <- final_df %>%
  dplyr::select(
    # IDs and source
    id, mom_id, dad_id, fam_id, source, wave, UGLI_Sample,
    
    # Demographics and family info
    female, age_at_inclusion, fam_id, partner_id,
    date_of_birth, year_of_birth, month_of_birth, date_of_inclusion,
	date_of_birth_m, year_of_birth_m, month_of_birth_m, n_child_m, years_at_birth_m,
    date_of_birth_f, year_of_birth_f, month_of_birth_f, n_child_f, years_at_birth_f,


    # EA4/EA3 Polygenic Indices
    ea4_pgi, ea4_pgi_m, ea4_pgi_f, ea4_pgi_m_imp, ea4_pgi_f_imp, ea4_pgi_sum,
    ea3noncog_pgi, ea3noncog_pgi_m, ea3noncog_pgi_f, ea3noncog_pgi_m_imp, ea3noncog_pgi_f_imp, ea3noncog_pgi_sum,
	ea3cog_pgi, ea3cog_pgi_m, ea3cog_pgi_f, ea3cog_pgi_m_imp, ea3cog_pgi_f_imp, ea3cog_pgi_sum,
	
	# PCs
	pc1, pc2, pc3, pc4, pc5, pc6, pc7, pc8, pc9, pc10,
	
	# Behavior vars
	aggressive, anxious, attention, delinquent, social, somatic, thought, withdrawn, externalising, internalising, 
	# Standardised
	aggressive_std, anxious_std, attention_std, delinquent_std, social_std, somatic_std, thought_std, withdrawn_std, externalising_std, internalising_std
	)
		
#######################################################################

# Save data frame

write.table(final_df,"/groups/umcg-lifelines/tmp01/projects/ov21_0226/lalajaasko/dfs/final_df.csv",
	sep=",",row.names=FALSE,quote=FALSE)

#########################################################################

