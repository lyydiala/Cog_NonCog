library(data.table)
library(dplyr)
library(lubridate)

#########################################################################

# Load full sample onto R
combined_df <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/OUTPUT/data/combined_df.csv", 
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
  mutate(across(all_of(variables), ~ (. - mean(.,na.rm=TRUE)) / sd(.,na.rm=TRUE), .names = "{.col}_std")) %>%
  ungroup()

#######################################################################

# Final selection of variables including PC variables at the end
final_df <- final_df %>%
  dplyr::select(
    # IDs and source
    id, mom_id, dad_id, fam_id, source, wave,
    
    # Demographics and family info
    female, age_at_inclusion, partner_id,
    date_of_birth, year_of_birth, month_of_birth, date_of_inclusion,
    date_of_birth_m, year_of_birth_m, month_of_birth_m, n_child_m, years_at_birth_m,
    date_of_birth_f, year_of_birth_f, month_of_birth_f, n_child_f, years_at_birth_f,

    # Polygenic Indices
    pgi_EA4, pgi_sum_EA4,
    pgi_EA3NonCog, pgi_sum_EA3NonCog,
    pgi_EA3Cog, pgi_sum_EA3Cog,

    # PCs
    PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10,

    # Behavior vars
    aggressive, anxious, attention, delinquent, social, somatic, thought, withdrawn, externalising, internalising,
    # Standardised
    aggressive_std, anxious_std, attention_std, delinquent_std, social_std, somatic_std, thought_std, withdrawn_std, externalising_std, internalising_std
  )

#######################################################################
		
# Save data frame

write.table(final_df,"/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/OUTPUT/data/final_df.csv",
	sep=",",row.names=FALSE,quote=FALSE)

#########################################################################

