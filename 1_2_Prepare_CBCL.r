########################################################################
# Date: September 2024
# Description: Code to prepare socioemotional skill data CBCL
#	Items defined according to ASEBA manual
########################################################################

module load R/4.2.2-foss-2022a-bare
R
library(magrittr)
library(dplyr)
library(data.table)

#######################################################################

## Load behavioral variables into R

# Wave 1
pheno1aq_behavior <- fread("/groups/umcg-lifelines/tmp01/projects/ov21_0226/dataset_order_202207/results/1a_q_behavior_results.csv", 
	sep=",", header=TRUE)
pheno1aq_behavior <- as.data.frame(pheno1aq_behavior)

# Wave 2
pheno2aq_child <- fread("/groups/umcg-lifelines/tmp01/projects/ov21_0226/dataset_order_202207/results/2a_q_child_results.csv", 
	sep=",", header=TRUE)
pheno2aq_child <- as.data.frame(pheno2aq_child)
# Trimming whitespace from column names
pheno2aq_child <- setNames(pheno2aq_child, trimws(names(pheno2aq_child)))

# Wave 3
pheno3aq_child <- fread("/groups/umcg-lifelines/tmp01/projects/ov21_0226/dataset_order_202207/results/3a_q_child_results.csv", 
	sep=",", header=TRUE)
pheno3aq_child <- as.data.frame(pheno3aq_child)

#######################################################################

## WITHDRAWN DEPRESSED

# WAVE 1

withdrawn_w1 <- pheno1aq_behavior[c("cbcl_hobbies_chi_q_05",
	"cbcl_withdrawn_chi_q_42",
	"cbcl_refusetalk_chi_q_65",
	"cbcl_introverted_chi_q_69_v1",
	"cbcl_shy_chi_q_75_v1",
	"cbcl_lowenergy_chi_q_102_v1",
	"cbcl_depressed_chi_q_103_v1",
	"cbcl_withdrawn_chi_q_111_v1")]
withdrawn_w1 <- as.data.frame(sapply(withdrawn_w1,as.numeric))
vars_to_change <- c("cbcl_hobbies_chi_q_05",
	"cbcl_withdrawn_chi_q_42",
	"cbcl_refusetalk_chi_q_65",
	"cbcl_introverted_chi_q_69_v1",
	"cbcl_shy_chi_q_75_v1",
	"cbcl_lowenergy_chi_q_102_v1",
	"cbcl_depressed_chi_q_103_v1",
	"cbcl_withdrawn_chi_q_111_v1")
for (var in vars_to_change) {
withdrawn_w1[[var]] <- withdrawn_w1[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}
withdrawn_w1$tot1 <- rowSums(withdrawn_w1)

# WAVE 2 

withdrawn_w2 <- pheno2aq_child[c("cbcl_hobbies_chi_q_05",
	"cbcl_withdrawn_chi_q_42",
	"cbcl_refusetalk_chi_q_65",
	"cbcl_introverted_chi_q_69_v2",
	"cbcl_shy_chi_q_75_v2",
	"cbcl_lowenergy_chi_q_102_v2",
	"cbcl_depressed_chi_q_103_v2",
	"cbcl_withdrawn_chi_q_111_v2")]
withdrawn_w2 <- as.data.frame(sapply(withdrawn_w2,as.numeric))
vars_to_change <- c("cbcl_hobbies_chi_q_05",
	"cbcl_withdrawn_chi_q_42",
	"cbcl_refusetalk_chi_q_65",
	"cbcl_introverted_chi_q_69_v2",
	"cbcl_shy_chi_q_75_v2",
	"cbcl_lowenergy_chi_q_102_v2",
	"cbcl_depressed_chi_q_103_v2",
	"cbcl_withdrawn_chi_q_111_v2")
for (var in vars_to_change) {
withdrawn_w2[[var]] <- withdrawn_w2[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}
withdrawn_w2$tot1 <- rowSums(withdrawn_w2)

# WAVE 3 

withdrawn_w3 <- pheno3aq_child[c("cbcl_hobbies_chi_q_05",
	"cbcl_withdrawn_chi_q_42",
	"cbcl_refusetalk_chi_q_65",
	"cbcl_introverted_chi_q_69_v2",
	"cbcl_shy_chi_q_75_v2",
	"cbcl_lowenergy_chi_q_102_v2",
	"cbcl_depressed_chi_q_103_v2",
	"cbcl_withdrawn_chi_q_111_v2")]
withdrawn_w3 <- as.data.frame(sapply(withdrawn_w3,as.numeric))
vars_to_change <- c("cbcl_hobbies_chi_q_05",
	"cbcl_withdrawn_chi_q_42",
	"cbcl_refusetalk_chi_q_65",
	"cbcl_introverted_chi_q_69_v2",
	"cbcl_shy_chi_q_75_v2",
	"cbcl_lowenergy_chi_q_102_v2",
	"cbcl_depressed_chi_q_103_v2",
	"cbcl_withdrawn_chi_q_111_v2")
for (var in vars_to_change) {
withdrawn_w3[[var]] <- withdrawn_w3[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}
withdrawn_w3$tot1 <- rowSums(withdrawn_w3)

#######################################################################

## SOMATIC COMPLAINTS 

# WAVE 1

somatic_w1 <- pheno1aq_behavior[c("cbcl_nightmares_chi_q_47_v1",
	"cbcl_dizzy_chi_q_51",
	"cbcl_fatigue_chi_q_54",
	"cbcl_pains_chi_q_56_a",
	"cbcl_headache_chi_q_56_b",
	"cbcl_nausea_chi_q_56_c",
	"cbcl_eyeproblems_chi_q_56_d",
	"cbcl_skinproblems_chi_q_56_e",
	"cbcl_abdominalpain_chi_q_56_f",
	"cbcl_vomiting_chi_q_56_g")]
somatic_w1 <- as.data.frame(sapply(somatic_w1,as.numeric))
vars_to_change <- c("cbcl_nightmares_chi_q_47_v1",
	"cbcl_dizzy_chi_q_51",
	"cbcl_fatigue_chi_q_54",
	"cbcl_pains_chi_q_56_a",
	"cbcl_headache_chi_q_56_b",
	"cbcl_nausea_chi_q_56_c",
	"cbcl_eyeproblems_chi_q_56_d",
	"cbcl_skinproblems_chi_q_56_e",
	"cbcl_abdominalpain_chi_q_56_f",
	"cbcl_vomiting_chi_q_56_g")
for (var in vars_to_change) {
somatic_w1[[var]] <- somatic_w1[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}
somatic_w1$tot2 <- rowSums(somatic_w1)

# WAVE 2

somatic_w2 <- pheno2aq_child[c("cbcl_nightmares_chi_q_47_v2",
	"cbcl_dizzy_chi_q_51",
	"cbcl_fatigue_chi_q_54",
	"cbcl_pains_chi_q_56_a",
	"cbcl_headache_chi_q_56_b",
	"cbcl_nausea_chi_q_56_c",
	"cbcl_eyeproblems_chi_q_56_d",
	"cbcl_skinproblems_chi_q_56_e",
	"cbcl_abdominalpain_chi_q_56_f",
	"cbcl_vomiting_chi_q_56_g")]
somatic_w2 <- as.data.frame(sapply(somatic_w2,as.numeric))
vars_to_change <- c("cbcl_nightmares_chi_q_47_v2",
	"cbcl_dizzy_chi_q_51",
	"cbcl_fatigue_chi_q_54",
	"cbcl_pains_chi_q_56_a",
	"cbcl_headache_chi_q_56_b",
	"cbcl_nausea_chi_q_56_c",
	"cbcl_eyeproblems_chi_q_56_d",
	"cbcl_skinproblems_chi_q_56_e",
	"cbcl_abdominalpain_chi_q_56_f",
	"cbcl_vomiting_chi_q_56_g")
for (var in vars_to_change) {
somatic_w2[[var]] <- somatic_w2[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}
somatic_w2$tot2 <- rowSums(somatic_w2)

# WAVE 3

somatic_w3 <- pheno3aq_child[c("cbcl_nightmares_chi_q_47_v2",
	"cbcl_dizzy_chi_q_51",
	"cbcl_fatigue_chi_q_54",
	"cbcl_pains_chi_q_56_a",
	"cbcl_headache_chi_q_56_b",
	"cbcl_nausea_chi_q_56_c",
	"cbcl_eyeproblems_chi_q_56_d",
	"cbcl_skinproblems_chi_q_56_e",
	"cbcl_abdominalpain_chi_q_56_f",
	"cbcl_vomiting_chi_q_56_g")]
somatic_w3 <- as.data.frame(sapply(somatic_w3,as.numeric))
vars_to_change <- c("cbcl_nightmares_chi_q_47_v2",
	"cbcl_dizzy_chi_q_51",
	"cbcl_fatigue_chi_q_54",
	"cbcl_pains_chi_q_56_a",
	"cbcl_headache_chi_q_56_b",
	"cbcl_nausea_chi_q_56_c",
	"cbcl_eyeproblems_chi_q_56_d",
	"cbcl_skinproblems_chi_q_56_e",
	"cbcl_abdominalpain_chi_q_56_f",
	"cbcl_vomiting_chi_q_56_g")
for (var in vars_to_change) {
somatic_w3[[var]] <- somatic_w3[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}
somatic_w3$tot2 <- rowSums(somatic_w3)

#######################################################################

## ANXIOUS DEPRESSED

# WAVE 1

anxious_w1 <- pheno1aq_behavior[c("cbcl_crying_chi_q_14",
	"cbcl_afraid_chi_q_29",
	"cbcl_afraid_chi_q_30",
	"cbcl_afraid_chi_q_31",
	"cbcl_perfectionist_chi_q_32",
	"cbcl_unloved_chi_q_33",
	"cbcl_inferior_chi_q_35",
	"cbcl_nervous_chi_q_45_v1",
	"cbcl_anxious_chi_q_50",
	"cbcl_guilty_chi_q_52",
	"cbcl_shame_chi_q_71",
	"cbcl_suicidal_chi_q_91",
	"cbcl_worry_chi_q_112")]
anxious_w1 <- as.data.frame(sapply(anxious_w1,as.numeric))
vars_to_change <- c("cbcl_crying_chi_q_14",
	"cbcl_afraid_chi_q_29",
	"cbcl_afraid_chi_q_30",
	"cbcl_afraid_chi_q_31",
	"cbcl_perfectionist_chi_q_32",
	"cbcl_unloved_chi_q_33",
	"cbcl_inferior_chi_q_35",
	"cbcl_nervous_chi_q_45_v1",
	"cbcl_anxious_chi_q_50",
	"cbcl_guilty_chi_q_52",
	"cbcl_shame_chi_q_71",
	"cbcl_suicidal_chi_q_91",
	"cbcl_worry_chi_q_112")
for (var in vars_to_change) {
anxious_w1[[var]] <- anxious_w1[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}
anxious_w1$tot3 <- rowSums(anxious_w1)

# WAVE 2

anxious_w2 <- pheno2aq_child[c("cbcl_crying_chi_q_14",
	"cbcl_afraid_chi_q_29",
	"cbcl_afraid_chi_q_30",
	"cbcl_afraid_chi_q_31",
	"cbcl_perfectionist_chi_q_32",
	"cbcl_unloved_chi_q_33",
	"cbcl_inferior_chi_q_35",
	"cbcl_nervous_chi_q_45_v2",
	"cbcl_anxious_chi_q_50",
	"cbcl_guilty_chi_q_52",
	"cbcl_shame_chi_q_71",
	"cbcl_suicidal_chi_q_91",
	"cbcl_worry_chi_q_112")]
anxious_w2 <- as.data.frame(sapply(anxious_w2,as.numeric))
vars_to_change <- c("cbcl_crying_chi_q_14",
	"cbcl_afraid_chi_q_29",
	"cbcl_afraid_chi_q_30",
	"cbcl_afraid_chi_q_31",
	"cbcl_perfectionist_chi_q_32",
	"cbcl_unloved_chi_q_33",
	"cbcl_inferior_chi_q_35",
	"cbcl_nervous_chi_q_45_v2",
	"cbcl_anxious_chi_q_50",
	"cbcl_guilty_chi_q_52",
	"cbcl_shame_chi_q_71",
	"cbcl_suicidal_chi_q_91",
	"cbcl_worry_chi_q_112")
for (var in vars_to_change) {
anxious_w2[[var]] <- anxious_w2[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}
anxious_w2$tot3 <- rowSums(anxious_w2)

# WAVE 3

anxious_w3 <- pheno3aq_child[c("cbcl_crying_chi_q_14",
	"cbcl_afraid_chi_q_29",
	"cbcl_afraid_chi_q_30",
	"cbcl_afraid_chi_q_31",
	"cbcl_perfectionist_chi_q_32",
	"cbcl_unloved_chi_q_33",
	"cbcl_inferior_chi_q_35",
	"cbcl_nervous_chi_q_45_v2",
	"cbcl_anxious_chi_q_50",
	"cbcl_guilty_chi_q_52",
	"cbcl_shame_chi_q_71",
	"cbcl_suicidal_chi_q_91",
	"cbcl_worry_chi_q_112")]
anxious_w3 <- as.data.frame(sapply(anxious_w3,as.numeric))
vars_to_change <- c("cbcl_crying_chi_q_14",
	"cbcl_afraid_chi_q_29",
	"cbcl_afraid_chi_q_30",
	"cbcl_afraid_chi_q_31",
	"cbcl_perfectionist_chi_q_32",
	"cbcl_unloved_chi_q_33",
	"cbcl_inferior_chi_q_35",
	"cbcl_nervous_chi_q_45_v2",
	"cbcl_anxious_chi_q_50",
	"cbcl_guilty_chi_q_52",
	"cbcl_shame_chi_q_71",
	"cbcl_suicidal_chi_q_91",
	"cbcl_worry_chi_q_112")
for (var in vars_to_change) {
anxious_w3[[var]] <- anxious_w3[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}
anxious_w3$tot3 <- rowSums(anxious_w3)

#######################################################################

## SOCIAL PROBLEMS

# WAVE 1

social_w1 <- pheno1aq_behavior[c("cbcl_dependence_chi_q_11",
	"cbcl_lonely_chi_q_12",
	"cbcl_getalong_chi_q_25",
	"cbcl_jealous_chi_q_27_v1",
	"cbcl_suspicious_chi_q_34",
	"cbcl_accidents_chi_q_36",
	"cbcl_bullyvictim_chi_q_38",
	"cbcl_notliked_chi_q_48",
	"cbcl_awkward_chi_q_62_v1",
	"cbcl_friends_chi_q_64",
	"cbcl_speech_chi_q_79_v1")]
social_w1 <- as.data.frame(sapply(social_w1,as.numeric))
vars_to_change <- c("cbcl_dependence_chi_q_11",
	"cbcl_lonely_chi_q_12",
	"cbcl_getalong_chi_q_25",
	"cbcl_jealous_chi_q_27_v1",
	"cbcl_suspicious_chi_q_34",
	"cbcl_accidents_chi_q_36",
	"cbcl_bullyvictim_chi_q_38",
	"cbcl_notliked_chi_q_48",
	"cbcl_awkward_chi_q_62_v1",
	"cbcl_friends_chi_q_64",
	"cbcl_speech_chi_q_79_v1")
for (var in vars_to_change) {
social_w1[[var]] <- social_w1[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
social_w1$tot4 <- rowSums(social_w1)

# WAVE 2

social_w2 <- pheno2aq_child[c("cbcl_dependence_chi_q_11",
	"cbcl_lonely_chi_q_12",
	"cbcl_getalong_chi_q_25",
	"cbcl_jealous_chi_q_27_v2",
	"cbcl_suspicious_chi_q_34",
	"cbcl_accidents_chi_q_36",
	"cbcl_bullyvictim_chi_q_38",
	"cbcl_notliked_chi_q_48",
	"cbcl_awkward_chi_q_62_v2",
	"cbcl_friends_chi_q_64",
	"cbcl_speech_chi_q_79_v2")]
social_w2 <- as.data.frame(sapply(social_w2,as.numeric))
vars_to_change <- c("cbcl_dependence_chi_q_11",
	"cbcl_lonely_chi_q_12",
	"cbcl_getalong_chi_q_25",
	"cbcl_jealous_chi_q_27_v2",
	"cbcl_suspicious_chi_q_34",
	"cbcl_accidents_chi_q_36",
	"cbcl_bullyvictim_chi_q_38",
	"cbcl_notliked_chi_q_48",
	"cbcl_awkward_chi_q_62_v2",
	"cbcl_friends_chi_q_64",
	"cbcl_speech_chi_q_79_v2")
for (var in vars_to_change) {
social_w2[[var]] <- social_w2[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
social_w2$tot4 <- rowSums(social_w2)

# WAVE 3

social_w3 <- pheno3aq_child[c("cbcl_dependence_chi_q_11",
	"cbcl_lonely_chi_q_12",
	"cbcl_getalong_chi_q_25",
	"cbcl_jealous_chi_q_27_v2",
	"cbcl_suspicious_chi_q_34",
	"cbcl_accidents_chi_q_36",
	"cbcl_bullyvictim_chi_q_38",
	"cbcl_notliked_chi_q_48",
	"cbcl_awkward_chi_q_62_v2",
	"cbcl_friends_chi_q_64",
	"cbcl_speech_chi_q_79_v2")]
social_w3 <- as.data.frame(sapply(social_w3,as.numeric))
vars_to_change <- c("cbcl_dependence_chi_q_11",
	"cbcl_lonely_chi_q_12",
	"cbcl_getalong_chi_q_25",
	"cbcl_jealous_chi_q_27_v2",
	"cbcl_suspicious_chi_q_34",
	"cbcl_accidents_chi_q_36",
	"cbcl_bullyvictim_chi_q_38",
	"cbcl_notliked_chi_q_48",
	"cbcl_awkward_chi_q_62_v2",
	"cbcl_friends_chi_q_64",
	"cbcl_speech_chi_q_79_v2")
for (var in vars_to_change) {
social_w3[[var]] <- social_w3[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
social_w3$tot4 <- rowSums(social_w3)

#######################################################################

## THOUGHT PROBLEMS 

# WAVE 1

thought_w1 <- pheno1aq_behavior[c("cbcl_obsessive_chi_q_09",
	"cbcl_suicidal_chi_q_18_v1",
	"cbcl_voices_chi_q_40",
	"cbcl_tics_chi_q_46_v1",
	"cbcl_picking_chi_q_58_v1",
	"cbcl_repetitive_chi_q_66",
	"cbcl_visions_chi_q_70",
	"cbcl_sleep_chi_q_76",
	"cbcl_hoarding_chi_q_83",
	"cbcl_strange_chi_q_84_v1",
	"cbcl_thoughts_chi_q_85_v1",
	"cbcl_sleep_chi_q_100")]
thought_w1 <- as.data.frame(sapply(thought_w1,as.numeric))
vars_to_change <- c("cbcl_obsessive_chi_q_09",
	"cbcl_suicidal_chi_q_18_v1",
	"cbcl_voices_chi_q_40",
	"cbcl_tics_chi_q_46_v1",
	"cbcl_picking_chi_q_58_v1",
	"cbcl_repetitive_chi_q_66",
	"cbcl_visions_chi_q_70",
	"cbcl_sleep_chi_q_76",
	"cbcl_hoarding_chi_q_83",
	"cbcl_strange_chi_q_84_v1",
	"cbcl_thoughts_chi_q_85_v1",
	"cbcl_sleep_chi_q_100")
for (var in vars_to_change) {
thought_w1[[var]] <- thought_w1[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
thought_w1$tot5 <- rowSums(thought_w1)

# WAVE 2

thought_w2 <- pheno2aq_child[c("cbcl_obsessive_chi_q_09",
	"cbcl_suicidal_chi_q_18_v2",
	"cbcl_voices_chi_q_40",
	"cbcl_tics_chi_q_46_v2",
	"cbcl_picking_chi_q_58_v2",
	"cbcl_repetitive_chi_q_66",
	"cbcl_visions_chi_q_70",
	"cbcl_sleep_chi_q_76",
	"cbcl_hoarding_chi_q_83",
	"cbcl_strange_chi_q_84_v2",
	"cbcl_thoughts_chi_q_85_v2",
	"cbcl_sleep_chi_q_100")]
thought_w2 <- as.data.frame(sapply(thought_w2,as.numeric))
vars_to_change <- c("cbcl_obsessive_chi_q_09",
	"cbcl_suicidal_chi_q_18_v2",
	"cbcl_voices_chi_q_40",
	"cbcl_tics_chi_q_46_v2",
	"cbcl_picking_chi_q_58_v2",
	"cbcl_repetitive_chi_q_66",
	"cbcl_visions_chi_q_70",
	"cbcl_sleep_chi_q_76",
	"cbcl_hoarding_chi_q_83",
	"cbcl_strange_chi_q_84_v2",
	"cbcl_thoughts_chi_q_85_v2",
	"cbcl_sleep_chi_q_100")
for (var in vars_to_change) {
thought_w2[[var]] <- thought_w2[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
thought_w2$tot5 <- rowSums(thought_w2)

# WAVE 3

thought_w3 <- pheno3aq_child[c("cbcl_obsessive_chi_q_09",
	"cbcl_suicidal_chi_q_18_v2",
	"cbcl_voices_chi_q_40",
	"cbcl_tics_chi_q_46_v2",
	"cbcl_picking_chi_q_58_v2",
	"cbcl_repetitive_chi_q_66",
	"cbcl_visions_chi_q_70",
	"cbcl_sleep_chi_q_76",
	"cbcl_hoarding_chi_q_83",
	"cbcl_strange_chi_q_84_v2",
	"cbcl_thoughts_chi_q_85_v2",
	"cbcl_sleep_chi_q_100")]
thought_w3 <- as.data.frame(sapply(thought_w3,as.numeric))
vars_to_change <- c("cbcl_obsessive_chi_q_09",
	"cbcl_suicidal_chi_q_18_v2",
	"cbcl_voices_chi_q_40",
	"cbcl_tics_chi_q_46_v2",
	"cbcl_picking_chi_q_58_v2",
	"cbcl_repetitive_chi_q_66",
	"cbcl_visions_chi_q_70",
	"cbcl_sleep_chi_q_76",
	"cbcl_hoarding_chi_q_83",
	"cbcl_strange_chi_q_84_v2",
	"cbcl_thoughts_chi_q_85_v2",
	"cbcl_sleep_chi_q_100")
for (var in vars_to_change) {
thought_w3[[var]] <- thought_w3[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
thought_w3$tot5 <- rowSums(thought_w3)

#######################################################################

## ATTENTION PROBLEMS

# WAVE 1

attention_w1 <- pheno1aq_behavior[c("cbcl_tooyoung_chi_q_01",
	"cbcl_finishing_chi_q_04_v1",
	"cbcl_concentration_chi_q_08_v1",
	"cbcl_restless_chi_q_10",
	"cbcl_confused_chi_q_13_v1",
	"cbcl_daydreams_chi_q_17_v1",
	"cbcl_impulsive_chi_q_41",
	"cbcl_schoolwork_chi_q_61",
	"cbcl_distracted_chi_q_78")]
attention_w1 <- as.data.frame(sapply(attention_w1,as.numeric))
vars_to_change <- c("cbcl_tooyoung_chi_q_01",
	"cbcl_finishing_chi_q_04_v1",
	"cbcl_concentration_chi_q_08_v1",
	"cbcl_restless_chi_q_10",
	"cbcl_confused_chi_q_13_v1",
	"cbcl_daydreams_chi_q_17_v1",
	"cbcl_impulsive_chi_q_41",
	"cbcl_schoolwork_chi_q_61",
	"cbcl_distracted_chi_q_78")
for (var in vars_to_change) {
attention_w1[[var]] <- attention_w1[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
attention_w1$tot6 <- rowSums(attention_w1)

# WAVE 2

attention_w2 <- pheno2aq_child[c("cbcl_tooyoung_chi_q_01",
	"cbcl_finishing_chi_q_04_v2",
	"cbcl_concentration_chi_q_08_v2",
	"cbcl_restless_chi_q_10",
	"cbcl_confused_chi_q_13_v2",
	"cbcl_daydreams_chi_q_17_v2",
	"cbcl_impulsive_chi_q_41",
	"cbcl_schoolwork_chi_q_61",
	"cbcl_distracted_chi_q_78")]
attention_w2 <- as.data.frame(sapply(attention_w2,as.numeric))
vars_to_change <- c("cbcl_tooyoung_chi_q_01",
	"cbcl_finishing_chi_q_04_v2",
	"cbcl_concentration_chi_q_08_v2",
	"cbcl_restless_chi_q_10",
	"cbcl_confused_chi_q_13_v2",
	"cbcl_daydreams_chi_q_17_v2",
	"cbcl_impulsive_chi_q_41",
	"cbcl_schoolwork_chi_q_61",
	"cbcl_distracted_chi_q_78")
for (var in vars_to_change) {
attention_w2[[var]] <- attention_w2[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
attention_w2$tot6 <- rowSums(attention_w2)

# WAVE 3

attention_w3 <- pheno3aq_child[c("cbcl_tooyoung_chi_q_01",
	"cbcl_finishing_chi_q_04_v2",
	"cbcl_concentration_chi_q_08_v2",
	"cbcl_restless_chi_q_10",
	"cbcl_confused_chi_q_13_v2",
	"cbcl_daydreams_chi_q_17_v2",
	"cbcl_impulsive_chi_q_41",
	"cbcl_schoolwork_chi_q_61",
	"cbcl_distracted_chi_q_78")]
attention_w3 <- as.data.frame(sapply(attention_w3,as.numeric))
vars_to_change <- c("cbcl_tooyoung_chi_q_01",
	"cbcl_finishing_chi_q_04_v2",
	"cbcl_concentration_chi_q_08_v2",
	"cbcl_restless_chi_q_10",
	"cbcl_confused_chi_q_13_v2",
	"cbcl_daydreams_chi_q_17_v2",
	"cbcl_impulsive_chi_q_41",
	"cbcl_schoolwork_chi_q_61",
	"cbcl_distracted_chi_q_78")
for (var in vars_to_change) {
attention_w3[[var]] <- attention_w3[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
attention_w3$tot6 <- rowSums(attention_w3)

#######################################################################

## RULE BREAKING BEHAVIOR, I.E. DELINQUENT BEHAVIOR

# WAVE 1

delinquent_w1 <- pheno1aq_behavior[c("cbcl_alcohol_chi_q_02_v1",
	"cbcl_guilty_chi_q_26",
	"cbcl_rulebreaking_chi_q_28",
	"cbcl_friends_chi_q_39",
	"cbcl_lying_chi_q_43",
	"cbcl_friends_chi_q_63",
	"cbcl_runaway_chi_q_67",
	"cbcl_pyromania_chi_q_72",
	"cbcl_stealing_chi_q_81",
	"cbcl_stealing_chi_q_82",
	"cbcl_swearing_chi_q_90",
	"cbcl_sexual_chi_q_96",
	"cbcl_tobacco_chi_q_99_v1",
	"cbcl_truancy_chi_q_101",
	"cbcl_drugs_chi_q_105")]
delinquent_w1 <- as.data.frame(sapply(delinquent_w1,as.numeric))
vars_to_change <- c("cbcl_alcohol_chi_q_02_v1",
	"cbcl_guilty_chi_q_26",
	"cbcl_rulebreaking_chi_q_28",
	"cbcl_friends_chi_q_39",
	"cbcl_lying_chi_q_43",
	"cbcl_friends_chi_q_63",
	"cbcl_runaway_chi_q_67",
	"cbcl_pyromania_chi_q_72",
	"cbcl_stealing_chi_q_81",
	"cbcl_stealing_chi_q_82",
	"cbcl_swearing_chi_q_90",
	"cbcl_sexual_chi_q_96",
	"cbcl_tobacco_chi_q_99_v1",
	"cbcl_truancy_chi_q_101",
	"cbcl_drugs_chi_q_105")
for (var in vars_to_change) {
delinquent_w1[[var]] <- delinquent_w1[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
delinquent_w1$tot7 <- rowSums(delinquent_w1)

# WAVE 2

delinquent_w2 <- pheno2aq_child[c("cbcl_alcohol_chi_q_02_v1",
	"cbcl_guilty_chi_q_26",
	"cbcl_rulebreaking_chi_q_28",
	"cbcl_friends_chi_q_39",
	"cbcl_lying_chi_q_43",
	"cbcl_friends_chi_q_63",
	"cbcl_runaway_chi_q_67",
	"cbcl_pyromania_chi_q_72",
	"cbcl_stealing_chi_q_81",
	"cbcl_stealing_chi_q_82",
	"cbcl_swearing_chi_q_90",
	"cbcl_sexual_chi_q_96",
	"cbcl_tobacco_chi_q_99_v1",
	"cbcl_truancy_chi_q_101",
	"cbcl_drugs_chi_q_105")]
delinquent_w2 <- as.data.frame(sapply(delinquent_w2,as.numeric))
vars_to_change <- c("cbcl_alcohol_chi_q_02_v1",
	"cbcl_guilty_chi_q_26",
	"cbcl_rulebreaking_chi_q_28",
	"cbcl_friends_chi_q_39",
	"cbcl_lying_chi_q_43",
	"cbcl_friends_chi_q_63",
	"cbcl_runaway_chi_q_67",
	"cbcl_pyromania_chi_q_72",
	"cbcl_stealing_chi_q_81",
	"cbcl_stealing_chi_q_82",
	"cbcl_swearing_chi_q_90",
	"cbcl_sexual_chi_q_96",
	"cbcl_tobacco_chi_q_99_v1",
	"cbcl_truancy_chi_q_101",
	"cbcl_drugs_chi_q_105")
for (var in vars_to_change) {
delinquent_w2[[var]] <- delinquent_w2[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
delinquent_w2$tot7 <- rowSums(delinquent_w2)

# WAVE 3

delinquent_w3 <- pheno3aq_child[c("cbcl_alcohol_chi_q_02_v1",
	"cbcl_guilty_chi_q_26",
	"cbcl_rulebreaking_chi_q_28",
	"cbcl_friends_chi_q_39",
	"cbcl_lying_chi_q_43",
	"cbcl_friends_chi_q_63",
	"cbcl_runaway_chi_q_67",
	"cbcl_pyromania_chi_q_72",
	"cbcl_stealing_chi_q_81",
	"cbcl_stealing_chi_q_82",
	"cbcl_swearing_chi_q_90",
	"cbcl_sexual_chi_q_96",
	"cbcl_tobacco_chi_q_99_v1",
	"cbcl_truancy_chi_q_101",
	"cbcl_drugs_chi_q_105")]
delinquent_w3 <- as.data.frame(sapply(delinquent_w3,as.numeric))
vars_to_change <- c("cbcl_alcohol_chi_q_02_v1",
	"cbcl_guilty_chi_q_26",
	"cbcl_rulebreaking_chi_q_28",
	"cbcl_friends_chi_q_39",
	"cbcl_lying_chi_q_43",
	"cbcl_friends_chi_q_63",
	"cbcl_runaway_chi_q_67",
	"cbcl_pyromania_chi_q_72",
	"cbcl_stealing_chi_q_81",
	"cbcl_stealing_chi_q_82",
	"cbcl_swearing_chi_q_90",
	"cbcl_sexual_chi_q_96",
	"cbcl_tobacco_chi_q_99_v1",
	"cbcl_truancy_chi_q_101",
	"cbcl_drugs_chi_q_105")
for (var in vars_to_change) {
delinquent_w3[[var]] <- delinquent_w3[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
delinquent_w3$tot7 <- rowSums(delinquent_w3)

#######################################################################

## AGGRESSIVE BEHAVIOR

# WAVE 1

aggressive_w1 <- pheno1aq_behavior[c("cbcl_quarrelsome_chi_q_03_v1",
	"cbcl_bully_chi_q_16_v1",
	"cbcl_attentionseeking_chi_q_19",
	"cbcl_destructive_chi_q_20",
	"cbcl_destructive_chi_q_21",
	"cbcl_disobedient_chi_q_22",
	"cbcl_disobedient_chi_q_23",
	"cbcl_fights_chi_q_37",
	"cbcl_attacks_chi_q_57",
	"cbcl_yelling_chi_q_68_v1",
	"cbcl_stubborn_chi_q_86_v1",
	"cbcl_moodswings_chi_q_87",
	"cbcl_suspicious_chi_q_89_v1",
	"cbcl_bully_chi_q_94",
	"cbcl_tantrums_chi_q_95",
	"cbcl_threatening_chi_q_97",
	"cbcl_noisy_chi_q_104_v1")]
aggressive_w1 <- as.data.frame(sapply(aggressive_w1,as.numeric))
vars_to_change <- c("cbcl_quarrelsome_chi_q_03_v1",
	"cbcl_bully_chi_q_16_v1",
	"cbcl_attentionseeking_chi_q_19",
	"cbcl_destructive_chi_q_20",
	"cbcl_destructive_chi_q_21",
	"cbcl_disobedient_chi_q_22",
	"cbcl_disobedient_chi_q_23",
	"cbcl_fights_chi_q_37",
	"cbcl_attacks_chi_q_57",
	"cbcl_yelling_chi_q_68_v1",
	"cbcl_stubborn_chi_q_86_v1",
	"cbcl_moodswings_chi_q_87",
	"cbcl_suspicious_chi_q_89_v1",
	"cbcl_bully_chi_q_94",
	"cbcl_tantrums_chi_q_95",
	"cbcl_threatening_chi_q_97",
	"cbcl_noisy_chi_q_104_v1")
for (var in vars_to_change) {
aggressive_w1[[var]] <- aggressive_w1[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
aggressive_w1$tot8 <- rowSums(aggressive_w1)

# WAVE 2

aggressive_w2 <- pheno2aq_child[c("cbcl_quarrelsome_chi_q_03_v1",
	"cbcl_bully_chi_q_16_v2",
	"cbcl_attentionseeking_chi_q_19",
	"cbcl_destructive_chi_q_20",
	"cbcl_destructive_chi_q_21",
	"cbcl_disobedient_chi_q_22",
	"cbcl_disobedient_chi_q_23",
	"cbcl_fights_chi_q_37",
	"cbcl_attacks_chi_q_57",
	"cbcl_yelling_chi_q_68_v1",
	"cbcl_stubborn_chi_q_86_v2",
	"cbcl_moodswings_chi_q_87",
	"cbcl_suspicious_chi_q_89_v2",
	"cbcl_bully_chi_q_94",
	"cbcl_tantrums_chi_q_95",
	"cbcl_threatening_chi_q_97",
	"cbcl_noisy_chi_q_104_v2")]
aggressive_w2 <- as.data.frame(sapply(aggressive_w2,as.numeric))
vars_to_change <- c("cbcl_quarrelsome_chi_q_03_v1",
	"cbcl_bully_chi_q_16_v2",
	"cbcl_attentionseeking_chi_q_19",
	"cbcl_destructive_chi_q_20",
	"cbcl_destructive_chi_q_21",
	"cbcl_disobedient_chi_q_22",
	"cbcl_disobedient_chi_q_23",
	"cbcl_fights_chi_q_37",
	"cbcl_attacks_chi_q_57",
	"cbcl_yelling_chi_q_68_v1",
	"cbcl_stubborn_chi_q_86_v2",
	"cbcl_moodswings_chi_q_87",
	"cbcl_suspicious_chi_q_89_v2",
	"cbcl_bully_chi_q_94",
	"cbcl_tantrums_chi_q_95",
	"cbcl_threatening_chi_q_97",
	"cbcl_noisy_chi_q_104_v2")
for (var in vars_to_change) {
aggressive_w2[[var]] <- aggressive_w2[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
aggressive_w2$tot8 <- rowSums(aggressive_w2)

# WAVE 3

aggressive_w3 <- pheno3aq_child[c("cbcl_quarrelsome_chi_q_03_v1",
	"cbcl_bully_chi_q_16_v2",
	"cbcl_attentionseeking_chi_q_19",
	"cbcl_destructive_chi_q_20",
	"cbcl_destructive_chi_q_21",
	"cbcl_disobedient_chi_q_22",
	"cbcl_disobedient_chi_q_23",
	"cbcl_fights_chi_q_37",
	"cbcl_attacks_chi_q_57",
	"cbcl_yelling_chi_q_68_v1",
	"cbcl_stubborn_chi_q_86_v2",
	"cbcl_moodswings_chi_q_87",
	"cbcl_suspicious_chi_q_89_v2",
	"cbcl_bully_chi_q_94",
	"cbcl_tantrums_chi_q_95",
	"cbcl_threatening_chi_q_97",
	"cbcl_noisy_chi_q_104_v2")]
aggressive_w3 <- as.data.frame(sapply(aggressive_w3,as.numeric))
vars_to_change <- c("cbcl_quarrelsome_chi_q_03_v1",
	"cbcl_bully_chi_q_16_v2",
	"cbcl_attentionseeking_chi_q_19",
	"cbcl_destructive_chi_q_20",
	"cbcl_destructive_chi_q_21",
	"cbcl_disobedient_chi_q_22",
	"cbcl_disobedient_chi_q_23",
	"cbcl_fights_chi_q_37",
	"cbcl_attacks_chi_q_57",
	"cbcl_yelling_chi_q_68_v1",
	"cbcl_stubborn_chi_q_86_v2",
	"cbcl_moodswings_chi_q_87",
	"cbcl_suspicious_chi_q_89_v2",
	"cbcl_bully_chi_q_94",
	"cbcl_tantrums_chi_q_95",
	"cbcl_threatening_chi_q_97",
	"cbcl_noisy_chi_q_104_v2")
for (var in vars_to_change) {
aggressive_w3[[var]] <- aggressive_w3[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
aggressive_w3$tot8 <- rowSums(aggressive_w3)

#######################################################################

## COLLATE RESULTS

# WAVE 1

cbcl_w1 <- as.data.frame(cbind(pheno1aq_behavior$project_pseudo_id,
	withdrawn_w1$tot1,
	somatic_w1$tot2,
	anxious_w1$tot3,
	social_w1$tot4,
	thought_w1$tot5,
	attention_w1$tot6,
	delinquent_w1$tot7,
	aggressive_w1$tot8))
colnames(cbcl_w1) <- c("project_pseudo_id",
	"withdrawn",
	"somatic",
	"anxious",
	"social",
	"thought",
	"attention",
	"delinquent",
	"aggressive")
		
# WAVE 2

cbcl_w2 <- as.data.frame(cbind(pheno2aq_child$project_pseudo_id,
	withdrawn_w2$tot1,
	somatic_w2$tot2,
	anxious_w2$tot3,
	social_w2$tot4,
	thought_w2$tot5,
	attention_w2$tot6,
	delinquent_w2$tot7,
	aggressive_w2$tot8))
colnames(cbcl_w2) <- c("project_pseudo_id",
	"withdrawn",
	"somatic",
	"anxious",
	"social",
	"thought",
	"attention",
	"delinquent",
	"aggressive")

# WAVE 3

cbcl_w3 <- as.data.frame(cbind(pheno3aq_child$project_pseudo_id,
	withdrawn_w3$tot1,
	somatic_w3$tot2,
	anxious_w3$tot3,
	social_w3$tot4,
	thought_w3$tot5,
	attention_w3$tot6,
	delinquent_w3$tot7,
	aggressive_w3$tot8))
colnames(cbcl_w3) <- c("project_pseudo_id",
	"withdrawn",
	"somatic",
	"anxious",
	"social",
	"thought",
	"attention",
	"delinquent",
	"aggressive")
	
print(nrow(cbcl_w1)) #9172
length(unique(cbcl_w1$project_pseudo_id)) 
print(nrow(cbcl_w2)) #5526
length(unique(cbcl_w2$project_pseudo_id)) 
print(nrow(cbcl_w3)) #468
length(unique(cbcl_w3$project_pseudo_id))

# Save into separate data frames
write.table(cbcl_w1,"/groups/umcg-lifelines/tmp01/projects/ov21_0226/lalajaasko/dfs/cbcl_w1.csv",
	sep=",",row.names=FALSE,quote=FALSE)
write.table(cbcl_w2,"/groups/umcg-lifelines/tmp01/projects/ov21_0226/lalajaasko/dfs/cbcl_w2.csv",
	sep=",",row.names=FALSE,quote=FALSE)
write.table(cbcl_w3,"/groups/umcg-lifelines/tmp01/projects/ov21_0226/lalajaasko/dfs/cbcl_w3.csv",
	sep=",",row.names=FALSE,quote=FALSE)

#######################################################################	
	
# Check overlap between ids
common_ids_w1_w2 <- intersect(cbcl_w1$project_pseudo_id, cbcl_w2$project_pseudo_id)
print(length(common_ids_w1_w2)) #1825
common_ids_w1_w3 <- intersect(cbcl_w1$project_pseudo_id, cbcl_w3$project_pseudo_id)
print(length(common_ids_w1_w3)) #0
common_ids_w2_w3 <- intersect(cbcl_w1$project_pseudo_id, cbcl_w3$project_pseudo_id)
print(length(common_ids_w2_w3)) #0

#######################################################################	

# END
