library(magrittr)
library(dplyr)
library(data.table)

#######################################################################

## Load behavioral variables into R

# Wave 1
pheno1aq_youth <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/dataset_order_202207/results/1a_q_youth_results.csv", 
	sep=",", header=TRUE)
pheno1aq_youth <- as.data.frame(pheno1aq_youth)

# Wave 2
pheno2aq_youth <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/dataset_order_202207/results/2a_q_youth_results.csv", 
	sep=",", header=TRUE)
pheno2aq_youth <- as.data.frame(pheno2aq_youth)
# Trimming whitespace from column names
pheno2aq_youth <- setNames(pheno2aq_youth, trimws(names(pheno2aq_youth)))

# Wave 3
pheno3aq_youth <- fread("/groups/umcg-lifelines/tmp02/projects/ov21_0226/dataset_order_202207/results/3a_q_youth_results.csv", 
	sep=",", header=TRUE)
pheno3aq_youth <- as.data.frame(pheno3aq_youth)

#######################################################################

## WITHDRAWN DEPRESSED

# WAVE 1

withdrawn_w1 <- pheno1aq_youth[c("ysr_hobbies_ach_q_05",
	"ysr_alone_ach_q_42",
	"ysr_refusetalk_ach_q_72", #cbcl_65 ysr_72
	"ysr_withdrawn_ach_q_76", #cbcl_69 ysr_76
	"ysr_shy_ach_q_82", #cbcl_75 ysr_82
	"ysr_lowenergy_ach_q_109", #cbcl_102 ysr_109
	"ysr_unhappy_ach_q_110", #cbcl_103 ysr_110
	"ysr_withdrawn_ach_q_118")] #cbcl_111 ysr_118
withdrawn_w1 <- as.data.frame(sapply(withdrawn_w1,as.numeric))
vars_to_change <- c("ysr_hobbies_ach_q_05",
	"ysr_alone_ach_q_42",
	"ysr_refusetalk_ach_q_72",
	"ysr_withdrawn_ach_q_76",
	"ysr_shy_ach_q_82",
	"ysr_lowenergy_ach_q_109",
	"ysr_unhappy_ach_q_110",
	"ysr_withdrawn_ach_q_118")
for (var in vars_to_change) {
withdrawn_w1[[var]] <- withdrawn_w1[[var]] %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}
withdrawn_w1$tot1 <- rowSums(withdrawn_w1)

# WAVE 2 

withdrawn_w2 <- pheno2aq_youth[c("ysr_hobbies_ach_q_05",
	"ysr_alone_ach_q_42",
	"ysr_refusetalk_ach_q_72", #cbcl_65 ysr_72
	"ysr_withdrawn_ach_q_76", #cbcl_69 ysr_76
	"ysr_shy_ach_q_82", #cbcl_75 ysr_82
	"ysr_lowenergy_ach_q_109", #cbcl_102 ysr_109
	"ysr_unhappy_ach_q_110", #cbcl_103 ysr_110
	"ysr_withdrawn_ach_q_118")] #cbcl_111 ysr_118
withdrawn_w2 <- as.data.frame(sapply(withdrawn_w2,as.numeric))
vars_to_change <- c("ysr_hobbies_ach_q_05",
	"ysr_alone_ach_q_42",
	"ysr_refusetalk_ach_q_72",
	"ysr_withdrawn_ach_q_76",
	"ysr_shy_ach_q_82",
	"ysr_lowenergy_ach_q_109",
	"ysr_unhappy_ach_q_110",
	"ysr_withdrawn_ach_q_118")
for (var in vars_to_change) {
withdrawn_w2[[var]] <- withdrawn_w2[[var]] %>%
	as.numeric() %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}
withdrawn_w2$tot1 <- rowSums(withdrawn_w2)

# WAVE 3 

withdrawn_w3 <- pheno3aq_youth[c("ysr_hobbies_ach_q_05",
	"ysr_alone_ach_q_42",
	"ysr_refusetalk_ach_q_72", #cbcl_65 ysr_72
	"ysr_withdrawn_ach_q_76", #cbcl_69 ysr_76
	"ysr_shy_ach_q_82", #cbcl_75 ysr_82
	"ysr_lowenergy_ach_q_109", #cbcl_102 ysr_109
	"ysr_unhappy_ach_q_110", #cbcl_103 ysr_110
	"ysr_withdrawn_ach_q_118")] #cbcl_111 ysr_118
withdrawn_w3 <- as.data.frame(sapply(withdrawn_w3,as.numeric))
vars_to_change <- c("ysr_hobbies_ach_q_05",
	"ysr_alone_ach_q_42",
	"ysr_refusetalk_ach_q_72",
	"ysr_withdrawn_ach_q_76",
	"ysr_shy_ach_q_82",
	"ysr_lowenergy_ach_q_109",
	"ysr_unhappy_ach_q_110",
	"ysr_withdrawn_ach_q_118")
for (var in vars_to_change) {
withdrawn_w3[[var]] <- withdrawn_w3[[var]] %>%
	as.numeric() %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}
withdrawn_w3$tot1 <- rowSums(withdrawn_w3)

#######################################################################

## SOMATIC COMPLAINTS 

# WAVE 1

somatic_w1 <- pheno1aq_youth[c("ysr_nightmares_ach_q_47",
	"ysr_dizzy_ach_q_51",
	"ysr_fatigue_ach_q_54",
	"ysr_pains_ach_q_56",
	"ysr_headache_ach_q_57",
	"ysr_nausea_ach_q_58",
	"ysr_eyeproblems_ach_q_59",
	"ysr_skinproblems_ach_q_60",
	"ysr_abdominalpain_ach_q_61",
	"ysr_vomiting_ach_q_62")]
somatic_w1 <- as.data.frame(sapply(somatic_w1,as.numeric))
vars_to_change <- c("ysr_nightmares_ach_q_47",
	"ysr_dizzy_ach_q_51",
	"ysr_fatigue_ach_q_54",
	"ysr_pains_ach_q_56",
	"ysr_headache_ach_q_57",
	"ysr_nausea_ach_q_58",
	"ysr_eyeproblems_ach_q_59",
	"ysr_skinproblems_ach_q_60",
	"ysr_abdominalpain_ach_q_61",
	"ysr_vomiting_ach_q_62")
for (var in vars_to_change) {
somatic_w1[[var]] <- somatic_w1[[var]] %>%
	as.numeric() %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}
somatic_w1$tot2 <- rowSums(somatic_w1)

# WAVE 2

somatic_w2 <- pheno2aq_youth[c("ysr_nightmares_ach_q_47",
	"ysr_dizzy_ach_q_51",
	"ysr_fatigue_ach_q_54",
	"ysr_pains_ach_q_56",
	"ysr_headache_ach_q_57",
	"ysr_nausea_ach_q_58",
	"ysr_eyeproblems_ach_q_59",
	"ysr_skinproblems_ach_q_60",
	"ysr_abdominalpain_ach_q_61",
	"ysr_vomiting_ach_q_62")]
somatic_w2 <- as.data.frame(sapply(somatic_w2,as.numeric))
vars_to_change <- c("ysr_nightmares_ach_q_47",
	"ysr_dizzy_ach_q_51",
	"ysr_fatigue_ach_q_54",
	"ysr_pains_ach_q_56",
	"ysr_headache_ach_q_57",
	"ysr_nausea_ach_q_58",
	"ysr_eyeproblems_ach_q_59",
	"ysr_skinproblems_ach_q_60",
	"ysr_abdominalpain_ach_q_61",
	"ysr_vomiting_ach_q_62")
for (var in vars_to_change) {
somatic_w2[[var]] <- somatic_w2[[var]] %>%
	as.numeric() %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}
somatic_w2$tot2 <- rowSums(somatic_w2)

# WAVE 3

somatic_w3 <- pheno3aq_youth[c("ysr_nightmares_ach_q_47",
	"ysr_dizzy_ach_q_51",
	"ysr_fatigue_ach_q_54",
	"ysr_pains_ach_q_56",
	"ysr_headache_ach_q_57",
	"ysr_nausea_ach_q_58",
	"ysr_eyeproblems_ach_q_59",
	"ysr_skinproblems_ach_q_60",
	"ysr_abdominalpain_ach_q_61",
	"ysr_vomiting_ach_q_62")]
somatic_w3 <- as.data.frame(sapply(somatic_w3,as.numeric))
vars_to_change <- c("ysr_nightmares_ach_q_47",
	"ysr_dizzy_ach_q_51",
	"ysr_fatigue_ach_q_54",
	"ysr_pains_ach_q_56",
	"ysr_headache_ach_q_57",
	"ysr_nausea_ach_q_58",
	"ysr_eyeproblems_ach_q_59",
	"ysr_skinproblems_ach_q_60",
	"ysr_abdominalpain_ach_q_61",
	"ysr_vomiting_ach_q_62")
for (var in vars_to_change) {
somatic_w3[[var]] <- somatic_w3[[var]] %>%
	as.numeric() %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}
somatic_w3$tot2 <- rowSums(somatic_w3)

#######################################################################

## ANXIOUS DEPRESSED

# WAVE 1

anxious_w1 <- pheno1aq_youth[c("ysr_crying_ach_q_14",
	"ysr_afraid_ach_q_29",
	"ysr_afraid_ach_q_30",
	"ysr_afraid_ach_q_31",
	"ysr_perfectionist_ach_q_32",
	"ysr_unloved_ach_q_33",
	"ysr_inferior_ach_q_35",
	"ysr_nervous_ach_q_45",
	"ysr_frightened_ach_q_50",
	"ysr_guilty_ach_q_52",
	"ysr_ashamed_ach_q_78",
	"ysr_suicidal_ach_q_98",
	"ysr_worry_ach_q_119")]
anxious_w1 <- as.data.frame(sapply(anxious_w1,as.numeric))
vars_to_change <- c("ysr_crying_ach_q_14",
	"ysr_afraid_ach_q_29",
	"ysr_afraid_ach_q_30",
	"ysr_afraid_ach_q_31",
	"ysr_perfectionist_ach_q_32",
	"ysr_unloved_ach_q_33",
	"ysr_inferior_ach_q_35",
	"ysr_nervous_ach_q_45",
	"ysr_frightened_ach_q_50",
	"ysr_guilty_ach_q_52",
	"ysr_ashamed_ach_q_78",
	"ysr_suicidal_ach_q_98",
	"ysr_worry_ach_q_119")
for (var in vars_to_change) {
anxious_w1[[var]] <- anxious_w1[[var]] %>%
	as.numeric() %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}
anxious_w1$tot3 <- rowSums(anxious_w1)

# WAVE 2

anxious_w2 <- pheno2aq_youth[c("ysr_crying_ach_q_14",
	"ysr_afraid_ach_q_29",
	"ysr_afraid_ach_q_30",
	"ysr_afraid_ach_q_31",
	"ysr_perfectionist_ach_q_32",
	"ysr_unloved_ach_q_33",
	"ysr_inferior_ach_q_35",
	"ysr_nervous_ach_q_45",
	"ysr_frightened_ach_q_50",
	"ysr_guilty_ach_q_52",
	"ysr_ashamed_ach_q_78",
	"ysr_suicidal_ach_q_98",
	"ysr_worry_ach_q_119")]
anxious_w2 <- as.data.frame(sapply(anxious_w2,as.numeric))
vars_to_change <- c("ysr_crying_ach_q_14",
	"ysr_afraid_ach_q_29",
	"ysr_afraid_ach_q_30",
	"ysr_afraid_ach_q_31",
	"ysr_perfectionist_ach_q_32",
	"ysr_unloved_ach_q_33",
	"ysr_inferior_ach_q_35",
	"ysr_nervous_ach_q_45",
	"ysr_frightened_ach_q_50",
	"ysr_guilty_ach_q_52",
	"ysr_ashamed_ach_q_78",
	"ysr_suicidal_ach_q_98",
	"ysr_worry_ach_q_119")
for (var in vars_to_change) {
anxious_w2[[var]] <- anxious_w2[[var]] %>%
	as.numeric() %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}
anxious_w2$tot3 <- rowSums(anxious_w2)

# WAVE 3

anxious_w3 <- pheno3aq_youth[c("ysr_crying_ach_q_14",
	"ysr_afraid_ach_q_29",
	"ysr_afraid_ach_q_30",
	"ysr_afraid_ach_q_31",
	"ysr_perfectionist_ach_q_32",
	"ysr_unloved_ach_q_33",
	"ysr_inferior_ach_q_35",
	"ysr_nervous_ach_q_45",
	"ysr_frightened_ach_q_50",
	"ysr_guilty_ach_q_52",
	"ysr_ashamed_ach_q_78",
	"ysr_suicidal_ach_q_98",
	"ysr_worry_ach_q_119")]
anxious_w3 <- as.data.frame(sapply(anxious_w3,as.numeric))
vars_to_change <- c("ysr_crying_ach_q_14",
	"ysr_afraid_ach_q_29",
	"ysr_afraid_ach_q_30",
	"ysr_afraid_ach_q_31",
	"ysr_perfectionist_ach_q_32",
	"ysr_unloved_ach_q_33",
	"ysr_inferior_ach_q_35",
	"ysr_nervous_ach_q_45",
	"ysr_frightened_ach_q_50",
	"ysr_guilty_ach_q_52",
	"ysr_ashamed_ach_q_78",
	"ysr_suicidal_ach_q_98",
	"ysr_worry_ach_q_119")
for (var in vars_to_change) {
anxious_w3[[var]] <- anxious_w3[[var]] %>%
	as.numeric() %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}
anxious_w3$tot3 <- rowSums(anxious_w3)

#######################################################################

## SOCIAL PROBLEMS

# WAVE 1

social_w1 <- pheno1aq_youth[c("ysr_dependence_ach_q_11",
	"ysr_lonely_ach_q_12",
	"ysr_getalong_ach_q_25",
	"ysr_jealous_ach_q_27",
	"ysr_suspicious_ach_q_34",
	"ysr_accidents_ach_q_36",
	"ysr_bullyvictim_ach_q_38",
	"ysr_notliked_ach_q_48",
	"ysr_awkward_ach_q_69",
	"ysr_friends_ach_q_71",
	"ysr_speech_ach_q_86")]
social_w1 <- as.data.frame(sapply(social_w1,as.numeric))
vars_to_change <- c("ysr_dependence_ach_q_11",
	"ysr_lonely_ach_q_12",
	"ysr_getalong_ach_q_25",
	"ysr_jealous_ach_q_27",
	"ysr_suspicious_ach_q_34",
	"ysr_accidents_ach_q_36",
	"ysr_bullyvictim_ach_q_38",
	"ysr_notliked_ach_q_48",
	"ysr_awkward_ach_q_69",
	"ysr_friends_ach_q_71",
	"ysr_speech_ach_q_86")
for (var in vars_to_change) {
social_w1[[var]] <- social_w1[[var]] %>%
	as.numeric() %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
social_w1$tot4 <- rowSums(social_w1)

# WAVE 2

social_w2 <- pheno2aq_youth[c("ysr_dependence_ach_q_11",
	"ysr_lonely_ach_q_12",
	"ysr_getalong_ach_q_25",
	"ysr_jealous_ach_q_27",
	"ysr_suspicious_ach_q_34",
	"ysr_accidents_ach_q_36",
	"ysr_bullyvictim_ach_q_38",
	"ysr_notliked_ach_q_48",
	"ysr_awkward_ach_q_69",
	"ysr_friends_ach_q_71",
	"ysr_speech_ach_q_86")]
social_w2 <- as.data.frame(sapply(social_w2,as.numeric))
vars_to_change <- c("ysr_dependence_ach_q_11",
	"ysr_lonely_ach_q_12",
	"ysr_getalong_ach_q_25",
	"ysr_jealous_ach_q_27",
	"ysr_suspicious_ach_q_34",
	"ysr_accidents_ach_q_36",
	"ysr_bullyvictim_ach_q_38",
	"ysr_notliked_ach_q_48",
	"ysr_awkward_ach_q_69",
	"ysr_friends_ach_q_71",
	"ysr_speech_ach_q_86")
for (var in vars_to_change) {
social_w2[[var]] <- social_w2[[var]] %>%
	as.numeric() %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
social_w2$tot4 <- rowSums(social_w2)

# WAVE 3

social_w3 <- pheno3aq_youth[c("ysr_dependence_ach_q_11",
	"ysr_lonely_ach_q_12",
	"ysr_getalong_ach_q_25",
	"ysr_jealous_ach_q_27",
	"ysr_suspicious_ach_q_34",
	"ysr_accidents_ach_q_36",
	"ysr_bullyvictim_ach_q_38",
	"ysr_notliked_ach_q_48",
	"ysr_awkward_ach_q_69",
	"ysr_friends_ach_q_71",
	"ysr_speech_ach_q_86")]
social_w3 <- as.data.frame(sapply(social_w3,as.numeric))
vars_to_change <- c("ysr_dependence_ach_q_11",
	"ysr_lonely_ach_q_12",
	"ysr_getalong_ach_q_25",
	"ysr_jealous_ach_q_27",
	"ysr_suspicious_ach_q_34",
	"ysr_accidents_ach_q_36",
	"ysr_bullyvictim_ach_q_38",
	"ysr_notliked_ach_q_48",
	"ysr_awkward_ach_q_69",
	"ysr_friends_ach_q_71",
	"ysr_speech_ach_q_86")
for (var in vars_to_change) {
social_w3[[var]] <- social_w3[[var]] %>%
	as.numeric() %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
social_w3$tot4 <- rowSums(social_w3)

#######################################################################

## THOUGHT PROBLEMS 

# WAVE 1

thought_w1 <- pheno1aq_youth[c("ysr_obsessive_ach_q_09",
	"ysr_selfharm_ach_q_18",
	"ysr_voices_ach_q_40",
	"ysr_tics_ach_q_46",
	"ysr_picking_ach_q_65",
	"ysr_repetitive_ach_q_73",
	"ysr_visions_ach_q_77",
	"ysr_sleep_ach_q_83",
	"ysr_hoarding_ach_q_90_v1",
	"ysr_strange_ach_q_91",
	"ysr_thoughts_ach_q_92",
	"ysr_sleep_ach_q_107")]
thought_w1 <- as.data.frame(sapply(thought_w1,as.numeric))
vars_to_change <- c("ysr_obsessive_ach_q_09",
	"ysr_selfharm_ach_q_18",
	"ysr_voices_ach_q_40",
	"ysr_tics_ach_q_46",
	"ysr_picking_ach_q_65",
	"ysr_repetitive_ach_q_73",
	"ysr_visions_ach_q_77",
	"ysr_sleep_ach_q_83",
	"ysr_hoarding_ach_q_90_v1",
	"ysr_strange_ach_q_91",
	"ysr_thoughts_ach_q_92",
	"ysr_sleep_ach_q_107")
for (var in vars_to_change) {
thought_w1[[var]] <- thought_w1[[var]] %>%
	as.numeric() %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
thought_w1$tot5 <- rowSums(thought_w1)

# WAVE 2

thought_w2 <- pheno2aq_youth[c("ysr_obsessive_ach_q_09",
	"ysr_selfharm_ach_q_18",
	"ysr_voices_ach_q_40",
	"ysr_tics_ach_q_46",
	"ysr_picking_ach_q_65",
	"ysr_repetitive_ach_q_73",
	"ysr_visions_ach_q_77",
	"ysr_sleep_ach_q_83",
	"ysr_hoarding_ach_q_90_v2",
	"ysr_strange_ach_q_91",
	"ysr_thoughts_ach_q_92",
	"ysr_sleep_ach_q_107")]
thought_w2 <- as.data.frame(sapply(thought_w2,as.numeric))
vars_to_change <- c("ysr_obsessive_ach_q_09",
	"ysr_selfharm_ach_q_18",
	"ysr_voices_ach_q_40",
	"ysr_tics_ach_q_46",
	"ysr_picking_ach_q_65",
	"ysr_repetitive_ach_q_73",
	"ysr_visions_ach_q_77",
	"ysr_sleep_ach_q_83",
	"ysr_hoarding_ach_q_90_v2",
	"ysr_strange_ach_q_91",
	"ysr_thoughts_ach_q_92",
	"ysr_sleep_ach_q_107")
for (var in vars_to_change) {
thought_w2[[var]] <- thought_w2[[var]] %>%
	as.numeric() %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
thought_w2$tot5 <- rowSums(thought_w2)

# WAVE 3

thought_w3 <- pheno3aq_youth[c("ysr_obsessive_ach_q_09",
	"ysr_selfharm_ach_q_18",
	"ysr_voices_ach_q_40",
	"ysr_tics_ach_q_46",
	"ysr_picking_ach_q_65",
	"ysr_repetitive_ach_q_73",
	"ysr_visions_ach_q_77",
	"ysr_sleep_ach_q_83",
	"ysr_hoarding_ach_q_90_v1",
	"ysr_strange_ach_q_91",
	"ysr_thoughts_ach_q_92",
	"ysr_sleep_ach_q_107")]
thought_w3 <- as.data.frame(sapply(thought_w3,as.numeric))
vars_to_change <- c("ysr_obsessive_ach_q_09",
	"ysr_selfharm_ach_q_18",
	"ysr_voices_ach_q_40",
	"ysr_tics_ach_q_46",
	"ysr_picking_ach_q_65",
	"ysr_repetitive_ach_q_73",
	"ysr_visions_ach_q_77",
	"ysr_sleep_ach_q_83",
	"ysr_hoarding_ach_q_90_v1",
	"ysr_strange_ach_q_91",
	"ysr_thoughts_ach_q_92",
	"ysr_sleep_ach_q_107")
for (var in vars_to_change) {
thought_w3[[var]] <- thought_w3[[var]] %>%
	as.numeric() %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
thought_w3$tot5 <- rowSums(thought_w3)

#######################################################################

## ATTENTION PROBLEMS

# WAVE 1

attention_w1 <- pheno1aq_youth[c("ysr_actingyoung_ach_q_01",
	"ysr_finishing_ach_q_04",
	"ysr_concentration_ach_q_08",
	"ysr_restless_ach_q_10",
	"ysr_confused_ach_q_13",
	"ysr_daydreams_ach_q_17",
	"ysr_thinking_ach_q_41",
	"ysr_schoolwork_ach_q_68",
	"ysr_distracted_ach_q_85")]
attention_w1 <- as.data.frame(sapply(attention_w1,as.numeric))
vars_to_change <- c("ysr_actingyoung_ach_q_01",
	"ysr_finishing_ach_q_04",
	"ysr_concentration_ach_q_08",
	"ysr_restless_ach_q_10",
	"ysr_confused_ach_q_13",
	"ysr_daydreams_ach_q_17",
	"ysr_thinking_ach_q_41",
	"ysr_schoolwork_ach_q_68",
	"ysr_distracted_ach_q_85")
for (var in vars_to_change) {
attention_w1[[var]] <- attention_w1[[var]] %>%
	as.numeric() %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
attention_w1$tot6 <- rowSums(attention_w1)

# WAVE 2

attention_w2 <- pheno2aq_youth[c("ysr_actingyoung_ach_q_01",
	"ysr_finishing_ach_q_04",
	"ysr_concentration_ach_q_08",
	"ysr_restless_ach_q_10",
	"ysr_confused_ach_q_13",
	"ysr_daydreams_ach_q_17",
	"ysr_thinking_ach_q_41",
	"ysr_schoolwork_ach_q_68",
	"ysr_distracted_ach_q_85")]
attention_w2 <- as.data.frame(sapply(attention_w2,as.numeric))
vars_to_change <- c("ysr_actingyoung_ach_q_01",
	"ysr_finishing_ach_q_04",
	"ysr_concentration_ach_q_08",
	"ysr_restless_ach_q_10",
	"ysr_confused_ach_q_13",
	"ysr_daydreams_ach_q_17",
	"ysr_thinking_ach_q_41",
	"ysr_schoolwork_ach_q_68",
	"ysr_distracted_ach_q_85")
for (var in vars_to_change) {
attention_w2[[var]] <- attention_w2[[var]] %>%
	as.numeric() %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
attention_w2$tot6 <- rowSums(attention_w2)

# WAVE 3

attention_w3 <- pheno3aq_youth[c("ysr_actingyoung_ach_q_01",
	"ysr_finishing_ach_q_04",
	"ysr_concentration_ach_q_08",
	"ysr_restless_ach_q_10",
	"ysr_confused_ach_q_13",
	"ysr_daydreams_ach_q_17",
	"ysr_thinking_ach_q_41",
	"ysr_schoolwork_ach_q_68",
	"ysr_distracted_ach_q_85")]
attention_w3 <- as.data.frame(sapply(attention_w3,as.numeric))
vars_to_change <- c("ysr_actingyoung_ach_q_01",
	"ysr_finishing_ach_q_04",
	"ysr_concentration_ach_q_08",
	"ysr_restless_ach_q_10",
	"ysr_confused_ach_q_13",
	"ysr_daydreams_ach_q_17",
	"ysr_thinking_ach_q_41",
	"ysr_schoolwork_ach_q_68",
	"ysr_distracted_ach_q_85")
for (var in vars_to_change) {
attention_w3[[var]] <- attention_w3[[var]] %>%
	as.numeric() %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
attention_w3$tot6 <- rowSums(attention_w3)

#######################################################################

## RULE BREAKING BEHAVIOR, I.E. DELINQUENT BEHAVIOR

# WAVE 1

delinquent_w1 <- pheno1aq_youth[c("ysr_alcohol_ach_q_02",
	"ysr_guilty_ach_q_26",
	"ysr_rulebreaking_ach_q_28",
	"ysr_trouble_ach_q_39",
	"ysr_lying_ach_q_43",
	"ysr_friends_ach_q_70",
	"ysr_runaway_ach_q_74",
	"ysr_pyromania_ach_q_79",
	"ysr_stealing_ach_q_88",
	"ysr_stealing_ach_q_89",
	"ysr_swearing_ach_q_97",
	"ysr_sexual_ach_q_103",
	"ysr_tobacco_ach_q_106",
	"ysr_truancy_ach_q_108",
	"ysr_drugs_ach_q_112")]
delinquent_w1 <- as.data.frame(sapply(delinquent_w1,as.numeric))
vars_to_change <- c("ysr_alcohol_ach_q_02",
	"ysr_guilty_ach_q_26",
	"ysr_rulebreaking_ach_q_28",
	"ysr_trouble_ach_q_39",
	"ysr_lying_ach_q_43",
	"ysr_friends_ach_q_70",
	"ysr_runaway_ach_q_74",
	"ysr_pyromania_ach_q_79",
	"ysr_stealing_ach_q_88",
	"ysr_stealing_ach_q_89",
	"ysr_swearing_ach_q_97",
	"ysr_sexual_ach_q_103",
	"ysr_tobacco_ach_q_106",
	"ysr_truancy_ach_q_108",
	"ysr_drugs_ach_q_112")
for (var in vars_to_change) {
delinquent_w1[[var]] <- delinquent_w1[[var]] %>%
	as.numeric() %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
delinquent_w1$tot7 <- rowSums(delinquent_w1)

# WAVE 2

delinquent_w2 <- pheno2aq_youth[c("ysr_alcohol_ach_q_02",
	"ysr_guilty_ach_q_26",
	"ysr_rulebreaking_ach_q_28",
	"ysr_trouble_ach_q_39",
	"ysr_lying_ach_q_43",
	"ysr_friends_ach_q_70",
	"ysr_runaway_ach_q_74",
	"ysr_pyromania_ach_q_79",
	"ysr_stealing_ach_q_88",
	"ysr_stealing_ach_q_89",
	"ysr_swearing_ach_q_97",
	"ysr_sexual_ach_q_103",
	"ysr_tobacco_ach_q_106",
	"ysr_truancy_ach_q_108",
	"ysr_drugs_ach_q_112")]
delinquent_w2 <- as.data.frame(sapply(delinquent_w2,as.numeric))
vars_to_change <- c("ysr_alcohol_ach_q_02",
	"ysr_guilty_ach_q_26",
	"ysr_rulebreaking_ach_q_28",
	"ysr_trouble_ach_q_39",
	"ysr_lying_ach_q_43",
	"ysr_friends_ach_q_70",
	"ysr_runaway_ach_q_74",
	"ysr_pyromania_ach_q_79",
	"ysr_stealing_ach_q_88",
	"ysr_stealing_ach_q_89",
	"ysr_swearing_ach_q_97",
	"ysr_sexual_ach_q_103",
	"ysr_tobacco_ach_q_106",
	"ysr_truancy_ach_q_108",
	"ysr_drugs_ach_q_112")
for (var in vars_to_change) {
delinquent_w2[[var]] <- delinquent_w2[[var]] %>%
	as.numeric() %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
delinquent_w2$tot7 <- rowSums(delinquent_w2)

# WAVE 3

delinquent_w3 <- pheno3aq_youth[c("ysr_alcohol_ach_q_02",
	"ysr_guilty_ach_q_26",
	"ysr_rulebreaking_ach_q_28",
	"ysr_trouble_ach_q_39",
	"ysr_lying_ach_q_43",
	"ysr_friends_ach_q_70",
	"ysr_runaway_ach_q_74",
	"ysr_pyromania_ach_q_79",
	"ysr_stealing_ach_q_88",
	"ysr_stealing_ach_q_89",
	"ysr_swearing_ach_q_97",
	"ysr_sexual_ach_q_103",
	"ysr_tobacco_ach_q_106",
	"ysr_truancy_ach_q_108",
	"ysr_drugs_ach_q_112")]
delinquent_w3 <- as.data.frame(sapply(delinquent_w3,as.numeric))
vars_to_change <- c("ysr_alcohol_ach_q_02",
	"ysr_guilty_ach_q_26",
	"ysr_rulebreaking_ach_q_28",
	"ysr_trouble_ach_q_39",
	"ysr_lying_ach_q_43",
	"ysr_friends_ach_q_70",
	"ysr_runaway_ach_q_74",
	"ysr_pyromania_ach_q_79",
	"ysr_stealing_ach_q_88",
	"ysr_stealing_ach_q_89",
	"ysr_swearing_ach_q_97",
	"ysr_sexual_ach_q_103",
	"ysr_tobacco_ach_q_106",
	"ysr_truancy_ach_q_108",
	"ysr_drugs_ach_q_112")
for (var in vars_to_change) {
delinquent_w3[[var]] <- delinquent_w3[[var]] %>%
	as.numeric() %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
delinquent_w3$tot7 <- rowSums(delinquent_w3)

#######################################################################

## AGGRESSIVE BEHAVIOR

# WAVE 1

aggressive_w1 <- pheno1aq_youth[c("ysr_quarrelsome_ach_q_03",
	"ysr_mean_ach_q_16",
	"ysr_attentionseeking_ach_q_19",
	"ysr_destructive_ach_q_20",
	"ysr_destructive_ach_q_21",
	"ysr_obedient_ach_q_22",
	"ysr_disobedient_ach_q_23",
	"ysr_fights_ach_q_37",
	"ysr_attacks_ach_q_64",
	"ysr_yelling_ach_q_75",
	"ysr_stubborn_ach_q_93",
	"ysr_moodswings_ach_q_94",
	"ysr_suspicious_ach_q_96",
	"ysr_bully_ach_q_101",
	"ysr_tantrums_ach_q_102",
	"ysr_threatening_ach_q_104",
	"ysr_boisterous_ach_q_111")]
aggressive_w1 <- as.data.frame(sapply(aggressive_w1,as.numeric))
vars_to_change <- c("ysr_quarrelsome_ach_q_03",
	"ysr_mean_ach_q_16",
	"ysr_attentionseeking_ach_q_19",
	"ysr_destructive_ach_q_20",
	"ysr_destructive_ach_q_21",
	"ysr_obedient_ach_q_22",
	"ysr_disobedient_ach_q_23",
	"ysr_fights_ach_q_37",
	"ysr_attacks_ach_q_64",
	"ysr_yelling_ach_q_75",
	"ysr_stubborn_ach_q_93",
	"ysr_moodswings_ach_q_94",
	"ysr_suspicious_ach_q_96",
	"ysr_bully_ach_q_101",
	"ysr_tantrums_ach_q_102",
	"ysr_threatening_ach_q_104",
	"ysr_boisterous_ach_q_111")
for (var in vars_to_change) {
aggressive_w1[[var]] <- aggressive_w1[[var]] %>%
	as.numeric() %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
aggressive_w1$tot8 <- rowSums(aggressive_w1)

# WAVE 2

aggressive_w2 <- pheno2aq_youth[c("ysr_quarrelsome_ach_q_03",
	"ysr_mean_ach_q_16",
	"ysr_attentionseeking_ach_q_19",
	"ysr_destructive_ach_q_20",
	"ysr_destructive_ach_q_21",
	"ysr_obedient_ach_q_22",
	"ysr_disobedient_ach_q_23",
	"ysr_fights_ach_q_37",
	"ysr_attacks_ach_q_64",
	"ysr_yelling_ach_q_75",
	"ysr_stubborn_ach_q_93",
	"ysr_moodswings_ach_q_94",
	"ysr_suspicious_ach_q_96",
	"ysr_bully_ach_q_101",
	"ysr_tantrums_ach_q_102",
	"ysr_threatening_ach_q_104",
	"ysr_boisterous_ach_q_111")]
aggressive_w2 <- as.data.frame(sapply(aggressive_w2,as.numeric))
vars_to_change <- c("ysr_quarrelsome_ach_q_03",
	"ysr_mean_ach_q_16",
	"ysr_attentionseeking_ach_q_19",
	"ysr_destructive_ach_q_20",
	"ysr_destructive_ach_q_21",
	"ysr_obedient_ach_q_22",
	"ysr_disobedient_ach_q_23",
	"ysr_fights_ach_q_37",
	"ysr_attacks_ach_q_64",
	"ysr_yelling_ach_q_75",
	"ysr_stubborn_ach_q_93",
	"ysr_moodswings_ach_q_94",
	"ysr_suspicious_ach_q_96",
	"ysr_bully_ach_q_101",
	"ysr_tantrums_ach_q_102",
	"ysr_threatening_ach_q_104",
	"ysr_boisterous_ach_q_111")
for (var in vars_to_change) {
aggressive_w2[[var]] <- aggressive_w2[[var]] %>%
	as.numeric() %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
aggressive_w2$tot8 <- rowSums(aggressive_w2)

# WAVE 3

aggressive_w3 <- pheno3aq_youth[c("ysr_quarrelsome_ach_q_03",
	"ysr_mean_ach_q_16",
	"ysr_attentionseeking_ach_q_19",
	"ysr_destructive_ach_q_20",
	"ysr_destructive_ach_q_21",
	"ysr_obedient_ach_q_22",
	"ysr_disobedient_ach_q_23",
	"ysr_fights_ach_q_37",
	"ysr_attacks_ach_q_64",
	"ysr_yelling_ach_q_75",
	"ysr_stubborn_ach_q_93",
	"ysr_moodswings_ach_q_94",
	"ysr_suspicious_ach_q_96",
	"ysr_bully_ach_q_101",
	"ysr_tantrums_ach_q_102",
	"ysr_threatening_ach_q_104",
	"ysr_boisterous_ach_q_111")]
aggressive_w3 <- as.data.frame(sapply(aggressive_w3,as.numeric))
vars_to_change <- c("ysr_quarrelsome_ach_q_03",
	"ysr_mean_ach_q_16",
	"ysr_attentionseeking_ach_q_19",
	"ysr_destructive_ach_q_20",
	"ysr_destructive_ach_q_21",
	"ysr_obedient_ach_q_22",
	"ysr_disobedient_ach_q_23",
	"ysr_fights_ach_q_37",
	"ysr_attacks_ach_q_64",
	"ysr_yelling_ach_q_75",
	"ysr_stubborn_ach_q_93",
	"ysr_moodswings_ach_q_94",
	"ysr_suspicious_ach_q_96",
	"ysr_bully_ach_q_101",
	"ysr_tantrums_ach_q_102",
	"ysr_threatening_ach_q_104",
	"ysr_boisterous_ach_q_111")
for (var in vars_to_change) {
aggressive_w3[[var]] <- aggressive_w3[[var]] %>%
	as.numeric() %>%
	recode(`1` = 0, `2` = 1, `3` = 2)
}	
aggressive_w3$tot8 <- rowSums(aggressive_w3)

#######################################################################

## COLLATE RESULTS

# WAVE 1

ysr_w1 <- as.data.frame(cbind(pheno1aq_youth$project_pseudo_id,
	withdrawn_w1$tot1,
	somatic_w1$tot2,
	anxious_w1$tot3,
	social_w1$tot4,
	thought_w1$tot5,
	attention_w1$tot6,
	delinquent_w1$tot7,
	aggressive_w1$tot8))
colnames(ysr_w1) <- c("project_pseudo_id",
	"withdrawn",
	"somatic",
	"anxious",
	"social",
	"thought",
	"attention",
	"delinquent",
	"aggressive")
		
# WAVE 2

ysr_w2 <- as.data.frame(cbind(pheno2aq_youth$project_pseudo_id,
	withdrawn_w2$tot1,
	somatic_w2$tot2,
	anxious_w2$tot3,
	social_w2$tot4,
	thought_w2$tot5,
	attention_w2$tot6,
	delinquent_w2$tot7,
	aggressive_w2$tot8))
colnames(ysr_w2) <- c("project_pseudo_id",
	"withdrawn",
	"somatic",
	"anxious",
	"social",
	"thought",
	"attention",
	"delinquent",
	"aggressive")

# WAVE 3

ysr_w3 <- as.data.frame(cbind(pheno3aq_youth$project_pseudo_id,
	withdrawn_w3$tot1,
	somatic_w3$tot2,
	anxious_w3$tot3,
	social_w3$tot4,
	thought_w3$tot5,
	attention_w3$tot6,
	delinquent_w3$tot7,
	aggressive_w3$tot8))
colnames(ysr_w3) <- c("project_pseudo_id",
	"withdrawn",
	"somatic",
	"anxious",
	"social",
	"thought",
	"attention",
	"delinquent",
	"aggressive")
	
print(nrow(ysr_w1)) #3953
length(unique(ysr_w1$project_pseudo_id)) 
print(nrow(ysr_w2)) #2621
length(unique(ysr_w2$project_pseudo_id)) 
print(nrow(ysr_w3)) #465
length(unique(ysr_w3$project_pseudo_id)) 

# Save into separate data frames
write.table(ysr_w1,"/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/OUTPUT/data/ysr_w1.csv",
	sep=",",row.names=FALSE,quote=FALSE)
write.table(ysr_w2,"/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/OUTPUT/data/ysr_w2.csv",
	sep=",",row.names=FALSE,quote=FALSE)
write.table(ysr_w3,"/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/OUTPUT/data/ysr_w3.csv",
	sep=",",row.names=FALSE,quote=FALSE)

#######################################################################	
	
# Check overlap between ids
common_ids_w1_w2 <- intersect(ysr_w1$project_pseudo_id, ysr_w2$project_pseudo_id)
print(length(common_ids_w1_w2)) #981
common_ids_w1_w3 <- intersect(ysr_w1$project_pseudo_id, ysr_w3$project_pseudo_id)
print(length(common_ids_w1_w3)) #0
common_ids_w2_w3 <- intersect(ysr_w1$project_pseudo_id, ysr_w3$project_pseudo_id)
print(length(common_ids_w2_w3)) #0

#######################################################################	

# END
