##File name: Tables_1_and_2.R
##Authors: Hannah Harrison
##Last Edit: 21/11/2022
##Description: summary statistics for cohort and for outcomes

#0. set-up for tables
save_folder <- res_t1

#merge from ukb baseline main to get smoking status and ethnicity
all_baseline_fields <- fread(fpath_baseline_vars)
extra_fields <- all_baseline_fields %>% filter(type == "smoking" | type == "ethnicity" | type == "deprivation")
df_baseline_extra <- fread(fpath_baseline, select = c("eid", extra_fields[c(1, 2, 3, 28)]$field))
colnames(df_baseline_extra) <- c("eid", "TDS", "ethnicity", "smoking_status", "English_IMD")
df_baseline_extra <- smoking_stat_clean(df_baseline_extra) 
df_baseline_extra <- ethnicity_clean(df_baseline_extra)
df_baseline_extra <- df_baseline_extra %>% mutate(IMD_quints = cut(English_IMD, breaks = c(0, 8.254352, 13.59066, 21.1599, 33.95404, 100)))

#code to merge with Xin's data (to get BMI)
df_baseline_xin <- fread(fpath_xin, select = c("f.eid", "f.21001.0.0"))
colnames(df_baseline_xin) <- c("eid_xin", "bmi")
key <- fread(fpath_key_fam) %>% dplyr::rename(eid_xin=Cambridge_ID, eid=UCL_ID)
df_baseline_xin <- merge(x = df_baseline_xin, y = key, by = "eid_xin", all.x = TRUE) %>% select(!c("eid_xin"))
df_baseline_extra <- merge(x = df_baseline_extra, y = df_baseline_xin, by = "eid", all.x = TRUE)

df_summary_stats <- merge(x = df_EHR_cohort_main, y = df_baseline_extra, by = "eid", all.x = TRUE)

##1. full cohort 
df_summary_stats <- df_summary_stats %>% mutate(bmi_cats = case_when(bmi < 20 ~ 0, bmi >= 20 & bmi < 25 ~ 1, bmi >= 25 & bmi < 30 ~ 2, bmi > 30 ~ 3, TRUE ~ NA_real_))
df_summary_stats <- df_summary_stats %>% mutate(cons_rate = cons_all/gp_follow_up_length)
df_summary_stats <- df_summary_stats %>% mutate(zero_cons = case_when(cons_all == 0 ~ 1, TRUE ~ 0))

##count stats
df_count_stats <- df_summary_stats %>% select(!c("event_dt")) %>% select(starts_with(c("sex", "num_mm_37", "num_mm_20", "group_A", "event", "case_death", "any_cancer_except", "cons_in_", "zero_")))
df_count_stats <- df_count_stats %>% mutate(at_least_one_mm = case_when(num_mm_37 > 0 ~ 1, TRUE ~ 0))
table_counts <- df_count_stats %>% summarise(across(where(is.numeric), list(sum = ~sum(.x), num = ~n(), min = ~min(.x), max = ~max(.x)))) %>%
                  pivot_longer(everything(), names_to = c("variable",".value"), names_pattern = "(.+)_(.+)") %>% 
                    mutate(percent = (sum/num)*100) #%>% print(n=50)
                    
##countinuous stats
df_continuous_stats <- df_summary_stats %>% select(starts_with(c("age", "bmi", "num_mm_37", "num_mm_20", "gp_follow_up_length", "cancer_follow_up_length", "cons_rate" )))
table_continuous <- df_continuous_stats %>% summarise(across(where(is.numeric), 
                    list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE), median = ~median(.x, na.rm = TRUE), 
                    Q1 = ~quantile(.x, probs = 0.25, na.rm = TRUE), Q3 = ~quantile(.x, probs = 0.75, na.rm = TRUE),  
                    missing = ~sum(is.na(.x)), num = ~n(), min = ~min(.x, na.rm = TRUE), max = ~max(.x, na.rm = TRUE)))) %>%
                  pivot_longer(everything(), names_to = c("variable",".value"), names_pattern = "(.+)_(.+)")  %>% 
                  mutate(percent_missing = (missing/num)*100) #%>% print(n=10)

#categorial stats
table_smokes <- df_summary_stats  %>% count(smoking_labels) %>% mutate(tot = sum(n)) %>% mutate(percent  = ((n/tot) *100))
table_ethnicity <- df_summary_stats  %>% count(ethnicity_labels) %>% mutate(tot = sum(n)) %>% mutate(percent  = ((n/tot) *100))
table_bmi <- df_summary_stats  %>% count(bmi_cats) %>% mutate(tot = sum(n)) %>% mutate(percent  = ((n/tot) *100))
table_imd <- df_summary_stats  %>% count(IMD_quints) %>% mutate(tot = sum(n)) %>% mutate(percent  = ((n/tot) *100))

#print all to file
write.csv(table_counts, file.path(save_folder, "tables_count_variables.csv"), row.names=FALSE)
write.csv(table_continuous, file.path(save_folder, "tables_continuous_variables.csv"), row.names=FALSE)
write.csv(table_smokes, file.path(save_folder, "tables_smoking_variables.csv"), row.names=FALSE)
write.csv(table_ethnicity, file.path(save_folder, "tables_ethnicity_variables.csv"), row.names=FALSE)
write.csv(table_bmi, file.path(save_folder, "tables_bmi_variables.csv"), row.names=FALSE)
write.csv(table_imd, file.path(save_folder, "tables_imd_variables.csv"), row.names=FALSE)

##2. group by presence of at least one comorbidity
df_summary_stats <- df_summary_stats %>% mutate(at_least_one = case_when(num_mm_37 > 0 ~ 1, TRUE ~0))

df_summary_stats_grouped <- df_summary_stats %>% group_by(at_least_one)

##count stats, grouped
df_count_stats <- df_summary_stats_grouped %>% select(!c("event_dt")) %>% select(starts_with(c("sex", "num_mm_37", "num_mm_20", "group_A", "case_death", "any_cancer_except", "cons_in", "zero_")))
table_counts <- df_count_stats %>% summarise(across(where(is.numeric), list(sum = ~sum(.x), num = ~n(), min = ~min(.x), max = ~max(.x)))) %>%
                  pivot_longer(everything(), names_to = c("variable",".value"), names_pattern = "(.+)_(.+)") %>% 
                    mutate(percent = (sum/num)*100) #%>% print(n=50)
                    
##countinuous stats, grouped
df_continuous_stats <- df_summary_stats_grouped %>% select(starts_with(c("age", "bmi", "num_mm_37", "num_mm_20", "gp_follow_up_length", "cancer_follow_up_length", "cons_rate" )))
table_continuous <- df_continuous_stats %>% summarise(across(where(is.numeric), 
                    list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE), median = ~median(.x, na.rm = TRUE), 
                    Q1 = ~quantile(.x, probs = 0.25, na.rm = TRUE), Q3 = ~quantile(.x, probs = 0.75, na.rm = TRUE),  
                    missing = ~sum(is.na(.x)), num = ~n(), min = ~min(.x, na.rm = TRUE), max = ~max(.x, na.rm = TRUE)))) %>%
                  pivot_longer(everything(), names_to = c("variable",".value"), names_pattern = "(.+)_(.+)")  %>% 
                  mutate(percent_missing = (missing/num)*100) #%>% print(n=20)

#categorial stats, grouped
table_smokes <- df_summary_stats_grouped  %>% count(smoking_labels) %>% mutate(tot = sum(n)) %>% mutate(percent  = ((n/tot) *100))
table_ethnicity <- df_summary_stats_grouped  %>% count(ethnicity_labels) %>% mutate(tot = sum(n)) %>% mutate(percent  = ((n/tot) *100))
table_bmi <- df_summary_stats_grouped  %>% count(bmi_cats) %>% mutate(tot = sum(n)) %>% mutate(percent  = ((n/tot) *100))
table_imd <- df_summary_stats_grouped  %>% count(IMD_quints) %>% mutate(tot = sum(n)) %>% mutate(percent  = ((n/tot) *100))


write.csv(table_counts, file.path(save_folder, "group_tables_count_variables.csv"), row.names=FALSE)
write.csv(table_continuous, file.path(save_folder, "group_tables_continuous_variables.csv"), row.names=FALSE)
write.csv(table_smokes, file.path(save_folder, "group_tables_smoking_variables.csv"), row.names=FALSE)
write.csv(table_ethnicity, file.path(save_folder, "group_tables_ethnicity_variables.csv"), row.names=FALSE)
write.csv(table_bmi, file.path(save_folder, "group_tables_bmi_variables.csv"), row.names=FALSE)
write.csv(table_imd, file.path(save_folder, "group_tables_imd_variables.csv"), row.names=FALSE)

##3. group by none, one or two+ conditions
df_summary_stats <- df_summary_stats %>% mutate(three_groups = case_when(num_mm_37 == 0 ~ 0, num_mm_37 == 1 ~ 1, num_mm_37 > 1 ~ 2))
df_summary_stats_3_groups <- df_summary_stats %>% group_by(three_groups)

df_summary_stats <- df_summary_stats %>% mutate(two_groups = case_when(num_mm_37 == 0 ~ 0, num_mm_37 > 0 ~ 1))
df_summary_stats_2_groups <- df_summary_stats %>% group_by(two_groups)

##count stats, grouped
df_count_stats <- df_summary_stats_3_groups %>% select(!c("event_dt")) %>% select(starts_with(c("sex", "num_mm_37", "num_mm_20", "group_A", "case_death", "any_cancer_except", "cons_in_", "zero_")))
table_counts <- df_count_stats %>% summarise(across(where(is.numeric), list(sum = ~sum(.x), num = ~n(), min = ~min(.x), max = ~max(.x)))) %>%
                  pivot_longer(everything(), names_to = c("variable",".value"), names_pattern = "(.+)_(.+)") %>% 
                    mutate(percent = (sum/num)*100) #%>% print(n=50)
                    
##countinuous stats, grouped
df_continuous_stats <- df_summary_stats_3_groups %>% select(starts_with(c("age", "bmi", "num_mm_37", "num_mm_20", "gp_follow_up_length", "cancer_follow_up_length", "cons_rate" )))
table_continuous <- df_continuous_stats %>% summarise(across(where(is.numeric), 
                    list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE), median = ~median(.x, na.rm = TRUE), 
                    Q1 = ~quantile(.x, probs = 0.25, na.rm = TRUE), Q3 = ~quantile(.x, probs = 0.75, na.rm = TRUE),  
                    missing = ~sum(is.na(.x)), num = ~n(), min = ~min(.x, na.rm = TRUE), max = ~max(.x, na.rm = TRUE)))) %>%
                  pivot_longer(everything(), names_to = c("variable",".value"), names_pattern = "(.+)_(.+)")  %>% 
                  mutate(percent_missing = (missing/num)*100) #%>% print(n=20)

df_continuous_stats2 <- df_summary_stats_2_groups %>% select(starts_with(c("age", "bmi", "num_mm_37", "num_mm_20", "gp_follow_up_length", "cancer_follow_up_length", "cons_rate" )))
table_continuous2 <- df_continuous_stats2 %>% summarise(across(where(is.numeric), 
                    list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE), median = ~median(.x, na.rm = TRUE), 
                    Q1 = ~quantile(.x, probs = 0.25, na.rm = TRUE), Q3 = ~quantile(.x, probs = 0.75, na.rm = TRUE),  
                    missing = ~sum(is.na(.x)), num = ~n(), min = ~min(.x, na.rm = TRUE), max = ~max(.x, na.rm = TRUE)))) %>%
                  pivot_longer(everything(), names_to = c("variable",".value"), names_pattern = "(.+)_(.+)")  %>% 
                  mutate(percent_missing = (missing/num)*100) #%>% print(n=20)

#categorial stats, grouped
table_smokes <- df_summary_stats_3_groups  %>% count(smoking_labels) %>% mutate(tot = sum(n)) %>% mutate(percent  = ((n/tot) *100))
table_ethnicity <- df_summary_stats_3_groups  %>% count(ethnicity_labels) %>% mutate(tot = sum(n)) %>% mutate(percent  = ((n/tot) *100))
table_bmi <- df_summary_stats_3_groups  %>% count(bmi_cats) %>% mutate(tot = sum(n)) %>% mutate(percent  = ((n/tot) *100))
table_imd <- df_summary_stats_3_groups  %>% count(IMD_quints) %>% mutate(tot = sum(n)) %>% mutate(percent  = ((n/tot) *100))


write.csv(table_counts, file.path(save_folder, "group3_tables_count_variables.csv"), row.names=FALSE)
write.csv(table_continuous,file.path(save_folder, "group3_tables_continuous_variables.csv"), row.names=FALSE)
write.csv(table_smokes, file.path(save_folder, "group3_tables_count_smoking.csv"), row.names=FALSE)
write.csv(table_ethnicity, file.path(save_folder, "group3_tables_ethnicity_variables.csv"), row.names=FALSE)
write.csv(table_bmi, file.path(save_folder, "group3_tables_bmi_variables.csv"), row.names=FALSE)
write.csv(table_imd, file.path(save_folder, "group3_tables_imd_variables.csv"), row.names=FALSE)

