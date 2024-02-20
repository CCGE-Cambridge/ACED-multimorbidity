##File name: discrimination_survival_v2.r
##Authors: Hannah Harrison
##Last Edit:26/04/2023
##Description: make KM plots for the three outcomes

death_date_max <- death_study_end  
cancer_date_max <- cancer_study_end
gp_date_max <- gp_study_end
save_folder <- graph_KM

##outcome of death - use whole cohort
df_cohort_death <- df_EHR_cohort_main %>% select(eid, death_date, baseline_date, case_death_any)
df_cohort_death <- df_cohort_death %>% mutate(follow_up_end = pmin(death_date_max, death_date, na.rm = TRUE))
df_cohort_death$censor_time <- as.Date(df_cohort_death$follow_up_end) - as.Date(df_cohort_death$baseline_date)
df_cohort_death <- df_cohort_death %>% mutate(status = case_when(case_death_any == 1 & death_date <= follow_up_end ~ TRUE, TRUE ~ FALSE))
#obs_surv <- Surv(df_cohort_death$censor_time/365.25, df_cohort_death$status)


survfit2(Surv(censor_time/365.25, status) ~ 1, data = df_cohort_death) %>% 
  ggsurvfit() +
  labs(x = "Years", y = "Overall Probability of Death") + 
  xlim(0, 10) +
  add_confidence_interval() +
  add_risktable()
ggsave(file.path(save_folder, "KM_death.png"),  dpi = 300, type = "cairo")


##outcome of cancer - use whole cohort
df_cohort_cancer <- df_EHR_cohort_main %>% select(eid, death_date, baseline_date, first_cancer_date_after, any_cancer_except_after)
df_cohort_cancer <- df_cohort_cancer %>% mutate(follow_up_end = pmin(cancer_date_max, death_date, first_cancer_date_after, na.rm = TRUE))
df_cohort_cancer$censor_time <- as.Date(df_cohort_cancer$follow_up_end) - as.Date(df_cohort_cancer$baseline_date)
df_cohort_cancer <- df_cohort_cancer %>% mutate(status = case_when(any_cancer_except_after == 1 & (first_cancer_date_after <= follow_up_end)  ~ TRUE, TRUE ~ FALSE))
obs_surv <- Surv(df_cohort_cancer$censor_time, df_cohort_cancer$status)

survfit2(Surv(censor_time/365.25, status) ~ 1, data = df_cohort_cancer) %>% 
  ggsurvfit() +
  labs(x = "Years", y = "Overall Probability of Cancer") + 
   xlim(0, 10) +
  add_confidence_interval() +
  add_risktable()
ggsave(file.path(save_folder, "KM_cancer.png"),  dpi = 300, type = "cairo")



