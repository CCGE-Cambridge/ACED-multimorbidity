##File name: main.r
##Author: Hannah Harrison
##Last Edit: 24/08/2023
##Description:full analysis for mm_score project - tidied up as complete project

##0. CONFIG (libraries, paths and functions)
source("~/ACED-multimorbidity/0_config.R") #adds libraries, sets paths and loads functions

##1. COHORT DEFINITION
source("~/ACED-multimorbidity/1_define_EHR_cohort.R") #combines all infromation into one cohort

##2. OUTCOME DEFINITION, WITH RESPECT TO INDEX DATE (all done for baseline date)
#i. death (get date for censoring/survival analysis date) 
source("~/ACED-multimorbidity/2_get_deaths.R") 
df_EHR_cohort_main <-  left_join(x = df_EHR_cohort_main, y = df_baseline_death %>% select(!c("baseline_date")) , by = "eid")
df_EHR_cohort_1yr <-  left_join(x = df_EHR_cohort_1yr, y = df_baseline_death %>% select(!c("baseline_date")) , by = "eid")
df_EHR_cohort_5yr <-  left_join(x = df_EHR_cohort_5yr, y = df_baseline_death %>% select(!c("baseline_date")) , by = "eid")
df_EHR_cohort_lb_only <-  left_join(x = df_EHR_cohort_lb_only, y = df_baseline_death %>% select(!c("baseline_date")) , by = "eid")

sum(df_EHR_cohort_lb_only$case_death_1yr) #254
sum(df_EHR_cohort_lb_only$case_death_5yr) #2120

sum(df_EHR_cohort_main$case_death_1yr) #161
sum(df_EHR_cohort_main$case_death_5yr) #1987

#ii. cancer (use cancer registry data) 
source("~/ACED-multimorbidity/2_get_cancers.R") 
df_EHR_cohort_main <-  left_join(x = df_EHR_cohort_main, y = df_baseline_cancer %>% select(!c("baseline_date")), by = "eid") 
df_EHR_cohort_1yr <-  left_join(x = df_EHR_cohort_1yr, y = df_baseline_cancer %>% select(!c("baseline_date")), by = "eid") 
df_EHR_cohort_5yr <-  left_join(x = df_EHR_cohort_5yr, y = df_baseline_cancer %>% select(!c("baseline_date")), by = "eid") 
df_EHR_cohort_lb_only <-  left_join(x = df_EHR_cohort_lb_only, y = df_baseline_cancer %>% select(!c("baseline_date")), by = "eid") 

sum(df_EHR_cohort_lb_only$any_cancer_except_1yr) #1094
sum(df_EHR_cohort_lb_only$any_cancer_except_5yr) #5623

sum(df_EHR_cohort_main$any_cancer_except_1yr) #1071
sum(df_EHR_cohort_main$any_cancer_except_5yr) #5589

#iii. gp consultations (rate in 1 yr/rate in 5 yr) - yes, need to make unique by eid and date
##note, these cohosrts are a slightly different size: have gp lookback (1year) condition and 6 months follow-up 
source("~/ACED-multimorbidity/2_get_gp_consult_rate.R") 

df_EHR_cohort_main  <- df_EHR_cohort_main  %>% mutate(gp_follow_up_length = interval(baseline_date, gp_end_date) / years(1))#follow-up time as numeric value in years


df_EHR_cohort_main <-  left_join(x = df_EHR_cohort_main, y = df_gp_main, by = "eid") 
df_EHR_cohort_1yr <-  left_join(x = df_EHR_cohort_1yr, y = df_gp_1yr, by = "eid") %>% 
                        mutate(n = case_when(is.na(n) ~ 0, TRUE ~ as.numeric(n)), rate = n)
df_EHR_cohort_5yr <-  left_join(x = df_EHR_cohort_5yr, y = df_gp_5yr, by = "eid") %>%
                     mutate(n = case_when(is.na(n) ~ 0, TRUE ~ as.numeric(n)), rate = n/5)


##3. APPLY MODELS AT INDEX DATE
##get cambridge multi-morbidity scores, inc residuals and number of conditions
##get charlson score, inc number of conditions - need to develop, start with codelists (bnf10 only - TPP data)
#df_EHR_cohort_main <- df_EHR_cohort_main %>% mutate(baseline_date = baseline_date.x)
source("~/ACED-multimorbidity/3_run_mm_scores.R") 
df_EHR_cohort_main <-  left_join(x = df_EHR_cohort_main, y = df_mm_all %>% select(!c("sex")), by = "eid") 
df_EHR_cohort_main <- df_EHR_cohort_main %>% select(!starts_with("RF_"))#residual fit not used in this analysis
#save full cohort for analysis here  
#save(df_EHR_cohort_main, file = file.path(fpath_save_data, "EHR_cohort_main_for_analysis.Rdata"))#used for main analysis - has 1 year lb and at least 6months follow-up
#load(file.path(fpath_save_data, "EHR_cohort_main_for_analysis.Rdata"))#used for main analysis - has 1 year lb and at least 6months follow-up

##4. Tables about cohort, outcome and condition prevalence
source("~/ACED-multimorbidity/4_tables_1_and_2.R") 

##5. DISCRIMINATION 
source("~/ACED-multimorbidity/5_discrimination_survival.R") #survival anlysis with censoring, calculates Harell's c-index (for cancer and death and consultation...?)
source("~/ACED-multimorbidity/5_discrimination_survival_subgroups.R") #by age, sex and age&sex
source("~/ACED-multimorbidity/5_discrimination_survival_SA_cancer.R") #SA removing people with cancer pre-baseline
source("~/ACED-multimorbidity/5_plot_cindex_over_time.R")
source("~/ACED-multimorbidity/5_plot_cindex_over_time_subgroups.R")

#6. measuring calibration (plots by decile)
source("~/ACED-multimorbidity/6_calibration_plots.R")

#7. KM plots
source("~/ACED-multimorbidity/7_KM_plots.R")
