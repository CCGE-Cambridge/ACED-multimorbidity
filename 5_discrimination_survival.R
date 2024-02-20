##File name: discrimination_survival_v2.r
##Authors: Hannah Harrison
##Last Edit: 23/01/2023
##Description: compute Harrell's c-index/concordance, using survival analysis approach for binary outocmes and fixed years of data for continuous outcomes

save_folder <- "~/ACED-RREDD-EHR/cam_mm_score_project/results/c_stats"
models <- c("PI_mort_37", "PI_mort_20", "PI_hosp_37", "PI_hosp_20", "num_mm_37", "num_mm_20", "PI_cons_NB_37", "PI_cons_NB_20", "PI_gen_37")

death_date_max <- death_study_end  
cancer_date_max <- cancer_study_end
gp_date_max <- gp_study_end

df_concordance <- data.frame(matrix(ncol =6, nrow = 0))
my_names <- c( "model", "years", "cases", "cohort", "mean_fu", "median_fu", "c_index", "L_CI", "U_CI")

#1. death by year (1-10 years)
for (i in 1:10) {
    print(paste0("death: ", i))
    df_EHR_cohort_main$max_end_date <- as.Date(df_EHR_cohort_main$baseline_date) + years(i)
    df_EHR_cohort_main <- df_EHR_cohort_main %>% mutate(study_end_date = pmin(max_end_date, death_date, death_date_max, na.rm = TRUE))
    df_EHR_cohort_main$censor_time <- as.Date(df_EHR_cohort_main$study_end_date) - as.Date(df_EHR_cohort_main$baseline_date)
    df_EHR_cohort_main <- df_EHR_cohort_main %>% mutate(status = case_when(death_date <= max_end_date ~ TRUE, TRUE ~ FALSE))
    obs_surv <- Surv(df_EHR_cohort_main$censor_time, df_EHR_cohort_main$status)

    sum_cases = sum(df_EHR_cohort_main$status)
    sum_cohort = nrow(df_EHR_cohort_main)   
    mean_fu = mean(df_EHR_cohort_main$censor_time)
    median_fu = median(df_EHR_cohort_main$censor_time)

    for (j in 1:length(models)) 
    {   
        df_EHR_cohort_main <- df_EHR_cohort_main %>% mutate(model_col = !!as.name(models[j]))
        temp_concordance <- concordance(obs_surv ~ model_col, data = df_EHR_cohort_main)
        my_concord <- 1-temp_concordance$concordance
        my_var <- temp_concordance$var

        df_concordance <- rbind(df_concordance, c(models[j], i, sum_cases, sum_cohort, mean_fu, median_fu, 
                                my_concord, my_concord -1.96*sqrt(my_var)/2, my_concord +1.96*sqrt(my_var)/2))

    }
}

colnames(df_concordance) <- my_names
write.csv(df_concordance, file.path(save_folder, "cstat_death.csv"), row.names=FALSE)

#2. cancer by year (1-5 years)
df_concordance <- data.frame(matrix(ncol =6, nrow = 0))

for (i in 1:10) {
print(paste0("cancer: ", i))
    df_EHR_cohort_main$max_end_date <- as.Date(df_EHR_cohort_main$baseline_date) + years(i)
    df_EHR_cohort_main <- df_EHR_cohort_main %>% mutate(study_end_date = pmin(max_end_date, death_date, first_cancer_date_after, cancer_date_max, na.rm = TRUE))
    df_EHR_cohort_main$censor_time <- as.Date(df_EHR_cohort_main$study_end_date) - as.Date(df_EHR_cohort_main$baseline_date)
    df_EHR_cohort_main <- df_EHR_cohort_main %>% mutate(status = case_when(first_cancer_date_after <= max_end_date ~ TRUE, TRUE ~ FALSE))
    obs_surv <- Surv(df_EHR_cohort_main$censor_time, df_EHR_cohort_main$status)

    sum_cases = sum(df_EHR_cohort_main$status)
    sum_cohort = nrow(df_EHR_cohort_main)
    mean_fu = mean(df_EHR_cohort_main$censor_time)
    median_fu = median(df_EHR_cohort_main$censor_time)

    for (j in 1:length(models)) 
    {   
        df_EHR_cohort_main <- df_EHR_cohort_main %>% mutate(model_col = !!as.name(models[j]))
        temp_concordance <- concordance(obs_surv ~ model_col, data = df_EHR_cohort_main)
        my_concord <- 1-temp_concordance$concordance
        my_var <- temp_concordance$var

        df_concordance <- rbind(df_concordance, c(models[j], i, sum_cases, sum_cohort, mean_fu, median_fu, 
                                my_concord, my_concord -1.96*sqrt(my_var)/2, my_concord +1.96*sqrt(my_var)/2))

    }
}
colnames(df_concordance) <- my_names
write.csv(df_concordance, file.path(save_folder, "cstat_cancer.csv"), row.names=FALSE)

##3. consultation rate, vary cohort by year
df_concordance <- data.frame(matrix(ncol = 8, nrow = 0))
df_EHR_cohort_main <- df_EHR_cohort_main %>% mutate(personal_end_date = pmin(death_date, gp_date_max, gp_end_date, na.rm = TRUE))

for (i in 1:8) {
print(paste0("consultatation rate: ", i))
    temp_num_var <- paste0("cons_in_", i, "_yrs_fu")
    #select members of cohort who have i years of follow-up
    df_EHR_cohort_main <- df_EHR_cohort_main %>% mutate(study_end_date = as.Date(df_EHR_cohort_main$baseline_date) + years(i))
    df_EHR_cohort_temp <- df_EHR_cohort_main %>% filter(personal_end_date >= study_end_date)
    #set outcome var to corresponding rate var
    df_EHR_cohort_temp <-  df_EHR_cohort_temp %>% mutate(outcome = as.numeric(!!as.name(temp_num_var))/i) %>% filter(!is.na(outcome))
    sum_cohort = nrow(df_EHR_cohort_temp)
    
    for (j in 1:length(models)) 
    {   
        df_EHR_cohort_temp <- df_EHR_cohort_temp %>% mutate(model_col = !!as.name(models[j]))
        temp_concordance <- concordance(outcome ~ model_col , data = df_EHR_cohort_temp)
        my_concord <- temp_concordance$concordance
        my_var <- temp_concordance$var

        df_concordance <- rbind(df_concordance, c(models[j], i, sum_cohort, mean(df_EHR_cohort_temp$outcome), median(df_EHR_cohort_temp$outcome), 
                         my_concord, my_concord -1.96*sqrt(my_var)/2, my_concord +1.96*sqrt(my_var)/2))

    }
}

my_names2 <- c("model", "years", "cohort", "mean_rate", "median_rate", "c_index", "L_CI", "U_CI")
colnames(df_concordance) <- my_names2
write.csv(df_concordance, file.path(save_folder, "cstat_rate.csv"), row.names=FALSE)


