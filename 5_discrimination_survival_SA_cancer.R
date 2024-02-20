##File name: discrimination_survival_SA_cancer.r
##Authors: Hannah Harrison
##Last Edit: 06/12/2023
##Description: SA for cancer outcome, removing pre-baseline cancers

save_folder <- res_cstats 
models <- c("PI_mort_37", "PI_mort_20", "PI_hosp_37", "PI_hosp_20", "num_mm_37", "num_mm_20", "PI_cons_NB_37", "PI_cons_NB_20", "PI_gen_37")

death_date_max <- death_study_end  
cancer_date_max <- cancer_study_end
gp_date_max <- gp_study_end

df_concordance <- data.frame(matrix(ncol =6, nrow = 0))
my_names <- c( "model", "years", "cases", "cohort", "mean_fu", "median_fu", "c_index", "L_CI", "U_CI")

df_EHR_cohort_SA <- df_EHR_cohort_main %>% filter(pre_baseline_cancer ==0) #there are 8435 people with cancer before baseline (n=103,463)

#2. cancer by year (1-5 years)
df_concordance <- data.frame(matrix(ncol =6, nrow = 0))

for (i in 1:10) {
print(paste0("cancer: ", i))
    df_EHR_cohort_SA$max_end_date <- as.Date(df_EHR_cohort_SA$baseline_date) + years(i)
    df_EHR_cohort_SA <- df_EHR_cohort_SA %>% mutate(study_end_date = pmin(max_end_date, death_date, first_cancer_date_after, cancer_date_max, na.rm = TRUE))
    df_EHR_cohort_SA$censor_time <- as.Date(df_EHR_cohort_SA$study_end_date) - as.Date(df_EHR_cohort_SA$baseline_date)
    df_EHR_cohort_SA <- df_EHR_cohort_SA %>% mutate(status = case_when(first_cancer_date_after <= max_end_date ~ TRUE, TRUE ~ FALSE))
    obs_surv <- Surv(df_EHR_cohort_SA$censor_time, df_EHR_cohort_SA$status)

    sum_cases = sum(df_EHR_cohort_SA$status)
    sum_cohort = nrow(df_EHR_cohort_SA)
    mean_fu = mean(df_EHR_cohort_SA$censor_time)
    median_fu = median(df_EHR_cohort_SA$censor_time)

    for (j in 1:length(models)) 
    {   
        df_EHR_cohort_SA <- df_EHR_cohort_SA %>% mutate(model_col = !!as.name(models[j]))
        temp_concordance <- concordance(obs_surv ~ model_col, data = df_EHR_cohort_SA)
        my_concord <- 1-temp_concordance$concordance
        my_var <- temp_concordance$var

        df_concordance <- rbind(df_concordance, c(models[j], i, sum_cases, sum_cohort, mean_fu, median_fu, 
                                my_concord, my_concord -1.96*sqrt(my_var)/2, my_concord +1.96*sqrt(my_var)/2))

    }
}
colnames(df_concordance) <- my_names
write.csv(df_concordance, file.path(save_folder, "cstat_SA_cancer.csv"), row.names=FALSE)




