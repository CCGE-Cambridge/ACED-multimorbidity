##File name: discrimination_survival_subgroups.r
##Authors: Hannah Harrison
##Last Edit: 23/01/2023
##Description: compute Harrell's c-index/concordance, using survival analysis approach for binary outocmes and fixed years of data 
##analysis by age and sex subgroups
save_folder <- "~/ACED-RREDD-EHR/cam_mm_score_project/results/subgroups"

##set up subdf_EHR_cohort_maingroup dataframes
df_all <- df_EHR_cohort_main
df_men <- df_EHR_cohort_main %>% filter(sex == 1)
df_women <- df_EHR_cohort_main %>% filter(sex == 0)
df_forties <- df_EHR_cohort_main %>% filter(age_at_assessment >= 40 & age_at_assessment < 50)
df_fifties <- df_EHR_cohort_main %>% filter(age_at_assessment >= 50 & age_at_assessment < 60)
df_sixties <- df_EHR_cohort_main %>% filter(age_at_assessment >= 60 & age_at_assessment < 70)

df_forties_men <- df_EHR_cohort_main %>% filter(age_at_assessment >= 40 & age_at_assessment < 50 & sex == 1)
df_fifties_men <- df_EHR_cohort_main %>% filter(age_at_assessment >= 50 & age_at_assessment < 60 & sex == 1)
df_sixties_men <- df_EHR_cohort_main %>% filter(age_at_assessment >= 60 & age_at_assessment < 70 & sex == 1)

df_forties_women <- df_EHR_cohort_main %>% filter(age_at_assessment >= 40 & age_at_assessment < 50 & sex == 0)
df_fifties_women <- df_EHR_cohort_main %>% filter(age_at_assessment >= 50 & age_at_assessment < 60 & sex == 0)
df_sixties_women <- df_EHR_cohort_main %>% filter(age_at_assessment >= 60 & age_at_assessment < 70 & sex == 0)

list_df <- list(df_all, df_men, df_women, df_forties, df_fifties, df_sixties, df_forties_men, df_fifties_men, df_sixties_men, df_forties_women, df_fifties_women, df_sixties_women)
subgroups <- c("all", "men", "women", "forties", "fifties", "sixties", "forties_men", "fifties_men", "sixties_men", "forties_women", "fifties_women", "sixties_women")

models <- c("PI_mort_37", "PI_mort_20", "PI_hosp_37", "PI_hosp_20", "num_mm_37", "num_mm_20", "PI_cons_NB_37", "PI_cons_NB_20", "PI_gen_37")
death_date_max <- death_study_end
cancer_date_max <- cancer_study_end
gp_date_max <- gp_study_end
my_names <- c( "model", "years", "cases", "cohort", "mean_fu", "median_fu", "c_index", "L_CI", "U_CI")

#death by year (1-10 years)
for (d in 1:length(list_df)){
    df_concordance <- data.frame(matrix(ncol =6, nrow = 0))
    df_temp <- list_df[[d]]

    for (i in 1:10) {
        print(paste0("death: ", i))
        df_temp$max_end_date <- as.Date(df_temp$baseline_date) + years(i)
        df_temp <- df_temp %>% mutate(study_end_date = pmin(max_end_date, death_date, death_date_max, na.rm = TRUE))
        df_temp$censor_time <- as.Date(df_temp$study_end_date) - as.Date(df_temp$baseline_date)
        df_temp <- df_temp %>% mutate(status = case_when(death_date <= max_end_date ~ TRUE, TRUE ~ FALSE))
        obs_surv <- Surv(df_temp$censor_time, df_temp$status)

        sum_cases = sum(df_temp$status)
        sum_cohort = nrow(df_temp)
        mean_fu = mean(df_temp$censor_time)
        median_fu = median(df_temp$censor_time)

        for (j in 1:length(models)){   
            df_temp <- df_temp %>% mutate(model_col = !!as.name(models[j]))
            temp_concordance <- concordance(obs_surv ~ model_col, data = df_temp)
            my_concord <- 1-temp_concordance$concordance
            my_var <- temp_concordance$var

            df_concordance <- rbind(df_concordance, c(models[j], i, sum_cases, sum_cohort, mean_fu, median_fu, 
                                my_concord, my_concord -1.96*sqrt(my_var)/2, my_concord +1.96*sqrt(my_var)/2))

        }
    }
    colnames(df_concordance) <- my_names
    write.csv(df_concordance, paste0(save_folder, "/cstat_death_", subgroups[d], ".csv"), row.names=FALSE)
}

#cancer by year (1-10 years)

for (d in 1:length(list_df)){
    df_concordance <- data.frame(matrix(ncol =6, nrow = 0))
    df_temp <- list_df[[d]]
    for (i in 1:10) {
        print(paste0("cancer: ", i))
        df_temp$max_end_date <- as.Date(df_temp$baseline_date) + years(i)
        df_temp <- df_temp %>% mutate(study_end_date = pmin(max_end_date, death_date, first_cancer_date_after, cancer_date_max, na.rm = TRUE))
        df_temp$censor_time <- as.Date(df_temp$study_end_date) - as.Date(df_temp$baseline_date)
        df_temp <- df_temp %>% mutate(status = case_when(first_cancer_date_after <= max_end_date ~ TRUE, TRUE ~ FALSE))
        obs_surv <- Surv(df_temp$censor_time, df_temp$status)

        sum_cases = sum(df_temp$status)
        sum_cohort = nrow(df_temp)
        mean_fu = mean(df_temp$censor_time)
        median_fu = median(df_temp$censor_time)

        for (j in 1:length(models)){   
            df_temp <- df_temp %>% mutate(model_col = !!as.name(models[j]))
            temp_concordance <- concordance(obs_surv ~ model_col, data = df_temp)
            my_concord <- 1-temp_concordance$concordance
            my_var <- temp_concordance$var

            df_concordance <- rbind(df_concordance, c(models[j], i, sum_cases, sum_cohort, mean_fu, median_fu, 
                                my_concord, my_concord -1.96*sqrt(my_var)/2, my_concord +1.96*sqrt(my_var)/2))
       }
    }
    colnames(df_concordance) <- my_names
    write.csv(df_concordance, paste0(save_folder, "/cstat_cancer_", subgroups[d], ".csv"), row.names=FALSE)
}

###consultation rate, vary cohort by year
my_names2 <- c("model", "years", "cohort", "mean_rate", "median_rate", "c_index", "L_CI", "U_CI")

for (d in 1:length(list_df)){
    df_concordance <- data.frame(matrix(ncol =8, nrow = 0))
    df_temp <- list_df[[d]]
    df_temp <- df_temp %>% mutate(personal_end_date = pmin(death_date, gp_date_max, gp_end_date, na.rm = TRUE))

    for (i in 1:8) {
        print(paste0("consultatation rate: ", i))
        temp_num_var <- paste0("cons_in_", i, "_yrs_fu")
        #select members of cohort who have i years of follow-up
        df_temp <- df_temp %>% mutate(study_end_date = as.Date(df_temp$baseline_date) + years(i))
        df_temp <- df_temp %>% filter(personal_end_date >= study_end_date)
        #set outcome var to corresponding rate var
        df_temp <-  df_temp %>% mutate(outcome = as.numeric(!!as.name(temp_num_var))/i) %>% filter(!is.na(outcome))
        sum_cohort = nrow(df_temp)
   
        for (j in 1:length(models)){   
            df_temp <- df_temp %>% mutate(model_col = !!as.name(models[j]))
            temp_concordance <- concordance(outcome ~ model_col , data = df_temp)
            my_concord <- temp_concordance$concordance
            my_var <- temp_concordance$var

            df_concordance <- rbind(df_concordance, c(models[j], i, sum_cohort, mean(df_temp$outcome), median(df_temp$outcome), 
                         my_concord, my_concord -1.96*sqrt(my_var)/2, my_concord +1.96*sqrt(my_var)/2))

        }
    }
    colnames(df_concordance) <- my_names2
    write.csv(df_concordance, paste0(save_folder, "/cstat_rate_", subgroups[d], ".csv"), row.names=FALSE)
}

