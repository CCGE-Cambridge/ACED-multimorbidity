##File name: calibration_plots.r
##Authors: Hannah Harrison
##Last Edit: 17/04/2023
##Description: calculate relative risk deciles and plot graphically (calibration assessment) for each model
save_folder <- graph_calib

model_list <- c("PI_mort_37",  "PI_mort_20",  "PI_hosp_37", "PI_hosp_20", "num_mm_37", "num_mm_20", "PI_gen_37") 
idx_ref_decile <- c(5, 5, 5, 5, 8, 8, 8) 
size_list <- c(3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2)        
outcome_list <- c("case_death_1yr", "case_death_5yr", "any_cancer_except_1yr", "any_cancer_except_5yr", "cons_in_1_yrs_fu", "cons_in_5_yrs_fu")


for (i in 1:length(model_list)){
       
        calib_score <- df_EHR_cohort_main %>% 
                                select(-starts_with("event")) %>% 
                                mutate(score_deciles = ntile(!!as.name(model_list[i]), 10))
        
        for (j in 1:length(outcome_list)) {
               
                calib_score_outcome <- calib_score %>% 
                                        group_by(score_deciles) %>%
                                        summarise(num = n(), 
                                                mean_expect = mean(!!as.name(model_list[i])),
                                                sd_expect = sd(!!as.name(model_list[i])), 
                                                mean_obs = mean(!!as.name(outcome_list[j]), na.rm = TRUE), 
                                                sd_obs = sd(!!as.name(outcome_list[j]),  na.rm = TRUE))

                calib_score_outcome  <- calib_score_outcome %>% 
                                        mutate(d6_expect = mean_expect[idx_ref_decile[i]], 
                                                d6_obs = mean_obs[idx_ref_decile[i]], 
                                                RR_mean_expect = mean_expect/d6_expect, 
                                                RR_mean_obs = mean_obs/d6_obs, 
                                                RR_UCI_obs = (RR_mean_obs + (1.96*(sd_obs/sqrt(num))/d6_obs)), 
                                                RR_LCI_obs = (RR_mean_obs - (1.96*(sd_obs/sqrt(num))/d6_obs)))

                calib_score_outcome %>% ggplot(aes(x=RR_mean_expect, y=RR_mean_obs)) + 
                                        geom_errorbar(aes(ymin=RR_LCI_obs, ymax=RR_UCI_obs),  width = .2) +
                                        geom_point(size=2) +
                                        geom_segment(aes(x = 0, y = 0, xend = 3.2, yend = 3.2)) +
                                        xlab("Predicted Relative Risk") + 
                                        ylab("Observed Relative Risk in UKB Cohort") +
                                        theme_hh() 

                
                ggsave(file.path(save_folder, paste0(model_list[i], "_", outcome_list[j], ".png")),  dpi = 300, type = "cairo")
        }         
}

