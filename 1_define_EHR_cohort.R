##File name: EHR_cohort.r
##Author: Hannah Harrison
##Last Edit: 21/11/2022
##Description: defines primary care cohort for model development under landmark framework

##read in gp registration data
df_EHR_cohort <- fread(fpath_gp_reg)

#read in required baseline data
all_baseline_fields <- fread(fpath_baseline_vars)
basic_fields <- all_baseline_fields %>% filter(type == "ukb" | type == "age" | type == "sex")
df_baseline_basic <- fread(fpath_baseline, select = c("eid", basic_fields$field))
colnames(df_baseline_basic) <- c("eid", "sex", "genetic_sex", "yob", "mob", "baseline_date", "ukb_centre", "age_at_assessment") 

#merge gp data with baseline 
df_EHR_cohort <-  merge(x = df_EHR_cohort, y = df_baseline_basic, by = "eid") # inner join
length(df_EHR_cohort$eid) #361770 
length(unique(df_EHR_cohort$eid)) #228913 

#As CPRD data uses GP practises registered with EMIS and Vision, we restrict our analysis to English TPP data (data_provider == 3)
#better to group by eid, and remove people ever registered at 1, 2 or 4? 
df_EHR_cohort <- df_EHR_cohort %>% group_by(eid) %>% mutate(dp_3 = case_when(all(data_provider == 3) ~ 1, TRUE ~ 0)) %>% ungroup()
df_EHR_cohort <- df_EHR_cohort %>% filter(dp_3 == 1)
length(df_EHR_cohort$eid) #243548 
length(unique(df_EHR_cohort$eid)) #162164

# impose provider-specific deduct_date (SI code) - for data provider 3 only
df_EHR_cohort <- df_EHR_cohort %>% mutate_at(vars(grep("date", names(df_EHR_cohort), value = TRUE)), funs(as.Date(.,"%d/%m/%Y")))
df_EHR_cohort$deduct_date <- fifelse((is.na(df_EHR_cohort$deduct_date) | (df_EHR_cohort$deduct_date > as.Date("2016-07-01"))) , 
    as.Date("2016-07-01", format="%Y-%m-%d"),
    df_EHR_cohort$deduct_date)

#identify (and remove) any registrations which are after deduct date
df_EHR_cohort <- df_EHR_cohort %>% filter(reg_date <= deduct_date)
length(unique(df_EHR_cohort$eid)) #162156 

#remove any nested periods (should not reduce number of unique eids - checked HH, originally sam ip's code)
df_EHR_cohort <- df_EHR_cohort %>% group_by(eid) %>% 
    arrange(reg_date, .by_group = TRUE) %>% 
    mutate(next_reg_date = lead(reg_date), next_deduct_date = lead(deduct_date), num_entries = n()) %>% ungroup()
df_nested <- df_EHR_cohort %>% filter((next_reg_date >= reg_date) & (next_deduct_date <= deduct_date))
df_EHR_cohort <-  df_EHR_cohort %>% anti_join(df_nested) 
length(unique(df_EHR_cohort$eid)) #162156 

#find discontinuities
df_EHR_cohort <- df_EHR_cohort %>% group_by(eid) %>% 
    arrange(reg_date, .by_group = TRUE) %>% 
    mutate(discont = ifelse(!is.na(next_reg_date), 1*((next_reg_date-deduct_date)>90), 0)) %>% ungroup()

##prioritise later data    # choose last continuous period ----
deduct_date_lastdiscont <- df_EHR_cohort %>% filter(discont == 1) %>% group_by(eid) %>% 
                    summarise(deduct_date_lastdiscont = max(deduct_date[discont==1])) # for eids with disconts -- pick one with latest deduct date
length(unique(deduct_date_lastdiscont$eid)) #12177
df_EHR_cohort <- merge(x = df_EHR_cohort, y = deduct_date_lastdiscont, by = "eid", all.x = TRUE) %>% 
    filter((reg_date > deduct_date_lastdiscont) | is.na(deduct_date_lastdiscont)) # keep all periods with reg_date after unique deduct_date_lastdiscont if latter exists
df_EHR_cohort <- df_EHR_cohort %>% group_by(eid) %>% summarise(gp_start_date = min(reg_date), gp_end_date = max(deduct_date)) 
length(unique(df_EHR_cohort$eid)) #162156

##remove people with gp_end_date prior to baseline assessment
df_EHR_cohort <- df_EHR_cohort %>% left_join(df_baseline_basic, by="eid") #%>% filter(gp_end_date >= baseline_date) 
#length(unique(df_EHR_cohort$eid)) #159909

##remove people with gp_start_date after baseline
#df_EHR_cohort <- df_EHR_cohort %>%  filter(gp_start_date <  baseline_date - ) %>% select(c("eid", "gp_start_date", "gp_end_date", "baseline_date"))
#length(unique(df_EHR_cohort$eid)) #114566

#select cohort with at least 1 year lb (only) 
df_EHR_cohort <- df_EHR_cohort %>% mutate(id_bad_lb_1yr = case_when(gp_start_date > as.Date(baseline_date) - years(1) ~ 1, TRUE ~0))
df_EHR_cohort_lb_only <- df_EHR_cohort %>% filter(id_bad_lb_1yr == 0) %>% select(!ends_with("yr"))
length(unique(df_EHR_cohort_lb_only$eid)) #112207 - USE FOR SENSITIVITY ANALYSIS

#select cohort with at least 6 months follow-up (compromise for consultation rate analysis) - THIS IS THE MAIN ANALYSIS COHORT
df_EHR_cohort_main <- df_EHR_cohort %>% mutate(id_bad_follow_6mnth = case_when(gp_end_date < as.Date(baseline_date) +  months(6) ~ 1, TRUE ~0))
df_EHR_cohort_main <- df_EHR_cohort_main %>% filter(id_bad_lb_1yr == 0)
length(unique(df_EHR_cohort_main$eid)) #114001
df_EHR_cohort_main <- df_EHR_cohort_main %>% filter(id_bad_follow_6mnth == 0) %>% select(!ends_with(c("yr", "mnth")))
length(unique(df_EHR_cohort_main$eid)) #111898 GET THIS NUMBER - MAKE THIS MAIN COHORT

#select cohort with 1 year follow-up 
df_EHR_cohort <- df_EHR_cohort %>% mutate(id_bad_follow_1yr = case_when(gp_end_date < as.Date(baseline_date) +  years(1) ~ 1, TRUE ~0))
df_EHR_cohort_1yr <- df_EHR_cohort %>% filter(id_bad_lb_1yr == 0 & id_bad_follow_1yr == 0) %>% select(!ends_with("yr"))
length(unique(df_EHR_cohort_1yr$eid)) #111385 - USE FOR SENSITIVITY ANALYSIS

#select cohort with 5 year follow-up 
df_EHR_cohort <- df_EHR_cohort %>% mutate(id_bad_follow_5yr = case_when(gp_end_date < as.Date(baseline_date) + years(5) ~ 1, TRUE ~0))
df_EHR_cohort_5yr <- df_EHR_cohort %>% filter(id_bad_lb_1yr == 0 & id_bad_follow_5yr == 0) %>% select(!ends_with("yr"))
length(unique(df_EHR_cohort_5yr$eid)) #105885 - USE FOR SENSITIVITY ANALYSIS