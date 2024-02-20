##File name: get_gp_consult_rate.r
##Authors: Hannah Harrison
##Last Edit: 18/08/2022
##Description: gets number of gp consultations in 1yr and 5yr follow-up respectively

#use subset of df_EHR_cohort_main to get gp_end_date
df_EHR_temp <- df_EHR_cohort_main %>% select(c("eid", "gp_end_date"))
#load gp clinical dataset 
df_gp_clinical <- fread(fpath_gp, colClasses = "character")
length(df_gp_clinical$eid) #123651169 
length(unique(df_gp_clinical$eid)) #230078 

df_gp_clinical <- df_gp_clinical %>% filter(data_provider == 3) %>% select(c("eid", "event_dt")) #make smaller, save time
df_gp_clinical$eid <- as.integer(df_gp_clinical$eid)
df_gp_clinical$event_dt <- as.Date(df_gp_clinical$event_dt, format = "%d/%m/%Y")

length(df_gp_clinical$eid) #87480305 
length(unique(df_gp_clinical$eid)) #165173 

#add in baseline date for each event
all_baseline_fields <- fread(fpath_baseline_vars)
ukb_fields <- all_baseline_fields %>% filter(type == "ukb")
df_baseline_basic <- fread(fpath_baseline, select = c("eid", ukb_fields$field))
colnames(df_baseline_basic) <- gsub(x = names(df_baseline_basic), pattern = "-|\\.", replacement = "_") 
colnames(df_baseline_basic) <- gsub(x = names(df_baseline_basic), pattern = "^([0-9])", replacement = "v\\1") 
colnames(df_baseline_basic) <- gsub(x = names(df_baseline_basic), pattern = "_0$", replacement = "") 
df_baseline_basic <- df_baseline_basic %>% rename(baseline_date = v53_0) %>% select(!c("v21003_0", "v54_0"))
df_baseline_basic$baseline_date <- as.Date(df_baseline_basic$baseline_date)
df_gp_clinical <-  left_join(x = df_gp_clinical, y = df_baseline_basic, by = "eid")
length(df_gp_clinical$eid) #87480305
length(unique(df_gp_clinical$eid)) #165173

##remove events prebaseline 
df_gp_post_baseline <- df_gp_clinical %>% filter(event_dt > baseline_date)
length(df_gp_post_baseline$eid) #50848807
length(unique(df_gp_post_baseline$eid)) #163701

##get events in each year of follow-up (cumulative)
df_gp_main <- df_gp_post_baseline %>% filter(event_dt < as.Date("2016-07-01")) ##last date for primary care records from data provider = 3
#length(df_gp_main$eid) #50847324
#length(unique(df_gp_main$eid)) #163700


 #get overall count of consultations (disregarding multiple events on the same day)
df_gp_main <- df_gp_main %>% distinct(eid, event_dt, .keep_all = TRUE) 
df_gp_main <- df_gp_main %>% inner_join(x = df_gp_main, y = df_EHR_temp, by = "eid") #removes people not in main analysis, and adds gp_end_date for all

df_gp_main <- df_gp_main %>% group_by(eid)
for (i in 1:10){
    new_var <- paste0("cons_in_", i, "_yrs_fu")
    df_gp_main <- df_gp_main %>% mutate(!!new_var := sum(event_dt < baseline_date + years(i)))
}

df_gp_main <- df_gp_main %>% mutate(cons_all = sum(event_dt < gp_end_date))

df_gp_main <- df_gp_main %>% ungroup()
df_gp_main <- df_gp_main %>% distinct(eid, .keep_all = TRUE)
df_gp_main <- df_gp_main %>% select(!c("baseline_date", "gp_end_date"))


##get only events in one year follow-up
df_gp_1yr <- df_gp_post_baseline %>% filter(event_dt < baseline_date + years(1))
length(df_gp_1yr$eid) #6124345
length(unique(df_gp_1yr$eid)) #141104
df_gp_1yr <- df_gp_1yr %>% distinct(eid, event_dt) %>% count(eid) #get count of consultations (disregarding multiple events on the same day)

##get only events in 5 year follow-up
df_gp_5yr <- df_gp_post_baseline %>% filter(event_dt < baseline_date + years(5)) 
length(df_gp_5yr$eid) #34128803
length(unique(df_gp_5yr$eid)) #160011
df_gp_5yr <- df_gp_5yr %>% distinct(eid, event_dt) %>% count(eid) #get count of consultations (disregarding multiple events on the same day)

##get all events before gp_end_date
