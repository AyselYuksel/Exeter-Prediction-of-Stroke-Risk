######################################################################################################################
# Project : Prediction of hypertension risk associated with risperidone
# 
# Aysel Yuksel MSc Health Data Science
#
## These codes for test purposes
##
##
######################################################################################################################

# Setup
setwd("/Users/ayselyuksel/Documents/EXETER_HDS_DOCUMENT/RESEARCH_PROJECT/R_Codes")
getwd()

library(aurum)
library(tidyverse)
library(dplyr)
library("MatchIt")


cprd = CPRDData$new(cprdEnv = "analysis",
                    cprdConf = "/Users/ayselyuksel/Documents/EXETER_HDS_DOCUMENT/RESEARCH_PROJECT/R_Codes/aurum.yaml")
codesets = cprd$codesets()
codesets
codes = codesets$getAllCodeSetVersion(v = "19/06/2023")
codes

codes_hypertension = codesets$getAllCodeSetVersion(v = "10/08/2023")
codes_hypertension

analysis = cprd$analysis("ays_prod")
analysis
# all tables 'cached' (saved on MySQL) will have this prefix


######################################################################################################################

# Merge observation table with dementia codes
dem_obs_codes <- cprd$tables$observation %>% inner_join(codes$dementia) %>% analysis$cached("dem_obs_codes", indexes=c("patid", "obsdate"))


dem_obs_codes



######################################################################################################################

# Merge observation table with hypertension codes
hypertension_obs_codes <- cprd$tables$observation %>% inner_join(codes_hypertension$hypertension, by="medcodeid") %>% analysis$cached("hypertension_obs_codes", indexes=c("patid", "obsdate"))

hypertension_obs_codes


#####################################
# RISPERIDONE COHORT
######################################

# Step 1: Identify all patients with a code for dementia at any time point.
# Keep only the row of data with the earliest obsdate – this is the date of diagnosis of dementia
# Find minimum dementia observation date for each patient
dem_pat_first_obs <- cprd$tables$patient %>%
  select(patid, acceptable, pracid, regenddate, emis_ddate, yob, mob, gender) %>%
  filter( acceptable != 0 ) %>%
  inner_join(dem_obs_codes, by = "patid", "pracid") %>%
  filter(!is.na(obsdate)) %>%
  filter(year(obsdate) > as.character(yob)) %>%
  group_by(patid, pracid.x, regenddate, emis_ddate, yob, mob, gender) %>%
  summarize(first_obsdate = min(obsdate, na.rm = TRUE)) 



dem_pat_first_obs




#drug issue

risp_codes <- cprd$tables$drugIssue %>% inner_join(codes$risperidone) %>% analysis$cached("risp_codes", indexes=c("patid", "issuedate"))


#risperidone_drug_codes


# Step 2: Identify all patients from Step 1 with a risperidone prescription ever
# Step 3: Identify the earliest date of risperidone prescription for all patients from step 2
# Find minimum risperidone issue date for each patient
dem_pat_first_risp <- cprd$tables$patient %>%
  select(patid, acceptable, yob) %>%
  filter( acceptable != 0 ) %>%
  inner_join(risp_codes, by = "patid") %>%
  filter(!is.na(issuedate)) %>%
  filter(year(issuedate) > as.character(yob)) %>%
  group_by(patid) %>%
  summarize(first_risp_date = min(issuedate, na.rm = TRUE))

dem_pat_first_risp




# Step 4: Exclude any patients whose earliest dementia code is after the date of earliest risperidone prescription
# Excluding any patients whose earliest dementia code is after the date of earliest risperidone prescription

# Step 5: Exclude any patients whose date of risperidone prescription is before age 65

dem_pat_risp_after_dem <- dem_pat_first_obs %>%
  select(patid, pracid.x, regenddate, emis_ddate, first_obsdate, yob, mob, gender) %>%
  inner_join(dem_pat_first_risp, by = "patid") %>%
  mutate (dob = as.Date(paste0(yob, ifelse(is.null(mob), "01", mob), "01"))) %>%
  filter( first_obsdate <= first_risp_date && datediff(first_risp_date, dob) >= 65*365.25 )


dem_pat_risp_after_dem

#dem_pat_risp_after_dem <- dem_pat_risp_after_dem %>% analysis$cached("dem_pat_risp_after_dem", indexes=c("patid"))


# Step 6: For all patients in step 5, define their study index date (date of first risperidone prescription) and
# study end date (earliest of: date of practice deregistration [regenddate], date of death [emis_ddate],
# last collection date [lcd] in Practice table, date of end of study period [01/06/2021].
dem_cohort_risperidone <- dem_pat_risp_after_dem %>%
  select(patid, pracid = pracid.x, first_obsdate, first_risp_date, dob, yob, mob, regenddate, emis_ddate, gender) %>%
  inner_join(cprd$tables$practice, by = "pracid") %>%
  select(patid, pracid, first_obsdate, first_risp_date, dob, yob, mob, regenddate, emis_ddate, gender, lcd) %>%
  mutate(end_date = pmin(ifelse(is.null(regenddate), as.Date('2021-06-01'), regenddate),
                         ifelse(is.null(emis_ddate), as.Date('2021-06-01'), emis_ddate) ,
                         as.Date('2021-06-01'),
                         ifelse(is.null(lcd), as.Date('2021-06-01'), lcd) )) %>%
  mutate(age = datediff(as.Date('2021-06-01'), dob)/365.25 ) %>%
  mutate(case_control =TRUE)

dem_cohort_risperidone

#dem_cohort_risperidone <- dem_cohort_risperidone %>% analysis$cached("dem_cohort_risperidone", indexes=c("patid"))





#####################################
# POTENTIAL COHORTS
######################################

# Step 1: Identify all patients with a code for dementia at any time point
dem_obs_codes

# Step 2: Identify all patients from Step 1 who reach age 65 at any date during the study period (Jan 2000-June 2021) or before

dem_pat_over_65 <- cprd$tables$patient %>%
  select(patid, acceptable, pracid, regenddate, emis_ddate, yob, mob, gender) %>%
  filter( acceptable != 0 ) %>%
  inner_join(dem_obs_codes, by = "patid", "pracid") %>%
  filter(!is.na(obsdate)) %>%
  filter(year(obsdate) > as.character(yob)) %>%
  group_by(patid, pracid.x, regenddate, emis_ddate, yob, mob, gender) %>%
  summarize(first_obsdate = min(obsdate, na.rm = TRUE))%>%
  mutate (dob = as.Date(paste0(yob, ifelse(  is.null(mob), "01", ifelse(length(mob)==1, paste0("0", mob), mob )  ), "01")  )  ) %>%
  filter( datediff(as.Date('2021-06-01'), dob) >= 65*365.25 )  %>%
  filter(datediff(ifelse(is.null(regenddate), as.Date('2021-06-01'), regenddate), dob) >= 65*365.25 )  %>%
  filter(datediff(ifelse(is.null(emis_ddate), as.Date('2021-06-01'), emis_ddate), dob) >= 65*365.25 )

dem_pat_over_65

#dem_pat_over_65 <- dem_pat_over_65 %>% analysis$cached("dem_pat_over_65", indexes=c("patid"))
#dem_pat_over_65

# Never prescribed risperidone
dem_never_risp <- dem_pat_over_65 %>%
  select(patid, pracid.x, first_obsdate, dob, yob, mob, regenddate, emis_ddate, gender) %>%
  anti_join(risp_codes, by = "patid")

dem_never_risp

#dem_never_risp <- dem_never_risp %>% analysis$cached("dem_never_risp", indexes=c("patid"))
#dem_never_risp

# Study end date (earliest of: date of practice deregistration [regenddate], date of death [emis_ddate],
# last collection date [lcd] in Practice table, date of end of study period [01/06/2021].
dem_cohort_control <- dem_never_risp %>%
  select(patid, pracid = pracid.x, first_obsdate, dob, yob, mob, regenddate, emis_ddate, gender) %>%
  inner_join(cprd$tables$practice, by = "pracid") %>%
  select(patid, pracid, first_obsdate, dob, yob, mob, regenddate, emis_ddate, gender, lcd) %>%
  mutate(end_date = pmin(ifelse(is.null(regenddate), as.Date('2021-06-01'), regenddate),
                         ifelse(is.null(emis_ddate), as.Date('2021-06-01'), emis_ddate) ,
                         as.Date('2021-06-01'),
                         ifelse(is.null(lcd), as.Date('2021-06-01'), lcd) )) %>%
  mutate(age = datediff(as.Date('2021-06-01'), dob)/365.25 ) %>%
  mutate(case_control =FALSE)


dem_cohort_control

dem_cohort_control <- dem_cohort_control %>% analysis$cached("dem_cohort_control", indexes=c("patid"))
dem_cohort_control
#####################################
# COMBINED RISPERIDONE AND CONTROL COHORTS
######################################

dem_cohort_risp_report <- dem_cohort_risperidone %>%
  select(patid, index_date = first_risp_date, end_date, age, sex = gender, case_control)

dem_cohort_risp_report

dem_cohort_risp_report <- dem_cohort_risp_report %>% analysis$cached("dem_cohort_risp_report", indexes=c("patid"))

dem_cohort_ctrl_report <- dem_cohort_control %>%
  select(patid, end_date, age, sex = gender, case_control) %>%
  mutate(index_date = as.Date('1900-01-01') ) 

dem_cohort_ctrl_report <- dem_cohort_ctrl_report  %>%
  select(patid, index_date, end_date, age, sex, case_control)

dem_cohort_ctrl_report

dem_cohort_ctrl_report <- dem_cohort_ctrl_report %>% analysis$cached("dem_cohort_ctrl_report", indexes=c("patid"))


df_report <- union(dem_cohort_risp_report, dem_cohort_ctrl_report)

df_report

df_report <- df_report %>% analysis$cached("df_report", indexes=c("patid"))
df_report
#df_report_local <- df_report %>% collect()

# Nearest neighbour matching on age and sex, with up to 10 controls per risperidone case
#The vignettes here explain what match it is doing and how to extract the cases and controls after matching https://cran.r-project.org/web/packages/MatchIt/

#data("df_report", package = "MatchIt")

match_report <- matchit(case_control ~ age + sex , data = data.frame(df_report), method = "nearest", distance = "glm", ratio= 10)
match_report
summary(match_report)

# 4. Extract the matched patients and assign each control the same index date as the index date of their matched risperidone patient

match.t <- get_matches(match_report, data = data.frame(df_report), distance = "glm") #Gets you the matches
table(match.t$case_control)

match.t

main_table <- match.t %>%
  select(id, subclass, weights, pracid, yob, patid, case_control, sex, age, end_date, index_date, glm) %>%
  group_by(subclass) %>%
  summarize(new_index_date = max(index_date))

main_table


n_distinct(main_table$subclass)


#5. Exclude any controls whose end_date < the assigned index date

main_table_final <- main_table %>% 
  select( subclass, new_index_date) %>% 
  inner_join(match.t, by = "subclass") %>%
  filter(end_date > new_index_date)

main_table_final




#Total Final Cohorts - Patients Number:
n_distinct(main_table_final$patid)

#Total Final Cohorts - Patients Number Controls and cases:
count_match_cohorts <- main_table_final %>% group_by(case_control)  %>% count()
count_match_cohorts

# Find the first hypertension observation date

hypertension_pat_first_obs <- cprd$tables$patient %>%
  select(patid, acceptable, pracid, regenddate, emis_ddate, yob, mob) %>%
  filter( acceptable != 0 ) %>%
  inner_join(hypertension_obs_codes, by = "patid", "pracid") %>%
  filter(!is.na(obsdate)) %>%
  filter(year(obsdate) > as.character(yob)) %>%
  group_by(patid, pracid.x, regenddate, emis_ddate, yob, mob) %>%
  summarize(first_hypertension_obs = min(obsdate, na.rm=TRUE)) 


hypertension_pat_first_obs

hypertension_pat_first_obs <- hypertension_pat_first_obs %>% analysis$cached("hypertension_pat_first_obs", indexes=c("patid"))





main_dem_hypertension <- left_join(data.frame(main_table_final), data.frame(hypertension_pat_first_obs), by = "patid")

main_dem_hypertension


main_dem_hypertension98 <- inner_join(data.frame(main_table_final), data.frame(hypertension_pat_first_obs), by = "patid")

main_dem_hypertension98


# 2 date variables
# 1) Add patients' hypertension_beforeindexdate: date of earliest hypertension

main_dem_hypertension2 <- main_dem_hypertension  %>% mutate(hypertension_beforeindexdate = ifelse(first_hypertension_obs < new_index_date, format(first_hypertension_obs, "%Y-%m-%d"), NA))

main_dem_hypertension2


# 2) Add hypertension_afterindex: Date of first hypertension after index date

main_dem_hypertension3 <- data.frame(main_dem_hypertension2)  %>% 
  left_join(data.frame(hypertension_obs_codes), by = "patid") %>%
  select(patid, new_index_date, obsdate) %>%
  group_by(patid) %>%
  summarize(hypertension_afterindexdate = if(all(is.na(obsdate) | obsdate < new_index_date)) NA else min(obsdate[obsdate > new_index_date]))

main_dem_hypertension3 


main_dem_hypertension4 = inner_join(main_dem_hypertension2, main_dem_hypertension3, by = "patid")
main_dem_hypertension4


# 3 binary variables
# hypertension_beforeindex: Add binary which is 1 if a patient has a history of hypertension prior to index date, otherwise set to 0

# hypertension_in12monthsafterindex: Add binary which is 1 if a patient has a hypertension record in the 12 months after the index date, otherwise set to 0
# hypertension_anyafterindex: Add binary which is 1 if a patient has a hypertension record at any time after the index date, otherwise set to 0


main_dem_hypertension5 <- main_dem_hypertension4 %>% mutate(hypertension_beforeindex =ifelse(is.na(first_hypertension_obs), 0, ifelse(first_hypertension_obs < new_index_date, 1, 0)),
                                                hypertension_in12monthsafterindex = ifelse(is.na(hypertension_afterindexdate), 0, ifelse(as.numeric(difftime(hypertension_afterindexdate, new_index_date)) <= 365.25, 1, 0)),
                                                hypertension_anyafterindex = ifelse(is.na(hypertension_afterindexdate), 0, ifelse(hypertension_afterindexdate >= new_index_date, 1, 0)) )

main_dem_hypertension5


#n_distinct(main_dem_hypertension5$hypertension_beforeindex)

count_hypertension_before_index <- main_dem_hypertension5 %>% group_by(hypertension_beforeindex)  %>% count()
count_hypertension_before_index

count_hypertension_in12monthsafterindex <- main_dem_hypertension5 %>% group_by(hypertension_in12monthsafterindex)  %>% count()
count_hypertension_in12monthsafterindex

count_hypertension_anyafterindex <- main_dem_hypertension5 %>% group_by(hypertension_anyafterindex)  %>% count()
count_hypertension_anyafterindex

#Summary tables
#2 Age (Mean SD)
df_age <- main_dem_hypertension5 %>% group_by(case_control) %>% 
  summarise(sd = sd(age),
            mean = mean(age),
            range = paste(min(age), "-", max(age)),
            n = sum(!is.na(age))
  )
df_age
#3 Sex: Number of females (% female)
df_sex <- main_dem_hypertension5 %>% group_by(case_control, sex) %>% count()
df_sex

#4 History of hypertension (hypertension_beforeindex):  
df_hypertension_beforeindex <- main_dem_hypertension5 %>% group_by(case_control, hypertension_beforeindex) %>% count()
df_hypertension_beforeindex

#5 History of hypertension (hypertension_in12monthsafterindex):  
df_hypertension_in12monthsafterindex <- main_dem_hypertension5 %>% group_by(case_control, hypertension_in12monthsafterindex) %>% count()
df_hypertension_in12monthsafterindex

#6 History of hypertension (hypertension_anyafterindex):  
df_hypertension_anyafterindex <- main_dem_hypertension5 %>% group_by(case_control, hypertension_anyafterindex) %>% count()
df_hypertension_anyafterindex

