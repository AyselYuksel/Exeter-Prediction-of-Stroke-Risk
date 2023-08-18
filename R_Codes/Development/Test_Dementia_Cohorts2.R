# Project : Prediction of stroke risk associated with risperidone
# Preparing Group 1 and Group 2: people with dementia who have been prescribed risperidone.
# Aysel Yuksel MSc Health Data Science

#######
###
##
######################################################################################################################

# Setup
setwd("/Users/ayselyuksel/Documents/EXETER_HDS_DOCUMENT/RESEARCH_PROJECT/R_Codes/Development")
getwd()

library(aurum)
library(tidyverse)
library(dplyr)

cprd = CPRDData$new(cprdEnv = "analysis",
                    cprdConf = "/Users/ayselyuksel/Documents/EXETER_HDS_DOCUMENT/RESEARCH_PROJECT/R_Codes/aurum.yaml")
codesets = cprd$codesets()
codesets
codes = codesets$getAllCodeSetVersion(v = "19/06/2023")
codes

analysis = cprd$analysis("ays_test")
analysis
# all tables 'cached' (saved on MySQL) will have this prefix


######################################################################################################################




# Merge observation table with dementia codes
dem_obs_codes <- cprd$tables$observationTest %>% inner_join(codes$dementia) %>% analysis$cached("dem_obs_codes", indexes=c("patid", "obsdate"))



dem_obs_codes



#currentDate<-format(Sys.Date(), "%Y")
#currentDate
#Sys.Date()


#dementia_patients <- cprd$tables$patientTest %>%
#  select(patid, acceptable) %>%
#  filter(acceptable != 0 && patid ==405802320484 ) %>%
#  inner_join(all_dementia_obs_codes, by="patid") %>%
#  group_by(patid) %>%
#  mutate(mindate =  min(obsdate) ) %>%
#  mutate(minenterdate =  min(enterdate) ) %>%
#  filter(mindate == obsdate) %>%
#  filter(minenterdate == enterdate)

#####################################
# RISPERIDONE COHORT
######################################

# Find minimum dementia observation date for each patient
dem_pat_first_obs <- cprd$tables$patientTest %>%
  select(patid, acceptable, pracid, regenddate, emis_ddate, yob, mob) %>%
  filter( acceptable != 0 ) %>%
  inner_join(dem_obs_codes, by = "patid", "pracid") %>%
  group_by(patid, pracid.x, regenddate, emis_ddate, yob, mob) %>%
  summarize(first_obsdate = min(obsdate))


dem_pat_first_obs

dem_pat_first_obs <- dem_pat_first_obs %>% analysis$cached("dem_pat_first_obs", indexes=c("patid", "first_obsdate"))


#drug issue

risp_codes <- cprd$tables$drugIssueTest %>% inner_join(codes$risperidone) %>% analysis$cached("risp_codes", indexes=c("patid", "issuedate"))


#risperidone_drug_codes



# Find minimum risperidone issue date for each patient
dem_pat_first_risp <- cprd$tables$patientTest %>%
  select(patid, acceptable) %>%
  filter( acceptable != 0 ) %>%
  inner_join(risp_codes, by = "patid") %>%
  group_by(patid) %>%
  summarize(first_risp_date = min(issuedate))

dem_pat_first_risp

dem_pat_first_risp <- dem_pat_first_risp %>% analysis$cached("dem_pat_first_risp", indexes=c("patid", "first_risp_date"))


# Excluding any patients whose earliest dementia code is after the date of earliest risperidone prescription

dem_pat_risp_after_dem <- dem_pat_first_obs %>%
  select(patid, pracid.x, regenddate, emis_ddate, first_obsdate, yob, mob) %>%
  inner_join(dem_pat_first_risp, by = "patid") %>%
  mutate (dob = as.Date(paste0(yob, ifelse(is.null(mob), "01", mob), "01"))) %>%
  filter( first_obsdate <= first_risp_date && datediff(first_risp_date, dob) >= 65*365.25 )


dem_pat_risp_after_dem

dem_pat_risp_after_dem <- dem_pat_risp_after_dem %>% analysis$cached("dem_pat_risp_after_dem", indexes=c("patid"))

dem_cohort_1 <- dem_pat_risp_after_dem %>%
  select(patid, pracid = pracid.x, first_obsdate, dob, yob, mob, regenddate, emis_ddate) %>%
  inner_join(cprd$tables$practice, by = "pracid") %>%
  select(patid, pracid, first_obsdate, dob, yob, mob, regenddate, emis_ddate, lcd) %>%
  mutate(end_date = pmin(ifelse(is.null(regenddate), as.Date('2021-06-01'), regenddate),
                         ifelse(is.null(emis_ddate), as.Date('2021-06-01'), emis_ddate) , as.Date('2021-06-01'),
                         ifelse(is.null(lcd), as.Date('2021-06-01'), lcd) ))

dem_cohort_1


#####################################
# POTENTIAL COHORTS
######################################

# Step 1: Identify all patients with a code for dementia at any time point
dem_obs_codes

# Step 2: Identify all patients from Step 1 who reach age 65 at any date during the study period (Jan 2000-June 2021) or before

dem_pat_over_65 <- cprd$tables$patientTest %>%
  select(patid, acceptable, pracid, regenddate, emis_ddate, yob, mob) %>%
  filter( acceptable != 0 ) %>%
  inner_join(dem_obs_codes, by = "patid", "pracid") %>%
  group_by(patid, pracid.x, regenddate, emis_ddate, yob, mob) %>%
  summarize(first_obsdate = min(obsdate)) %>%
  mutate (dob = as.Date(  paste0(  yob, ifelse(  is.null(mob), "01", ifelse(length(mob)==1, paste0("0", mob), mob )  ), "01")  )  ) %>%
  filter( datediff(as.Date('2021-06-01'), dob) >= 65*365.25 )  %>%
  filter(datediff(regenddate, dob) >= 65*365.25 )  %>%
  filter(datediff(emis_ddate, dob) >= 65*365.25 )

dem_pat_over_65

dem_pat_over_65 <- dem_pat_over_65 %>% analysis$cached("dem_pat_over_65", indexes=c("patid"))


# Never prescribed risperidone
dem_cohort_2 <- dem_pat_over_65 %>%
    select(patid, pracid = pracid.x, first_obsdate, dob, yob, mob, regenddate, emis_ddate) %>%
    anti_join(risp_codes, by = "patid")

dem_cohort_2

dem_cohort_2 <- dem_cohort_2 %>% analysis$cached("dem_cohort_2", indexes=c("patid"))


