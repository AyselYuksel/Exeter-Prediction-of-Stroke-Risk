# Project : Prediction of stroke risk
# Preparing Group 1: people with dementia who have been prescribed risperidone.
# Aysel Yuksel MSc Health Data Science
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

analysis = cprd$analysis("aysel_test")
analysis
# all tables 'cached' (saved on MySQL) will have this prefix


######################################################################################################################



# Similarly for a whole codelist (once the codelist is on MySQL):
# Merge observation table with dementia codes to define the
all_dementia_obs_codes <- cprd$tables$observationTest %>% inner_join(codes$dementia) %>% analysis$cached("all_dementia_obs_codes", indexes=c("patid", "obsdate"))

#all_dementia_obs_codes_local <- all_dementia_obs_codes %>% collect()
## This is now in your local environment - please don't save it though (even if patids have been removed)
## Sometimes variables are of class 'integer64' which can give errors when you run some statistical functions -
## if this happens then convert them to normal integers

#all_dementia_obs_codes

#write.table(all_dementia_obs_codes, file="all_dementia_obs_codes.txt", sep="\t", row.names=FALSE, quote=FALSE)

currentDate<-format(Sys.Date(), "%Y")
currentDate


dementia_patients <- cprd$tables$patientTest %>%
  select(patid, yob) %>%
  filter(currentDate-yob >= 65) %>%
  inner_join(all_dementia_obs_codes, by="patid")

dementia_patients <- dementia_patients %>% analysis$cached("dementia_patients", indexes=c("patid", "obsdate"))

#write.table(dementia_patients, file="dementia_patients.txt", sep="\t", row.names=FALSE, quote=FALSE)

#drug issue

all_risperidone_drug_codes <- cprd$tables$drugIssueTest %>% inner_join(codes$risperidone) %>% analysis$cached("all_risperidone_drug_codes", indexes=c("patid", "issuedate"))


#all_risperidone_drug_codes

#write.table(all_risperidone_drug_codes, file="all_risperidone_drug_codes.txt", sep="\t", row.names=FALSE, quote=FALSE)

#Merge all_risoeridone_drug_codes and dementia_patients

all_risperidone_drug_codes_distinct <- all_risperidone_drug_codes %>% distinct(patid)
all_risperidone_drug_codes_distinct

dementia_patients_distinct <- dementia_patients %>% distinct(patid)
dementia_patients_distinct



cohort_1 <- dementia_patients_distinct %>% inner_join(all_risperidone_drug_codes_distinct) %>% analysis$cached("cohort_1", indexes=c("patid"))

#cohort_1 <- cohort_1 %>% analysis$cached("cohort_1", indexes="patid")

cohort_1

#write.table(cohort_1, file="cohort_1.txt", sep="\t", row.names=FALSE, quote=FALSE)


#cohort 2


cohort_2 <- anti_join(dementia_patients_distinct, cohort_1, by = 'patid') %>% analysis$cached("cohort_2", indexes="patid")

cohort_2

