# FINDS MED CODES OF DIMENTIA

# In CPRD Aurum prodcodes lookup, product name can contain generic or brand name, drug substance name only contains generic names

# Produces txt file with fields separated by tabs
# Project : Prediction of stroke risk
# Aysel Yuksel MSc Health Data Science

############################################################################################


setwd("/Users/ayselyuksel/Documents/EXETER_HDS_DOCUMENT/RESEARCH_PROJECT/R_Codes")
getwd()

library(aurum)
library(stats)
library(rlang)
library(dplyr)
library(tidyverse)



#cprd = CPRDData$new(cprdEnv = "analysis",
#                    cprdConf = "/Users/ayselyuksel/Documents/EXETER_HDS_DOCUMENT/RESEARCH_PROJECT/R_Codes/aurum.yaml")

#df_med_dict = cprd$tables$medDict
#df_med_dict

#write.table(df_med_dict, file="exeter_med_dict.csv", sep=",")
#df_med_dict_file = read.csv("exeter_med_dict.csv",sep=",")

df_med_dict_file <- read.csv("CPRDAurumMedical.txt",sep="\t")

df_med_dict_file

# Import read and snomed codes for Dementia
df_read = read.csv("Dementia_Read_Codes.csv", sep =",")
df_read
df_snomed = read.csv("Dementia_Snomed_Codes.csv",sep=",")
df_snomed
#df_snomed <- df_snomed%>%
#  mutate(Snomed_Codes = as.character(Snomed_Codes))
#df_snomed

df_medcode_1 = inner_join(df_med_dict_file, df_read, by = c('OriginalReadCode'='Read_Codes') )
df_medcode_1

df_medcode_2 = inner_join(df_med_dict_file, df_snomed, by = c('SnomedCTConceptId'='Snomed_Codes') )
df_medcode_2

df_medcodes = rbind(df_medcode_1, df_medcode_2)
df_medcodes
write.table(df_medcodes, file="dementia_med_codes.csv", sep=",", row.names=FALSE, quote=FALSE)


cprd = CPRDData$new(cprdEnv = "analysis",
                    cprdConf = "/Users/ayselyuksel/Documents/EXETER_HDS_DOCUMENT/RESEARCH_PROJECT/R_Codes/aurum.yaml")
codesets = cprd$codesets()
codesets

df_medcodes <- df_medcodes%>%
  mutate(MedCodeId = as.character(MedCodeId))
df_medcodes

med_codes = codesets$loadMedCodeSet(df_medcodes, category="Term", "dementia", '19/06/2023', colname="MedCodeId")
med_codes


