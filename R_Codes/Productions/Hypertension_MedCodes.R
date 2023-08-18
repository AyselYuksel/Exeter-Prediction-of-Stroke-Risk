# FINDS MED CODES OF STROKE


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



df_med_dict_file <- read.csv("CPRDAurumMedical.txt",sep="\t")
df_med_dict_file

df_medcodes_hypertension = read.csv("exeter_medcodelist_hypertension.txt", sep ="\t")

cprd = CPRDData$new(cprdEnv = "analysis",
                    cprdConf = "/Users/ayselyuksel/Documents/EXETER_HDS_DOCUMENT/RESEARCH_PROJECT/R_Codes/aurum.yaml")
codesets = cprd$codesets()
codesets

df_medcodes_hypertension <- df_medcodes_hypertension%>%
  mutate(medcodeid = as.character(medcodeid))
df_medcodes_hypertension

medcodes_hypertension = codesets$loadMedCodeSet(df_medcodes_hypertension, category="term_medcode", "hypertension", '10/08/2023', colname="medcodeid")
medcodes_hypertension


