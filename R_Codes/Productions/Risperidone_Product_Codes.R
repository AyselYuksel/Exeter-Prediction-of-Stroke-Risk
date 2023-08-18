# FINDS PRODUCTION CODES OF RISPERIDONE

# In CPRD Aurum prodcodes lookup, product name can contain generic or brand name, drug substance name only contains generic names

# Produces txt file with fields separated by tabs and Windows-style (\r\n aka CR LF) line endings

############################################################################################

setwd("/Users/ayselyuksel/Documents/EXETER_HDS_DOCUMENT/RESEARCH_PROJECT/R_Codes")
getwd()

library(tidyverse)
#install.packages("rlang")
#install.packages("dplyr")
library(stats)
library("rlang")
library("dplyr")

############################################################################################

# Import Aurum prodcode lookup table
drug_prodcodes <- read.csv("CPRDAurumProduct.txt",sep="\t")

############################################################################################

#drug_substance_match <- aurum_prodcodes %>% filter(grepl("risperidone", 'DrugSubstanceName', ignore.case=TRUE))
View(drug_prodcodes)

#risperidone
drug_names <- c("risperidone")
risperidone_match <- filter(drug_prodcodes, grepl(paste(drug_names, collapse = '|'), Term.from.EMIS, ignore.case=TRUE) | grepl('risperidone', "DrugSubstanceName", ignore.case=TRUE)  )


View(risperidone_match)

write.table(risperidone_match, file="risperidone_prod_codes.txt", sep="\t", row.names=FALSE, quote=FALSE)

cprd = CPRDData$new(cprdEnv = "analysis",
                    cprdConf = "/Users/ayselyuksel/Documents/EXETER_HDS_DOCUMENT/RESEARCH_PROJECT/R_Codes/aurum.yaml")
codesets = cprd$codesets()
risperidone_match <- risperidone_match%>%
  mutate(ProdCodeId = as.character(ProdCodeId))
risperidone_match

# insertion of the risperidone product codes to codesets table on MySQL CPRD database
prod_codes = codesets$loadProdCodeSet(risperidone_match, category="Term.from.EMIS", "risperidone", '19/06/2023', colname="ProdCodeId")
prod_codes













