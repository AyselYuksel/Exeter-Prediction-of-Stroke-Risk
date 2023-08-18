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

prod_codes = codesets$loadProdCodeSet(risperidone_match, category="Term.from.EMIS", "risperidone", '20/06/2023', colname="ProdCodeId")
prod_codes

#risperdal
drug_names_risperdal <- c("risperdal", "okedi", "perseris","rykindo","risperlet")
risperdal_match <- filter(drug_prodcodes, grepl(paste(drug_names_risperdal, collapse = '|'), Term.from.EMIS, ignore.case=TRUE) | grepl('risperidone', "DrugSubstanceName", ignore.case=TRUE)  )


View(others_match)

write.table(risperdal_match, file="risperdal_prod_codes.txt", sep="\t", row.names=FALSE, quote=FALSE)

cprd = CPRDData$new(cprdEnv = "analysis",
                    cprdConf = "/Users/ayselyuksel/Documents/EXETER_HDS_DOCUMENT/RESEARCH_PROJECT/R_Codes/aurum.yaml")
codesets = cprd$codesets()
risperdal_match <- risperdal_match%>%
  mutate(ProdCodeId = as.character(ProdCodeId))
risperdal_match

prod_codes_risperdal = codesets$loadProdCodeSet(risperdal_match, category="Term.from.EMIS", "risperdal", '21/06/2023', colname="ProdCodeId")
prod_codes_risperdal


#other_drugs

drug_names_others <- c("risperdal", "okedi", "perseris","rykindo","risperlet") #change this part
others_match <- filter(drug_prodcodes, grepl(paste(drug_names_others, collapse = '|'), Term.from.EMIS, ignore.case=TRUE) | grepl('risperidone', "DrugSubstanceName", ignore.case=TRUE)  )


View(others_match)

write.table(others_match, file="other_antipsychotics_prod_codes.txt", sep="\t", row.names=FALSE, quote=FALSE)

cprd = CPRDData$new(cprdEnv = "analysis",
                    cprdConf = "/Users/ayselyuksel/Documents/EXETER_HDS_DOCUMENT/RESEARCH_PROJECT/R_Codes/aurum.yaml")
codesets = cprd$codesets()
others_match <- others_match%>%
  mutate(ProdCodeId = as.character(ProdCodeId))
others_match

prod_codes_others = codesets$loadProdCodeSet(others_match, category="Term.from.EMIS", "other_antipsychotics", '22/06/2023', colname="ProdCodeId")
prod_codes_others









