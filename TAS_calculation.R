

########################################
#
# TAS CALCULATION FOR ADIPOGENOME
#
########################################

# R packages
library(shiny)
library(K2Taxonomer)
library(visNetwork)
library(plotly)
library(heatmaply)
library(DT)
library(GSVA)
library(Biobase)
library(RColorBrewer)
library(limma)
library(dendextend)
library(tidyverse)

# Read in analysis results
K2summary <- readRDS("data/ADIPO/K2results.rds")

# Parse results
chemical <- K2info(K2summary) # chemical info
write.csv(chemical, "data/ADIPO/chemical.csv", row.names = TRUE)

# Convert concentration from M to uM
chemical$Concentration[which(chemical$Concentration %in% "10 ug/ml")] <- 10/10^6
chemical$Concentration <- as.numeric(chemical$Concentration) * 10^6

profile <- K2eSet(K2summary)@phenoData@data # profile information
write.csv(profile, "data/ADIPO/profile.csv", row.names = TRUE)

differential_expression <- K2data(K2summary) # z-score of each chemical
write.csv(differential_expression, "data/ADIPO/differential_expression.csv", row.names = TRUE)

###########################################
#
# ASSUME THERE ARE NO REPLICATES
# 
###########################################

# Calculate the signature strength (SS)
signature_strength <- data.frame(sig_id=colnames(differential_expression), SS=NA, stringsAsFactors = FALSE)

for(i in 1:ncol(differential_expression)){
  #i=1;
  signature_strength$SS[which(signature_strength$sig_id %in% colnames(differential_expression)[i])] = length(which(abs(differential_expression[,i]) >= 2))
}

# Calculate TAS
TAS <- data.frame(Chemical=signature_strength$sig_id, TAS=NA, stringsAsFactors=FALSE)

for(i in 1:nrow(TAS)){
  #i=1;
  TAS$TAS[i] = sqrt(signature_strength$SS[i]/nrow(differential_expression)) 
}

# chemical annotation information
chemical_info <- chemical %>% mutate(BUID=paste0("BUID_", 1:nrow(chemical))) %>% 
  left_join(TAS) %>% 
  filter(!is.na(TAS)) %>% 
  rename(
    "Abbreviation" := Chemical ,
    "Chemical Name" := Full,
    "BUID" := BUID,
    "CAS" := CAS,
    "Source/Use" := Source,
    "dose (uM)" := Concentration,
    "PPARg_Mod" := PPARg_Mod,
    "TAS" := TAS
  ) %>% 
  select(
    c("Chemical Name", "Abbreviation", "BUID", "CAS", "Source/Use", "dose (uM)", "PPARg_Mod", "TAS")
  ) 
  
write.csv(chemical_info, "data/ADIPO/chemical_info.csv", row.names = FALSE)

# profile annotation information
profile_info <- data.frame(sig_id=rownames(profile ), profile %>% select(-Concentration)) %>% 
  left_join(chemical %>% mutate(BUID=paste0("BUID_", 1:nrow(chemical)))) %>% 
  left_join(
    TAS
  ) %>% 
  rename(
    "Abbreviation" = Chemical ,
    "Chemical Name" := Full,
    "BUID" := BUID,
    "CAS" := CAS,
    "Source/Use" := Source,
    "dose (uM)" := Concentration,
    "TAS" := TAS
  ) %>% 
  select(
    c("sig_id", "Chemical Name", "Abbreviation", "BUID", "CAS", "Source/Use", "NileRed", "Plate", "dose (uM)", "PPARg_Mod", "TAS")
  ) 

write.csv(profile_info, "data/ADIPO/profile_info.csv", row.names = FALSE)
       
##Create adipogenome data####
adipogenome <- list()

adipogenome[["Profile Annotation"]] <- profile_info
adipogenome[["Chemical Annotation"]] <- chemical_info
adipogenome[["Gene Expression"]] <-  K2eSet(K2summary)
adipogenome[["Gene Set Enrichment"]] <- carcinogenome_data[["Gene Set Enrichment"]]
adipogenome[["Connectivity"]] <- carcinogenome_data[["Connectivity"]]
adipogenome[["title"]] <- "ADIPO Portal"
adipogenome[["about page"]] <- "introduction_ADIPO.Rmd"

write_rds(adipogenome, "data/ADIPO/data.RDS")
