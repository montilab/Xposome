
#######################################
#
# CODE TO CALCULATE TAS FOR ADIPOGENOME
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

# Read in the analysis results from Eric
K2summary <- readRDS("data/ADIPO/K2results.rds")

# Parse results
chemical <- K2info(K2summary) # chemical info
profile <- K2eSet(K2summary)@phenoData@data # profile info
differential_expression <- K2eSet(K2summary)@assayData[["exprs"]]   # differential gene expression info

# Compute the spearman correlation for all pairwise combinations of replicate level 4 (differential expression) profiles 
samples <- unique(rownames(profile))
spearman_corr <- matrix(NA, nrow=length(samples), ncol=length(samples), byrow=T, dimnames = list(samples, samples))

for(k in 1:nrow(spearman_corr)){
  for(l in 1:ncol(spearman_corr)){
    spearman_corr[k,l] = cor(differential_expression[,samples[k]], differential_expression[,samples[l]], method="spearman")
  }
}

# Get the lower diagonal of spearman corr matrix
lower_triangle <- spearman_corr[lower.tri(spearman_corr)]
replicate_corr <- quantile(lower_triangle, c(0.75)) # get 75th percentile of the spearman corr

# Getting the number of replicate per each profile
n_replicate <- matrix(NA, nrow=1, ncol=length(samples), byrow=T, dimnames = list("number_of_replicate", samples))

for(i in 1:nrow(n_replicate)){
  for(j in 1:ncol(n_replicate)){
    #i=1; j=1;
    chem_rep <- profile$Chemical[which(rownames(profile) %in% samples[j])]
    n_replicate[i,j] = nrow(profile %>% filter(Chemical %in% chem_rep))
  }
}

# Calculate the adjusted z-scores
Z_adjust <- differential_expression

for(j in 1:ncol(Z_adjust)){
  #j=1;
  Z_adjust[,j] = differential_expression[,j]*sqrt(n_replicate[,which(colnames(n_replicate) %in% colnames(differential_expression)[j])])
}

# Calculate the signature strength (SS) for each signature
signature_strength <- matrix(NA, nrow=1, ncol=length(samples), byrow=T, dimnames = list("SS", samples))

for(i in 1:ncol(Z_adjust)){
  #i=1;
  signature_strength[,i] = length(which(abs(Z_adjust[,i]) >= 2)) # get number of absoulate z-score >= 2
}

# Calculate TAS
TAS <- matrix(NA, nrow=1, ncol=length(samples), byrow=T, dimnames = list("TAS", samples))

for(i in 1:ncol(signature_strength)){
  #i=1;
  TAS[,i] = sqrt(signature_strength[,i] * max(c(replicate_corr, 0))/length(differential_expression)) #divide by the total number of genes not landmark gene of 978
}

# Gather the matrix as a data frame
TAS <- data.frame(TAS) %>% 
  gather(key="sig_id", value="TAS") %>% 
  left_join(
    data.frame(sig_id=rownames(profile), Chemical=profile$Chemical, stringsAsFactors=FALSE)
  )

# Calculate the TAS mean for chemicals
TAS_mean <- TAS %>% 
  group_by(Chemical) %>% 
  summarise(TAS_mean=mean(TAS, na.rm=T))

# create chemical annotation 
chemical_info <- chemical %>% 
  left_join(TAS_mean) %>% 
  mutate(CAS_No = paste0("CAS_", 1:nrow(chemical)), BUID=paste0("BUID_", 1:nrow(chemical))) %>% 
  filter(!is.na(TAS_mean)) %>% 
  rename(
    "Abbreviation" = Chemical ,
    "Chemical Name" := Full,
    "BUID" := BUID,
    "CAS No." := CAS_No,
    "Source/Use" := Source,
    "Concentration (M)" := Concentration,
    "TAS (mean)" := TAS_mean
  ) %>% 
  select(
    c("Chemical Name", "Abbreviation", "BUID", "CAS No.", "Source/Use", "Concentration (M)", "TAS (mean)")
  ) 
  
write.csv(chemical_info, "data/ADIPO/chemical_info.csv", row.names = FALSE)

# create profile annotation 
profile_info <- data.frame(sig_id=rownames(profile ), profile) %>% 
  left_join(
    TAS
  )

write.csv(profile_info, "data/ADIPO/profile_info.csv", row.names = FALSE)
