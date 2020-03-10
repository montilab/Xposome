#########################################################
# 
# COMBINE ADIPOGENS AND CARCINOGENS DATA
# 
#########################################################
library(limma)
library(GSVA)
library(Biobase)
library(GSEABase)
library(tidyverse)

# Loading data files ####
HEPG2 <- readRDS(paste0("data/HEPG2/data.RDS"))
MCF10A <- readRDS(paste0("data/MCF10A/data.RDS"))
ADIPO <- readRDS(paste0("data/ADIPO/data.RDS"))

# Read in analysis results
K2_HEPG2 <- readRDS("data/HEPG2/K2results.rds")
K2_MCF10A <- readRDS("data/MCF10A/K2results.rds")
K2_ADIPO <- readRDS("data/ADIPO/K2results.rds")

