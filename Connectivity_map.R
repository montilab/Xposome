
library(cmapR)
library(tidyverse)

dat <- readRDS(paste0("data/ADIPO/data.RDS"))

classes = c("pcl", "pert")

# Create a null list 
connectivity_map <- list();

for(c in 1:length(classes)){
  #c=1;
  #read in the summary
  connectivity <- parse_gctx(paste0("data/Connectivity Map/ps_", classes[c], "_summary.gctx"))
  
  #create phenotypic data
  pData <- data.frame(Chemical = connectivity@cid)
  rownames(pData) <- connectivity@cid
  phenoData <- new("AnnotatedDataFrame", data=pData)
  
  #create feature data
  fData <- data.frame(connectivity@rdesc)
  rownames(fData) <- connectivity@rdesc[["id"]]
  featureData <- new("AnnotatedDataFrame", data=fData)
  
  #create expression set
  expressionSet <- connectivity@mat
  
  eSet <- ExpressionSet(assayData=expressionSet, phenoData=phenoData, featureData=featureData)
  connectivity_map[[paste0(classes[c])]] <- eSet
  
}
  
dat[["Connectivity"]] <- connectivity_map
write_rds(dat, paste0("data/ADIPO/data.RDS"))
  