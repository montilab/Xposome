

#Read in the reactive data###
dat %<-% value(future(readRDS(paste0("data/HEPG2/data.RDS")))) 

#Run the app###
source("main_app.R", local=TRUE)