

#Read in the reactive data###
dat %<-% value(future(readRDS(paste0("data/ADIPO/data.RDS")))) 

#Run the app###
source("main_app.R", local=TRUE)