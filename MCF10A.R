

#Read in the reactive data###
dat %<-% value(future(readRDS(paste0("data/MCF10A/data.RDS")))) 

#Run the app###
source("main_app.R", local=TRUE)