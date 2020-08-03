
TAS_Modzscores_Calculation <- function(pro_ann, chem_ann, gene_expression){

  #Get the column variable of gene expression####
  var <- ifelse(all(colnames(gene_expression) %in% pro_ann$Sig_Id), "Sig_Id", "Chemical_Id")
  
  #print(var)
  
  # Define a  data frame to store number of replicates and ModZscores
  chem_replicate <- data.frame(Chemical_Id = unique(chem_ann$Chemical_Id), Number_of_Replicates = NA, CC = NA, SS=NA, TAS = NA, stringsAsFactors = FALSE)
  
  #print("hello")
  
  # Create the modzscore expression set
  ModZ_Expression_Set <- matrix(NA, nrow=nrow(gene_expression), ncol=nrow(chem_replicate), byrow=T, dimnames=list(rownames(gene_expression), chem_replicate$Chemical_Id))
  
  # Getting the number of replicates for each chemical
  for(i in 1:nrow(chem_replicate)){
    #i=1;
    #print(i)
    if(var=="Sig_Id"){
      chem_replicate$Number_of_Replicates[i] <- nrow(pro_ann %>% filter(Chemical_Id %in% chem_replicate$Chemical_Id[i]))
      rep <- pro_ann$Sig_Id[which(pro_ann$Chemical_Id %in% chem_replicate$Chemical_Id[i])]
    }else{
      chem_replicate$Number_of_Replicates[i] <- nrow(chem_ann %>% filter(Chemical_Id %in% chem_replicate$Chemical_Id[i]))
      rep <- chem_ann$Chemical_Id[which(chem_ann$Chemical_Id %in% chem_replicate$Chemical_Id[i])]
    } 
    
    if(length(rep) == 1){
      
      chem_replicate$CC[i] <- 1
      
      w=1 #unweighted
      
      #Calculate the modzscore for the chemical
      modzscore <-  w*gene_expression[,as.character(rep)]
      
      ModZ_Expression_Set[,chem_replicate$Chemical_Id[i]] <- modzscore
      
    }else {
      
      # Calculate the spearman correlation between the pairwise replicates
      spearman_corr <- matrix(NA, nrow=length(rep), ncol=length(rep), byrow=T, dimnames=list(rep, rep))
      
      for(l in 1:nrow(spearman_corr)){
        for(k in 1:nrow(spearman_corr)){
          #l=1, k=1;
          spearman_corr[l,k] <- cor(gene_expression[,as.character(rep[l])], gene_expression[,as.character(rep[k])], method = c("spearman"))
        }
      }
      
      #CALCULATE THE MODZ-SCORES
      if(length(rep) == 2){
        
        w=0.5 #unweighted
        
        #Calculate the modzscore for the chemical
        modzscore <- 0
        
        for(m in 1:length(rep)){
          #m=2;
          modzscore <- w*gene_expression[,as.character(rep[m])] + modzscore
        }
        
        ModZ_Expression_Set[,chem_replicate$Chemical_Id[i]] <- modzscore
        
      }else{
        
        rep_corr <- NULL;
        
        for(g in 1:length(rep)){
          #g=1;
          corr <- spearman_corr[as.character(rep[g]),as.character(rep[which(!rep %in% rep[g])])]
          rep_corr <- c(rep_corr, abs(sum(corr, na.rm=T)))
        }
        
        w <- rep_corr/sum(rep_corr, na.rm=TRUE)
        
        if(1-sum(w, na.rm=T) > 0.001) {
          print(rep_corr)
          print(w)
          print('incorrect calculation of w!')
        }
        
        #Calculate the modzscore for the chemical
        modzscore <- 0
        
        for(m in 1:length(rep)){
          #m=2;
          modzscore  <- w[m]*gene_expression[,as.character(rep[m])] + modzscore
        }
        
        ModZ_Expression_Set[,chem_replicate$Chemical_Id[i]] <- modzscore
        
      }
      
      #CALCULATE THE CC
      corr <- spearman_corr[lower.tri(spearman_corr)]
      quan_75 <- quantile(corr, 0.75, na.rm = T)
      chem_replicate$CC[i] <- quan_75
      
    }
    
    chem_replicate$SS[i] = length(which(abs(ModZ_Expression_Set[,i]) >= 2))
    chem_replicate$TAS[i] = sqrt(chem_replicate$SS[i]*max(c(0,chem_replicate$CC[i]))/nrow(ModZ_Expression_Set))
    
  }
  
  TAS <- chem_replicate[, c("Chemical_Id", "TAS", "Number_of_Replicates")]
  
  return(list(TAS=TAS, Modzscores=ModZ_Expression_Set))
  
}

