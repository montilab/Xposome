TAS_Modzscores_Calculation <- function (pro_ann, chem_ann, gene_expression)
{
  # Determine the column variable name to use
  var <- ifelse(
    all(colnames(gene_expression) %in% pro_ann$Sig_Id), "Sig_Id", "Chemical_Id"
  )
  # Define a data frame to store number of replicates and moderated z-scores
  TAS_output <- data.frame(
    Chemical_Id = unique(as.character(chem_ann$Chemical_Id)),
    TAS = NA,
    stringsAsFactors = FALSE
  )
  # Tabulate the number of replicates for each chemical
  if (var=="Sig_Id") {
    TAS_output$Number_of_Replicates <-
      as.integer(table(pro_ann$Chemical_Id)[TAS_output$Chemical_Id])
  } else {
    TAS_output$Number_of_Replicates <-
      as.integer(table(chem_ann$Chemical_Id)[TAS_output$Chemical_Id])
  }

  # Initialize a matrix of moderated z-scores
  modzscores <- matrix(
    NA, nrow=nrow(gene_expression), ncol=nrow(TAS_output),
    byrow=TRUE,
    dimnames=list(rownames(gene_expression), TAS_output$Chemical_Id)
  )

  for (i in 1:nrow(TAS_output)) {
    if (var=="Sig_Id") {
      rep <- pro_ann$Sig_Id[pro_ann$Chemical_Id == TAS_output$Chemical_Id[i]]
    } else {
      rep <- TAS_output$Chemical_Id[i]
    }
    # The 'Chemical_Id' field is unfortunately stored as a factor,
    # so it is coerced to a character variable
    rep <- as.character(rep)
    if (length(rep) == 1) {
      CC <- 1
      w <- 1
      # Calculate the moderated z-score for the chemical
      modzscores[,i] <- w * gene_expression[,rep]
    } else {
      # Calculate the Spearman correlation between the pairwise replicates
      rho <- cor(gene_expression[, rep], method="spearman")
      # Calculate the mod z-scores
      if (length(rep) == 2) {
        w <- 0.5
        # Calculate the moderated z-score for the chemical
        modzscores[,i] <- rowSums(w * gene_expression[,rep])
      } else {
        # Compute the weights
        rep_corr <- abs(rowSums(rho[rep,], na.rm=TRUE)-1)
        w <- rep_corr / sum(rep_corr, na.rm=TRUE)
        if (1-sum(w, na.rm=TRUE) > 0.001) {
          print(rep_corr)
          print(w)
          print('incorrect calculation of w!')
        }
        # Calculate the moderated z-score for the chemical
        modzscores[, i] <- rowSums(
          sweep(gene_expression[,rep], MARGIN=2, STATS=w, FUN="*")
        )
      }
      # Calculate the CC
      CC <- quantile(rho[lower.tri(rho)], 0.75, na.rm=TRUE)
    }
    SS <- length(which(abs(modzscores[,i]) >= 2))
    TAS_output$TAS[i] <- sqrt(SS * max(c(0,CC)) / nrow(modzscores))
  }
  TAS_output <- TAS_output[, c("Chemical_Id", "TAS", "Number_of_Replicates")]
  return(list(TAS=TAS_output, Modzscores=modzscores))
}
