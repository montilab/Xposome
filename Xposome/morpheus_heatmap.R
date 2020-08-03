
##CREATE JSON FILES#####
CreateJSON <- function(outputpath, dataset, genesetname, ge, proann, feature, nr, nc, marker){
  
  #Clean up na in expresssion set
  es_na <- c(which(is.na(ge)), which(is.nan(ge)))
  ge[es_na] <- "null";
  
  #Clean up na in character and numeric string in profile data
  character_cols <- sapply(colnames(proann), function(x){ !is.numeric(proann[,x]) })
  character_cols <- names(which(character_cols))
  numeric_cols <- sapply(colnames(proann), function(x){ is.numeric(proann[,x]) })
  numeric_cols <- names(which(numeric_cols))
  
  #Character string
  if(length(character_cols) > 0){
    for(c in character_cols){
      #c=3;
      dat <-  as.character(proann[,c])
      na <- which(is.na(dat))
      
      if(length(na) > 0){
        dat[na] <- "N/A"
      }
      
      l <- grep("\n", dat, fixed = TRUE, useBytes = TRUE)
      
      if(length(l) > 0){
        for(i in l){
          dat[i] <- gsub("\n", " ", dat[i], fixed=TRUE, useBytes = TRUE)
        }
      }
      proann[,c] <- dat
    }
  }
  
  #Round numeric to 3 digits 
  if(length(numeric_cols) > 0){
    for(l in numeric_cols){
      #l=2;
      dat <- round(proann[,l], 3)
      proann[,l] <- dat
    }
  }
  
  #Clean up na in character and numeric string in feature data
  character_cols <- sapply(colnames(feature), function(x){ !is.numeric(feature[,x]) })
  character_cols <- names(which(character_cols))
  numeric_cols <- sapply(colnames(feature), function(x){ is.numeric(feature[,x]) })
  numeric_cols <- names(which(numeric_cols))
  
  #Character string
  if(length(character_cols) > 0){
    for(c in character_cols){
      #c=3;
      dat <-  as.character(feature[,c])
      na <- which(is.na(dat))
      
      if(length(na) > 0){
        dat[na] <- "N/A"
      }
      
      l <- grep("\n", dat, fixed = TRUE, useBytes = TRUE)
      
      if(length(l) > 0){
        for(i in l){
          dat[i] <- gsub("\n", " ", dat[i], fixed=TRUE, useBytes = TRUE)
        }
      }
      feature[,c] <- dat
    }
  }
  
  #Round numeric to 3 digits 
  if(length(numeric_cols) > 0){
    for(l in numeric_cols){
      #l=2;
      dat <- round(feature[,l], 3)
      feature[,l] <- dat
    }
  }
  
  #Get which marker is selected;
  if(marker=="Gene Sets"){
    markername <- "Geneset"
  }else if(marker=="Genes"){
    markername <- "Gene"
  }else if(marker=="Landmark"){
    markername <- "Landmark_Gene"
  }else if(marker=="Connectivity"){
    markername <- "Connectivity_Id"
  }
  
  #Output the json file
  sink(outputpath)
  cat(paste0('{', '\n'))
  cat('\t', paste0('"rows": ', nr, ',\n'))
  cat('\t', paste0('"columns": ', nc, ',\n'))
  cat('\t', paste0('"seriesArrays": [[', '\n'))
  series <- NULL;
  for(j in 1:nr){
    #j=1;
    series <- c(series, paste0('[', paste0(ge[j,], collapse = ', '), ']', collapse = ''))
  }
  cat(paste0('\t\t', series, collapse = ',\n'), '\n')
  cat('\t', "]],", "\n")
  cat('\t', paste0('"seriesDataTypes": ["Float32"],', '\n'))
  cat('\t', paste0('"seriesNames": ', '["', genesetname, '"],', '\n'))
  cat('\t', paste0('"rowMetadataModel": {', '\n'))
  cat('\t', '\t', paste0('"vectors": [', '\n'))
  featurevar <- colnames(feature)
  rowseries <- NULL
  for(v in 1:length(featurevar)){
    #v=1;
    dat <- feature[,featurevar[v]]
    
    if(class(dat) != "numeric"){
      rowseries <- c(
        rowseries, 
        paste0(
          paste0('\t\t\t{', '\n'),
          paste0('\t\t\t\t"name": ', '"', featurevar[v], '"', ',', '\n'),
          paste0('\t\t\t\t"array": [', paste0(paste0('"', dat, '"'), collapse=", "), ']', '\n'),
          paste0('\t\t\t}'),
          collapse=""
        )
      )
    }else{
      
      na <- which(is.na(dat))
      
      if(length(na) > 0){
        dat[na] <- "null"
      }
      
      rowseries <- c(
        rowseries, 
        paste0(
          paste0('\t\t\t{', '\n'),
          paste0('\t\t\t\t"name": ', '"', featurevar[v], '"', ',', '\n'),
          paste0('\t\t\t\t"array": [', paste0(dat, collapse=", "), ']', '\n'),
          paste0('\t\t\t}'),
          collapse=""
        )
      )
    }
  }
  cat(paste0(rowseries, collapse = ",\n"), '\n')
  cat('\t', '\t', paste0(']', '\n'))
  cat('\t', paste0('},', '\n'))
  cat('\t', paste0('"columnMetadataModel": {', '\n'))
  cat('\t', '\t', paste0('"vectors": [', '\n'))
  phenovar <- colnames(proann)
  columnseries <- NULL
  for(v in 1:length(phenovar)){
    #v=1;
    dat <- proann[,phenovar[v]]
    
    if(class(dat) != "numeric"){
      columnseries <- c(
        columnseries, 
        paste0(
          paste0('\t\t\t{', '\n'),
          paste0('\t\t\t\t"name": ', '"', phenovar[v], '"', ',', '\n'),
          paste0('\t\t\t\t"array": [', paste0(paste0('"', dat, '"'), collapse=", "), ']', '\n'),
          paste0('\t\t\t}'),
          collapse=""
        )
      )
    }else{
      
      na <- which(is.na(dat))
      
      if(length(na) > 0){
        dat[na] <- "null"
      }
      
      columnseries <- c(
        columnseries, 
        paste0(
          paste0('\t\t\t{', '\n'),
          paste0('\t\t\t\t"name": ', '"', phenovar[v], '"', ',', '\n'),
          paste0('\t\t\t\t"array": [', paste0(dat, collapse=", "), ']', '\n'),
          paste0('\t\t\t}'),
          collapse=""
        )
      )
    }
  }
  cat(paste0(columnseries, collapse = ",\n"), '\n')
  cat('\t', '\t', paste0(']', '\n'))
  cat('\t', paste0('}', '\n'))
  cat("}", sep="")
  sink()
  
}

##CREATE HTML FILES#####
CreateHTML <- function(outputpath, dataset, genesetname, proann, feature, marker, cluster=FALSE){
  
  #Create column annotation####
  columnseries <- NULL; phenovar <- colnames(proann);
  n <- ifelse(length(phenovar) >= 20, 20, length(phenovar));
  
  for(v in 1:n){
    #v=1;
    columnseries <- c(
      columnseries, 
      paste0(
        paste0('\t\t\t\t{', '\n'),
        paste0('\t\t\t\t\tfield: ', '"', phenovar[v], '"', ', display: ["color"]', '\n'),
        paste0('\t\t\t\t}'),
        collapse=""
      )
    )
  }
  
  #Create row annotation####
  rowseries <- NULL; feature_var <- colnames(feature)
  n <- ifelse(length(feature_var) >= 3, 3, length(feature_var));
  
  for(v in 1:n){
    #v=1;
    rowseries <- c(
      rowseries, 
      paste0(
        paste0('\t\t\t\t{', '\n'),
        paste0('\t\t\t\t\tfield: ', '"', feature_var[v], '"', ', display: ["text"]', '\n'),
        paste0('\t\t\t\t}'),
        collapse=""
      )
    )
  }
  
  #Get which marker is selected;
  if(marker=="Gene Sets"){
    markername <- "Geneset"
  }else if(marker=="Genes"){
    markername <- "Gene"
  }else if(marker=="Landmark"){
    markername <- "Landmark_Gene"
  }else if(marker=="Connectivity"){
    markername <- "Connectivity_Id"
  }
  
  #Determine whether to include cluster
  if(cluster){
    clustermethod <- paste0(
      '\t\t\tname: "Hierarchical Clustering",', '\n',
      '\t\t\tparams: {cluster: "Rows and columns"}', '\n',
      collapse = ""
    )
  }else{
    clustermethod <- paste0(
      '\t\t\t', '\n', 
      '\t\t\t', '\n',
      collapse=""
    )
  }
  
  #Output the html file
  sink(outputpath)
  cat(
    '<!DOCTYPE html>', '\n',
    '<html>', '\n',
    '<head>', '\n',
    '\t', '<meta http-equiv="Content-Type" content="text/html;charset=utf-8">', '\n',
    '\t', '<meta http-equiv="X-UA-Compatible" content="IE=edge">', '\n',
    '\t', '<meta name="viewport" content="user-scalable=no, width=device-width, initial-scale=1, maximum-scale=1">', '\n',
    '\t', '<title>Morpheus</title>', '\n',
    '\t', '<link rel="stylesheet" href="https://software.broadinstitute.org/morpheus/css/morpheus-latest.min.css">', '\n',
    '\t', '<link rel="shortcut icon" href="https://software.broadinstitute.org/morpheus/favicon.ico" type="image/x-icon">', '\n',
    '\t', '<script type="text/javascript" src="https://software.broadinstitute.org/morpheus/js/morpheus-external-latest.min.js"></script>', '\n',
    '\t', '<script src="https://software.broadinstitute.org/morpheus/js/morpheus-latest.min.js"></script>', '\n',
    '</head>', '\n',
    '<body>', '\n',
    '<noscript>', '\n',
    '<p>Please enable JavaScript</p>', '\n',
    '</noscript>', '\n',
    '<div style="width: 100%; padding: 20px 20px 20px 20px;" id="vis"></div>', '\n',
    '<script type="text/javascript">', '\n',
    '\t', 'function xhrSuccess() {', '\n',
    '\t', '\t', 'this.callback.apply(this, this.arguments);', '\n',
    '\t', '}', '\n',
    '\t', 'function xhrError() {', '\n', 
    '\t', '\t', 'console(this.statusText);', '\n', 
    '\t', '}', '\n',
    '\t', 'function loadFile(url, callback) {', '\n', 
    '\t', '\t', 'var xhr = new XMLHttpRequest();', '\n', 
    '\t', '\t', 'xhr.callback = callback;', '\n', 
    '\t', '\t', 'xhr.arguments = Array.prototype.slice.call(arguments, 2);', '\n', 
    '\t', '\t', 'xhr.onload = xhrSuccess;', '\n', 
    '\t', '\t', 'xhr.onerror = xhrError;', '\n', 
    '\t', '\t', 'xhr.open("GET", url, true);', '\n', 
    '\t', '\t', 'xhr.send(null);', '\n', 
    '\t', '}', '\n', 
    '\t', 'function showHeatmap() {', '\n', 
    '\t', '\t', 'var json = JSON.parse(this.responseText);', '\n', 
    '\t', '\t', 'new morpheus.HeatMap({', '\n',
    '\t', '\t', '\t', 'el: $("#vis"),', '\n',
    '\t', '\t', '\t', 'dataset: morpheus.Dataset.fromJSON(json),', '\n',
    '\t', '\t', '\t', 'columns: [', '\n',
    paste0(columnseries, collapse = ",\n"), '\n',
    '\t', '\t', '\t', '],', '\n',
    '\t', '\t', '\t', 'rows: [ ', '\n',
    paste0(rowseries, collapse = ",\n"), '\n',
    '\t', '\t', '\t', '],', '\n',
    '\t', '\t', '\t', 'columnFilter: {', '\n',
    '\t', '\t', '\t', '\t', 'filters : [{', '\n',
    '\t', '\t', '\t', '\t', '\t', 'field : "TAS",', '\n',
    '\t', '\t', '\t', '\t', '\t', 'type : "range",', '\n',
    '\t', '\t', '\t', '\t', '\t', 'min : 0,', '\n',
    '\t', '\t', '\t', '\t', '\t', 'max : 1.0', '\n',
    '\t', '\t', '\t', '\t', '}]', '\n',
    '\t', '\t', '\t', '},', '\n',
    '\t', '\t', '\t', 'tools: [{', '\n',
    '\t', clustermethod,
    '\t', '\t', '\t', '}]', '\n',
    '\t', '\t', '});', '\n',
    '\t', "}", "\n",
    paste0('loadFile("', genesetname, '.json", showHeatmap);'), '\n',
    '</script>', '\n',
    '</body>', '\n',
    '</html>',
    sep=""
  )
  sink()
  
}
