

output$portal_main_page <- renderUI({
  
  req(input$portal_id, input$main_page, input$chemical_tab)
  
  fname <- input$portal_id; main_page <- input$main_page; chemical_tab <- input$chemical_tab;

  #get input options####
  geneset_collection <- projectlist$GS_Collection[which(projectlist$Portal == fname)]
  gs_collection(geneset_collection)
  
  geneset_collection_link <- projectlist$GS_Collection_Link[which(projectlist$Portal == fname)]
  gs_collection_link(geneset_collection_link)
  
  phenotype_test <- unlist(strsplit(as.character(projectlist$Exposure_Phenotype_Test[which(projectlist$Portal == fname)]), ",", fixed=TRUE)) %>% trimws()
  exposure_phenotype_test(phenotype_test)
  
  phenotype <- unlist(strsplit(as.character(projectlist$Exposure_Phenotype[which(projectlist$Portal == fname)]), ",", fixed=TRUE)) %>% trimws()
  exposure_phenotype(phenotype)
  
  #get landmark option
  landmark(projectlist$Landmark_Gene[which(projectlist$Portal == fname)])

  ##Getting the helpext for different gene set enrichment
  if(geneset_collection %in% "Default"){
    
    ##the default gs enrichment version####
    gs_enrichment_version <- 7
    
    ##Getting the gene set scores for diffrent gsva methods
    gs_enrichment_list <- list(
      Hallmark=paste0("gsscores_h.all.v", gs_enrichment_version, ".0"),
      C2=paste0("gsscores_c2.cp.reactome.v", gs_enrichment_version, ".0"),
      NURSA=paste0("gsscores_nursa_consensome_Cbyfdrvalue_0.01")
    )
    
    dsmap(gs_enrichment_list)
    
    ##Getting the helptext####
    helptext_geneset(
      paste0(
        "<a href=\"https://www.gsea-msigdb.org/gsea/msigdb\">MSigDB Hallmark Pathways (v", gs_enrichment_version, ")</a><br>",
        "<a href=\"https://www.gsea-msigdb.org/gsea/msigdb\">MSigDB C2 reactome Pathways (v", gs_enrichment_version, ")</a><br>",
        "<a href=\"https://signalingpathways.org\">NURSA: Nuclear Receptor Signaling Atlas, consensome data for human</a><br>"
      )
    )
    
  }else{
    
    ##Getting the gene set scores for diffrent gsva methods
    gs_enrichment_list <- paste0("gsscores_", geneset_collection)
    names(gs_enrichment_list) <- paste0("gsscores_", geneset_collection)
    
    dsmap(gs_enrichment_list)
    
    ##Getting the helptext####
    helptext_geneset(paste0("<a href=\"", geneset_collection_link, "\">", geneset_collection, "</a><br>"))
    
  }
  
  tabsetPanel(
    id="main_page", type="pills", selected=main_page, 
    
    ###Annotation#####
    tabPanel(
      title="Annotation", value="annotation",
      #"Hello World!"
      source("ui_annotation.R", local=TRUE)$value
    ),
    
    ###Chemical Explorer#####
    tabPanel(
      title = "Chemical Explorer", value="chemical_explorer",
      #"Hello World!"
      source("ui_chemical.R", local=TRUE)$value
    ),
    
    ###Marker Explorer####
    tabPanel(
      title = "Marker Explorer", value="marker_explorer",
      #"Hello World!"
      source("ui_marker.R", local=TRUE)$value
    ),
    
    ###Heatmap Explorer####
    tabPanel(
      title = "Heatmap Explorer", value="heatmap_explorer",
      #"Hello World!"
      source("ui_heatmap.R", local=TRUE)$value
    ),
    
    ###Taxonomic Clustering####
    tabPanel(
      title = "K2 Taxonomer Results", value="k2_taxonomer_results",
      #"Hello World!"
      source("ui_taxonomic_clustering.R", local=TRUE)$value
    ),
    
    tabPanel(
      title = "Compare Multiple", value="compare_multiple",
      #"Hello World!"
      source("ui_compare_multiple.R", local=TRUE)$value
    )
  )

})