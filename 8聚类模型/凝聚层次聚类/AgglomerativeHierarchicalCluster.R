DataCheck<-function(dataset, rowname = NULL, colname = NULL){
  #############################################################################################
  ############################ data check #####################################################
  #Check dataset 
  if(is.null(dataset)){
    return(list(ErrorMsg = "Error in input data: data is null"))
  }
  
  #Check rowname/colname
  ErrorMsg<-tryCatch({
    
    if(is.data.frame(dataset)){
      if(!is.null(rowname)){
        rownames(dataset) = rowname
      }
      if(!is.null(colname)){
        colnames(dataset) = colname
      }
    }
    ErrorMsg = NULL
  }, 
  error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in rowname/colname:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # check is dataframe
  if(!is.data.frame(dataset)){
    return(list(ErrorMsg = "Error in input data: must be dataframe"))
  }
  
  # delete NA
  dataset = na.omit(dataset)
  
  #Check dataset ncol
  if(ncol(dataset)<1){
    return(list(ErrorMsg = paste("Error in data: at least 2 columns allowed, you have", ncol(dataset))))
  }
  
  return(dataset)
}

AgglomerativeHierarchicalCluster<-function(dataset, rowname = NULL, colname = NULL, culstervar = NULL, 
                                           metric = "euclidean", method = "average", stand = FALSE,
                                           plotstr = NULL, bannername = NULL, treename = NULL){
  #-------------------------------------------------------------------------------------------------
  # File: AgglomerativeHierarchicalCluster.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-03-14
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # AgglomerativeHierarchicalCluster.R -  Perform K-Medoids Cluster of dataset. 
  #                                       1.culstervar is the names of columns for clustering  
  #                                       2.trueclass is the names of column representign ture class, 
  #                                         NUll usually in  unsupervised learning
  #                                       3.centers is the number of clusters
  #                                       4.metric is the distance taking value in ("euclidean", "manhattan").
  #                                       5.method is  clustering method taking value in ( "average","single",
  #                                         "complete", "ward", "weighted", ""gaverage"") 
  #                                       6.logical; if true, the measurements in x are standardized before 
  #                                         calculating the dissimilaritie.
  #                    
  #                                        
  # To run this file, call it in KMeansCluster.R 
  #############################################################################################
  dataset = DataCheck(dataset, rowname = rowname, colname = colname)
  #############################################################################################
  ######################################## parameters check ###################################
  # check culstervar 
  if(is.null(culstervar)){
    culstervar = colnames(dataset)
  }else{
    if(!all(culstervar %in% colnames(dataset))){
      Missxname = culstervar[!(culstervar %in% colnames(dataset))]
      return(list(ErrorMsg = paste("Error in culstervar:", paste(Missxname, collapse = ' '), "not exist")))
    }
  }
  
  # culstervar are required to be numeic
  ErrorMsg<-tryCatch({
    
    for(i in culstervar){
      if(!(i %in% colnames(dataset))){
        return(list(ErrorMsg = paste("Error in culstervar", i, ": not exist")))
      }else{
        CharExitFlag = grep('[^0-9]',dataset[[i]])
        if(length(CharExitFlag) >1){
          dataset[[i]] = as.numeric(as.factor(dataset[[i]]))
        }else{
          dataset[[i]]  = as.numeric(dataset[[i]])
        }
      }  
    }
    
    
    ErrorMsg = NULL
  }, 
  error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in converting culstervar:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }  
  
  #############################################################################################
  ###################################### perform cluster ###################################
  #Agglomerative Hierarchical Cluster
  ErrorMsg<-tryCatch({
    library(cluster)
    clusterset = dataset[,culstervar]
    ClusterResult = agnes(clusterset, diss = FALSE, metric =  metric, method = method, stand = stand)
    
    ErrorMsg = NULL
  }, 
  error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R agnes function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }  
  
  # abstracting information from test result
  ClusterOrder = as.matrix(ClusterResult$order)
  ClusterOrderRowName = rownames(dataset)
  ClusterOrderColName = "Order"
  colnames(ClusterOrder) = ClusterOrderColName
  
  ClusterHeight = as.matrix(ClusterResult$height)
  ClusterHeightRowName = rownames(dataset)
  ClusterHeightColName = "Height"
  colnames(ClusterHeight) = ClusterHeightColName
  
  #############################################################################################
  ########################################## plot #############################################
  ErrorMsg<-tryCatch({
     
    # bannerplot
    if(!is.null(plotstr) & !is.null(bannername)){
      filename = paste(plotstr, bannername, ".png", sep = '')
      png(file=filename, bg="white")
      sub = paste("Agglomerative Coefficient = ", round(ClusterResult$ac, digits = 2))
      adj = 0
      nmax.lab = 35
      max.strlen = 5
      xax.pretty = TRUE
      cluster::bannerplot(ClusterResult, fromLeft = TRUE, main = "Banner of of Agglomerative Hierarchical Cluster", 
                          sub = sub, adj = adj, xax.pretty = 10, nmax.lab = nmax.lab, 
                          max.strlen = max.strlen)
      dev.off()
    }

    
    # pltree
    if(!is.null(plotstr) & !is.null(treename)){
      filename = paste(plotstr, treename, ".png", sep = '')
      png(file=filename, bg="white", width = 1084, height = 780)
      sub = paste("Agglomerative Coefficient = ", round(ClusterResult$ac, digits = 2))
      cluster::pltree(ClusterResult, main = "Dendrogram of Agglomerative Hierarchical Cluster", sub = sub)
      dev.off()
    }

    ErrorMsg = NULL
    
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in plot:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  
  return(list(ClusterOrderRowName = ClusterOrderRowName, 
              ClusterOrderColName = ClusterOrderColName, 
              ClusterOrder = ClusterOrder,
              ClusterHeightRowName = ClusterHeightRowName,
              ClusterHeightColName = ClusterHeightColName,
              ClusterHeight = ClusterHeight))
}


#codes below are testing codes
rm(list=ls(all=TRUE))
String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
setwd(String)
data = read.csv('datacon.csv',stringsAsFactors=F, na.strings = c(""))
dataset = data
culstervar = c('pat_sex','pat_age','dp_diff','dp_nervus')
centers = 3
metric = "euclidean"
method = "average"
stand = TRUE
plotstr = String
bannername = 'MyBannerPlot'
treename = 'MyDendrogramPlot'

a = AgglomerativeHierarchicalCluster(dataset, culstervar = culstervar,
                  plotstr = plotstr, bannername = bannername, treename =treename)


