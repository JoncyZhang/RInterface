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

KMedoidsCluster<-function(dataset, rowname = NULL, colname = NULL, culstervar = NULL, centers=3, 
                          metric = "euclidean", stand = FALSE){

  #-------------------------------------------------------------------------------------------------
  # File: KMedoidsCluster.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-03-14
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # KMedoidsCluster.R -  Perform K-Medoids Cluster of dataset. 
  #                      1.culstervar is the names of columns for clustering  
  #                      2.trueclass is the names of column representign ture class, NUll usually in 
  #                        unsupervised learning
  #                      3.centers is the number of clusters
  #                      4.metric is the distance taking value in ("euclidean", "manhattan").
  #                      5.logical; if true, the measurements in x are standardized before 
  #                        calculating the dissimilaritie.
                            
  #                    
  #                                        
  # To run this file, call it in KMedoidsCluster.R 
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
  #K-Medoids Cluster
  ErrorMsg<-tryCatch({
    library(cluster)
    clusterset = dataset[,culstervar]
    ClusterResult = pam(clusterset, k = centers, diss = FALSE, metric =  metric, stand = stand)
    ErrorMsg = NULL
  }, 
  error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R pam function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }  
  
  # abstracting information from test result
  ClusterCenter = as.matrix(ClusterResult$medoids)
  ClusterCenterRowName = rownames(ClusterCenter)
  ClusterCenterColName = colnames(ClusterCenter)
  
  ClusterLabel = as.matrix(ClusterResult$clustering)
  ClusterLabelRowName = rownames(ClusterLabel)
  ClusterLabelColName = "Label"
  colnames(ClusterLabel) = ClusterLabelColName

  return(list(ClusterCenterRowName = ClusterCenterRowName, 
              ClusterCenterColName = ClusterCenterColName, 
              ClusterCenter = ClusterCenter,
              ClusterLabelRowName = ClusterLabelRowName,
              ClusterLabelColName = ClusterLabelColName,
              ClusterLabel = ClusterLabel))
}


# codes below are testing codes
# rm(list=ls(all=TRUE))
# String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
# setwd(String)
# data = read.csv('datacon.csv',stringsAsFactors=F, na.strings = c(""))
# dataset = data
# culstervar = c('pat_sex','pat_age','dp_nervus')
# centers = 3
# metric = "euclidean"
# stand = FALSE
# 
# a = KMedoidsCluster(dataset, culstervar = culstervar,  centers = 3,
#                     metric = "euclidean", stand = FALSE)


