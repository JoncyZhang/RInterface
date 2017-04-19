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
        rowname = iconv(rowname,to = 'gbk')
        rownames(dataset) = rowname
      }
      if(!is.null(colname)){
        colname = iconv(colname,to = 'gbk')
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

KMeansCluster<-function(dataset,rowname = NULL, colname = NULL,culstervar = NULL, 
                        scale = TRUE, centers = 3, algorithm = "Hartigan-Wong"){
  #-------------------------------------------------------------------------------------------------
  # File: KMeansCluster.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-03-14
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # KMeansCluster.R -  Perform K-Means Cluster of dataset. 
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
    culstervar = iconv(culstervar, to = 'gbk')
  }
  
  if(length(culstervar)<1){
    return(list(ErrorMsg = paste("Error in culstervar: at least 2 culstervar, you have", length(culstervar))))
  }
  
  if(!all(culstervar %in% colnames(dataset))){
    Missxname = culstervar[!(culstervar %in% colnames(dataset))]
    return(list(ErrorMsg = paste("Error in culstervar:", paste(Missxname, collapse = ' '), "not exist")))
  }
  
  # culstervar are required to be numeic
  for(i in culstervar){
    CharExitFlag = is.na(as.numeric(dataset[[i]]))
    if(any(CharExitFlag)){
      dataset[[i]] = as.numeric(as.factor(dataset[[i]]))
    }else{
      dataset[[i]]  = as.numeric(dataset[[i]])
    }
  }
  
  # check scale
  if(scale == TRUE){
    dataset = as.data.frame(scale(dataset[culstervar]))
  }
  #############################################################################################
  ###################################### perform cluster ###################################
  #K-Means Cluster
  ErrorMsg<-tryCatch({
    
    clusterset = dataset[,culstervar]
    KMeansResult = kmeans(clusterset, centers = centers, algorithm = algorithm )
    
    ErrorMsg = NULL
  }, 
  error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in kmeans:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }  
  
  # abstracting information from test result
  ClusterCenter = as.matrix(KMeansResult$centers)
  ClusterCenterRowName = rownames(ClusterCenter)
  ClusterCenterColName = colnames(ClusterCenter)
  
  ClusterLabel = as.matrix(sort(KMeansResult$cluster))
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


#codes below are testing codes
# rm(list=ls(all=TRUE))
# String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
# setwd(String)
# data = read.csv('datacon.csv',stringsAsFactors=F, na.strings = c(""))
# dataset = data
# rowname = NULL
# colname = NULL
# culstervar = c('pat_sex','pat_age','dp_diff','dp_nervus')
# scale = TRUE
# centers = 3
# algorithm = "Hartigan-Wong"
# 
# a = KMeansCluster(dataset, culstervar = culstervar,  centers = 3)
# 


