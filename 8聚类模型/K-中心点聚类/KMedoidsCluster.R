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

KMedoidsCluster<-function(dataset, rowname = NULL, colname = NULL, culstervar = NULL, 
                          scale = TRUE, centers=3,  metric = "euclidean"){
  
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
  #K-Medoids Cluster
  ErrorMsg<-tryCatch({
    library(cluster)
    clusterset = dataset[,culstervar]
    ClusterResult = pam(clusterset, k = centers, diss = FALSE, metric =  metric, stand = F)
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
  
  ClusterLabel = as.matrix(sort(ClusterResult$clustering))
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
rm(list=ls(all=TRUE))
String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
setwd(String)
data = read.csv('datacon.csv',stringsAsFactors=F, na.strings = c(""))
dataset = data
rowname = NULL
colname = NULL
culstervar = c('pat_sex','pat_age','dp_nervus')
centers = 3
metric = "euclidean"


a = KMedoidsCluster(dataset, culstervar = culstervar,  
                    scale = T,centers = 3, metric = "euclidean")
                   


