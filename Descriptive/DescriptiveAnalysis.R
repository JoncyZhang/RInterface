DescriptiveAanlysis<-function(dataset, rowname = NULL, colname = NULL){
  #-------------------------------------------------------------------------------------------------
  # File: DescriptiveAanlysis.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-02-28
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # DescriptiveAanlysis.R -  Give the simple descriptive analysis of dataset. 
  #                          1. Here the dataset are required to be a vector(like c(1,2,3,4...))
  #                             ,matrix or dataframe. Eventually matrix will be converted into dataframe
  #                             before computing.
  #                          2. rowname and colname will be updated (if given) for dataset
  #                                 
  #                                        
  # To run this file, call it in DescriptiveAanlysis.R 
  #############################################################################################
  ########################################## data check #######################################
  #Check dataset completeness
  if(is.null(dataset)){
    return(list(ErrorMsg = "Error in input data: data is null"))
  }
  
  #Check rowname/colname
  ErrorMsg<-tryCatch({
    if(is.matrix(dataset)){
      if(!is.null(rowname)){
        rownames(dataset) = rowname
      }
      if(!is.null(colname)){
        colnames(dataset) = colname
      }
      dataset = as.data.frame(dataset)
    }
    
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
  
  
  # check data again
  if(!is.data.frame(dataset)){
    return(list(ErrorMsg = "Error in input data: must be dataframe"))
  }
  
  #############################################################################################
  ######################################## parameters check ###################################
  # All columns of character type will be converted into factor type
  for(i in 1:ncol(dataset)){
    if(is.character(dataset[[i]])){
        dataset[[i]] =  as.factor(dataset[[i]])
    }
  }
  
  
  #############################################################################################
  ######################################### summary  ##########################################
  DespResult = summary(dataset)
  #return results
  
  DespResultRowName = rownames(DespResult)
  DespResultColName = colnames(DespResult)
  
  return(list(DespResultRowName = DespResultRowName, DespResultColName = DespResultColName, DespResult = DespResult ))
  
} 

# codes below are testing codes
# rm(list=ls(all=TRUE))
# String = "E:/WorkSpace/Rstudio/Deepaint/"
# setwd(String)
# d = read.csv('data.csv',stringsAsFactors=F, na.strings = c(""))
# dataset = d
# 
# a = DescriptiveAanlysis(dataset)
# 

