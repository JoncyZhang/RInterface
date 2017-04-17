DescriptiveAnalysis<-function(datase){
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
  
  # check is dataframe
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
  b = lapply(dataset, summary, na.rm=FALSE)  #####此处把sapply 改为 lapply
  
  #############################################################################################
  ################################# abstract information  #####################################
  #return results
  Result = list()
  for(i in 1:length(b)){
    if(is.factor(dataset[[i]])){
      b[[i]] = as.matrix(b[[i]])
      colname = c("frequenct","precentage")
      rowname = rownames(b[[i]])
      
      pct = round(b[[i]]/sum(b[[i]]), digits = 2)*100
      b[[i]] = cbind(b[[i]], pct)
      colnames(b[[i]]) = colname
    }else{
      b[[i]] = round(t(as.matrix(b[[i]])), digits = 2)
      colname = colnames(b[[i]])
      rowname = names(b[i])
    }
    
    RowName = paste(names(b[i]), "RowName", sep = '')
    ColName = paste(names(b[i]), "ColName", sep = '')
    DscpName = paste(names(b[i]), "Dscp", sep = '')
    
    eval(substitute(Result[[RowName]] <- rowname, list(RowName = RowName)))
    eval(substitute(Result[[ColName]] <- colname, list(ColName = ColName)))
    eval(substitute(Result[[DscpName]] <- b[[i]], list(DscpName = DscpName)))

  }
  
  return(Result)
  
} 

# codes below are testing codes
# rm(list=ls(all=TRUE))
# String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
# setwd(String)
# d = read.csv('datacon.csv',stringsAsFactors=F, na.strings = c(""))
# dataset = d
# 
# a = DescriptiveAnalysis(dataset)
# 





