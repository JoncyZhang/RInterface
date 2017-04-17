DataCheck<-function(dataset){
  #############################################################################################
  ############################ data check #####################################################
  #Check dataset 
  if(is.null(dataset)){
    return(list(ErrorMsg = "Error in input data: data is null"))
  }
  
  # check is dataframe
  if(!is.data.frame(dataset)){
    return(list(ErrorMsg = "Error in input data: must be dataframe"))
  }
  
  # delete NA
  dataset = na.omit(dataset)
  
  #Check dataset ncol
  if(ncol(dataset)<2){
    return(list(ErrorMsg = paste("Error in data: exactly 3 columns allowed, you have", ncol(dataset))))
  }
  
  
  return(dataset)
}

FactorAnalysis<-function(dataset, xname = NULL, scale=TRUE, factors = NULL,
                         scores = "regression", rotation = 'varimax'){
  #-------------------------------------------------------------------------------------------------
  # File: FactorAnalysis.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-03-14
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # FactorAnalysis.R -  Perform Factor Analysis. 
  #                                        
  # To run this file, call it in FactorAnalysis.R 
  #############################################################################################
  dataset = DataCheck(dataset)
  #############################################################################################
  ######################################## parameters check ###################################
  # check xname
  if(is.null(xname)){
    xname = colnames(dataset)
  }else{
    xname = iconv(xname, to = 'gbk')
  }
  
  if(length(xname)<1){
    return(list(ErrorMsg = paste("Error in xname: at least 2 xname, you have", length(xname))))
  }
  
  if(!all(xname %in% colnames(dataset))){
    Missxname = xname[!(xname %in% colnames(dataset))]
    return(list(ErrorMsg = paste("Error in xname:", paste(Missxname, collapse = ' '), "not exist")))
  }
  
  # xname are required to be numeic
  for(i in xname){
    CharExitFlag = is.na(as.numeric(dataset[[i]]))
    if(any(CharExitFlag)){
      dataset[[i]] = as.numeric(as.factor(dataset[[i]]))
    }else{
      dataset[[i]]  = as.numeric(dataset[[i]])
    }
  }
  
  # check scale
  if(scale == TRUE){
    dataset = as.data.frame(scale(dataset))
  }
  #############################################################################################
  ################################# perform factor analysis ###################################
  # perform fact analysis
  ErrorMsg<-tryCatch({
    if(is.null(factors)){
      pca<-princomp(dataset[xname], cor=F)
      CumulativeVar  = cumsum((pca$sdev)^2)/sum((pca$sdev)^2)
      for(i in 1:length(CumulativeVar)){
        if(CumulativeVar[i]>=0.8){
          break
        }
      }
      factors = min(i, ceiling(length(xname)/2))
    }
    fact<-factanal(dataset[xname], factors = factors,  scores = "regression", rotation = "varimax")
    ErrorMsg = NULL  
  }, 
  error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R factanal function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # abstracting information from test result
  ErrorMsg<-tryCatch({
    LoadResult = unclass(fact$loadings)
    LoadResultRowName = rownames(LoadResult) 
    LoadResultColName = colnames(LoadResult)
    
    p <- nrow(LoadResult)
    vx <- colSums(LoadResult^2)
    VarResult = rbind(`SS loadings` = vx)
    VarResult = rbind(VarResult, `Proportion Var` = vx/p)
    VarResult = rbind(VarResult, `Cumulative Var` = cumsum(vx/p))
    
    VarResultRowName = rownames(VarResult) 
    VarResultColName = colnames(VarResult)
    
    LoadResult = round(LoadResult,digits = 3L)
    VarResult  = round(VarResult,digits = 3L)
    
    ErrorMsg = NULL
  }, 
  error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in abstracting information:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # return
  return(list( LoadResultRowName = LoadResultRowName, LoadResultColName = LoadResultColName, 
                 LoadResult = LoadResult, VarResultRowName = VarResultRowName,
               VarResultColName = VarResultColName, VarResult = VarResult))
}
                       

#codes below are testing codes
# rm(list=ls(all=TRUE))
# String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
# setwd(String)
# data(USArrests)
# dataset = USArrests
# dataset$add1 = rnorm(nrow(dataset), mean = 10, sd = 2)
# dataset$add2 = rnorm(nrow(dataset), mean = 11, sd = 3)
# xname = c("Murder", "Assault", "UrbanPop","Rape", "add1","add2")
# a = FactorAnalysis(dataset, xname = xname)
