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
  if(ncol(dataset)<1){
    return(list(ErrorMsg = paste("Error in data: exactly 2 columns allowed, you have", ncol(dataset))))
  }
  
  
  return(dataset)
}

Sig<-function(v){
  v = as.numeric(v)
  significance = c()
  for(i in v){
    if(is.na(i)){
      significance = c(significance, NA)
      next
    }
    
    if(i<=0.01){
      significance = c(significance, '***')
      next
    }
    
    if(i>0.01 & i<=0.05){
      significance = c(significance, '**')
      next
    }
    
    if(i>0.05 & i<=0.1){
      significance = c(significance, '*')
      next
    }
    if(i>0.1){
      significance = c(significance, " ")
      next
    }
    
  }
  return(significance)
}

MultiNormLogistic<-function(dataset,yname=NULL, xname=NULL, 
                            formulastring=NULL){
  #-------------------------------------------------------------------------------------------------
  # File: MultiNormLogistic.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-03-01
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # BinaryLogistic.R -  Perform Multinominal Logistic Regression of dataset 
  #                 1. dependent variable is a binary variable. 
  #  
  #
  # To run this file, call it in MultiNormLogistic.R 
  
  #############################################################################################
  dataset = DataCheck(dataset)
  #############################################################################################
  ######################################## parameters check ###################################
  # check yname
  if(!(yname %in% colnames(dataset))){
    return(list(ErrorMsg = paste("Error in response variable", yname, ": not exist")))
  }else{
    dataset[[yname]] = as.factor(dataset[[yname]])
  }
  ResLeves = levels(dataset[[yname]])
  if(length(ResLeves) < 2){
    return(list(ErrorMsg = paste("Error in response variable", yname, ":at least 2 values are required after omitting na, you have", paste(ResLeves, collapse = ' '))))
  }
  
  # check xname
  if(!all(xname %in% colnames(dataset))){
    Missxname = xname[!(xname %in% colnames(dataset))]
    return(list(ErrorMsg = paste("Error in response variable:", paste(Missxname, collapse = ' '), "not exist")))
  }else{
    for(i in xname){
      if(is.character(dataset[[i]])){
        dataset[[i]] = as.factor(dataset[[i]])
      }
    }
  }
  
  # check formulastring
  if(is.null(formulastring)){
    formulastring = paste(paste(yname, " 1|", sep ="~"),paste(xname, collapse = '+'), sep ="" )
    
  }
  LogiFormula = as.formula(formulastring)
  
  #############################################################################################
  ###################################### perform regression ###################################
  # reshap data
  ErrorMsg<-tryCatch({
    library(mlogit)
    mdataset = mlogit.data(dataset, shape = "wide", choice = yname)
    ErrorMsg =NULL 
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R mlogit function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # Multi Norminal Logistic
  ErrorMsg<-tryCatch({
    result = mlogit(LogiFormula, data=mdataset)
    ErrorMsg =NULL 
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R mlogit function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # abstracting information from test result
  ErrorMsg<-tryCatch({
    SummResult = summary(result)
    Formula = formulastring
    RegResult = round(SummResult$CoefTable, digits =  2)
    
    RegResultRowName = rownames(RegResult)
    RegResultColName = colnames(RegResult)
    
    significance = Sig(RegResult[,"Pr(>|t|)"])
    significance = matrix(significance, ncol = 1, dimnames = list(RegResultRowName, "significance"))
    RegResult = cbind(RegResult, significance)
    
    LogLikelyhood = SummResult$logLik[[1]]
    McFaddenR2 = SummResult$mfR2[[1]]
    
    LRatioRowName = "Likelihood Ratio Test"
    LRatioColName = c("ChiStatistic", "Df", "PValue")
    ChiStatistic = SummResult$lratio$statistic[[1]]
    Df = SummResult$lratio$parameter[[1]]
    PValue = SummResult$lratio$p.value[[1]]
    LRatioTest = matrix(c(ChiStatistic, Df, PValue), nrow = 1, 
                           dimnames = list(LRatioRowName, LRatioColName))
    ErrorMsg = NULL
    
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R glm function:', conditionMessage(e)))
  })
  
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  #############################################################################################
  ######################################## return result ######################################
  # return result
  return(list(RegResultRowName = RegResultRowName, RegResultColName = RegResultColName,
              RegResult = RegResult, LogLikelyhood = LogLikelyhood, McFaddenR2 = McFaddenR2,
              LRatioRowName = LRatioRowName,  LRatioColName = LRatioColName,
              LRatioTest = LRatioTest))
  
}

# codes below are testing codes
# rm(list=ls(all=TRUE))
# String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
# setwd(String)
# data = read.csv('datacon.csv',stringsAsFactors=F, na.strings = c(""))
# dataset = data
# dataset = na.omit(dataset)
# yname =  'dp_diff'
# xname = c('pat_age','pat_sex',"dp_nervus")
# a = MultiNormLogistic(dataset, yname = yname, xname = xname)
