BinaryLogistic<-function(dataset, rowname = NULL, colname = NULL, yname=NULL, xname=NULL, formulastring=NULL, 
                         plotstr = NULL, plotname = NULL){
  #-------------------------------------------------------------------------------------------------
  # File: BinaryLogistic.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-03-01
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # BinaryLogistic.R -  Perform Logistic Regression of dataset 
  #                 1. dependent variable is a binary variable. 
  #  
  #
  # To run this file, call it in BinaryLogistic.R 
  
  #############################################################################################
  ########################################## data check #######################################
  #Check dataset completeness
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
        colnames(dataset) = colname #注释掉这行
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
  
  # delete NA
  dataset = na.omit(dataset)
  
  # check dataset ncol
  if(!ncol(dataset)>1){
    return(list(ErrorMsg = paste("Error in data: at least 2 column allowed, you have", ncol(dataset))))
  }
  
  #############################################################################################
  ######################################## parameters check ###################################
  # check yname
  if(!(yname %in% colnames(dataset))){
    return(list(ErrorMsg = paste("Error in response variable", yname, ": not exist")))
  }else{
    dataset[[yname]] = as.factor(dataset[[yname]])
  }
  ResLeves = levels(dataset[[yname]])
  if(length(ResLeves) != 2){
    return(list(ErrorMsg = paste("Error in response variable", yname, ": only binary variable allowed after omitting na, you have", paste(ResLeves, collapse = ' '))))
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
    formulastring = paste(yname, paste(xname, collapse = '+'), sep ="~" )
  }
  LogiFormula = as.formula(formulastring)
  
  #############################################################################################
  ###################################### perform regression ###################################
  # Binary Logistic
  ErrorMsg<-tryCatch({
    result = glm(LogiFormula, family=binomial(link='logit'), data=dataset)
    ErrorMsg =NULL 
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R glm function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # abstracting information from test result
  ErrorMsg<-tryCatch({
    SummResult = summary(result)
    Formula = formulastring
    RegResult = round(SummResult$coefficients, digits =  2)
  
    RegResultRowName = rownames(RegResult)
    RegResultColName = colnames(RegResult)
  
    significance = c()
    for(i in 1:nrow(RegResult)){
    if(RegResult[i,"Pr(>|z|)"]<0.1)
    {
      if(RegResult[i,"Pr(>|z|)"]<0.05 )
      {
        if(RegResult[i,"Pr(>|z|)"]<0.05)
        {
          significance = c(significance, '***')
          next
          
        }
        significance = c(significance, '**')
        next
      }
      significance = c(significance, '*')
      next
    }else{
      significance = c(significance, '')
      next
    }
  } 
  
    significance = matrix(significance, ncol = 1, dimnames = list(RegResultRowName, "significance"))
    RegResult = cbind(RegResult, significance)
    AIC = SummResult$aic
    ErrorMsg = NULL
  
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R glm function:', conditionMessage(e)))
  })
  
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  #############################################################################################
  ########################################## plot #############################################
  ErrorMsg<-tryCatch({
    library(ROCR)
    filename = paste(plotstr, plotname, ".png")
    png(file=filename, bg="white")
    
    pre=predict(result,type='response')
    pred=prediction(pre, dataset[[yname]])
    perf=performance(pred,'tpr','fpr')
    plot(perf, main='ROC Curve')
    
    dev.off()
    ErrorMsg = NULL
    
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in plot:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # return result
  return(list(RegResultRowName = RegResultRowName, RegResultColName = RegResultColName,
              RegResult = RegResult, AIC = AIC))
}

# codes below are testing codes
# rm(list=ls(all=TRUE))
# String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
# setwd(String)
# data = read.csv('datacon.csv',stringsAsFactors=F, na.strings = c(""))
# dataset = data
# yname = 'dp_nervus'
# xname = c('pat_sex','pat_age','dp_diff')
# plotstr = paste(String, "LogisticRegression/", sep = '')
# plotname = 'roc'
# a = BinaryLogistic(dataset, yname = yname, xname = xname, plotstr = plotstr, plotname = plotname)
