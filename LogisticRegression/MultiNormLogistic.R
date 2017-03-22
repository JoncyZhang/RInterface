MultiNormLogistic<-function(dataset, rowname = NULL, colname = NULL, yname=NULL, xname=NULL, formulastring=NULL, 
                         plotstr = NULL){
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
    formulastring = paste(yname, paste(xname, collapse = '+'), sep ="~" )
  }
  LogiFormula = as.formula(formulastring)
  
  #############################################################################################
  ###################################### perform regression ###################################
  # Multi Norminal Logistic
  ErrorMsg<-tryCatch({
    library(nnet)
    result = multinom(LogiFormula, data=dataset)
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
  
}

# codes below are testing codes
rm(list=ls(all=TRUE))
String = "E:/WorkSpace/Rstudio/Deepaint/"
setwd(String)
data = read.csv('data.csv',stringsAsFactors=F, na.strings = c(""))
dataset = data
write.csv(dataset, file = "MultiNormLogistic.csv", row.names = FALSE,na = "")
yname =  'dp_diff'
xname = c('pat_sex','pat_age','dp_nervus')
plotstr = String
a = BinaryLogistic(dataset, yname = yname, xname = xname, plotstr = plotstr)