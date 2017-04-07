MultiFactorsAnova<-function(dataset, rowname = NULL, colname = NULL,  yname = NULL, xname = NULL,
                         formulastring = NULL){
  #-------------------------------------------------------------------------------------------------
  # File: UinFactorAnova.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-02-28
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # MultiFactorsAnova.R -  Perform one-way factorial variance analysis of dataset.
  #            1. two columns are stirctly required in dataset. One of the column is a string vector
  #               representing factors, while another is numeric vector. 
  #            2. strongly suggest giving formulastring by user, otherwise system will treat string 
  #               vector as response variable and numeric vector as independent variable.xs
  #                                        
  # To run this file, call it in UinFactorAnova.R 
  #Check dataset completeness
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
  if(!(ncol(dataset) > 1)){
    return(list(ErrorMsg = paste("Error in data: exactly 2 column allowed, you have", ncol(dataset))))
  }
  #############################################################################################
  ######################################## parameters check ###################################
  # check xname
  if(is.null(xname)){
    return(list(ErrorMsg = paste("Error in independent variable: at least one needed")))
  }
  if(!all(xname %in% colnames(dataset))){
    return(list(ErrorMsg = paste("Error in independent variable:", xname, "not exist")))
  }
  
  for(i in xname){
    dataset[[i]] = as.factor(dataset[[i]])
  }
  
  
  # check yname
  if(is.null(yname) || length(yname) != 1){
    return(list(ErrorMsg = paste("Error in response variable: exactly one allowed, you have ", length(yname))))
  }
  if(!(yname %in% colnames(dataset))){
    return(list(ErrorMsg = paste("Error in response variable:", yname, "not exist")))
  }
  if(!is.numeric(dataset[[yname]])){
    return(list(ErrorMsg = paste("Error in response variable:", yname, " require numeric")))
  }
  
  # check formulastring
  if(is.null(formulastring)){
    formulastring = paste(yname, paste(xname, collapse = '+'), sep ="~" )
  }
  LogiFormula = as.formula(formulastring)
  #############################################################################################
  ###################################### perform anova ########################################
  # anova
  ErrorMsg<-tryCatch({
    result = aov(LogiFormula, data = dataset)
    ErrorMsg =NULL 
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R aov function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # abstracting information from test result
  ErrorMsg<-tryCatch({
    SummResult = summary(result)
    AnoResult = as.matrix(round(result$coefficients, digits =  2))
    AnoResultRowName =  rownames(AnoResult)
    AnoResultColName = "Estimate"
    
    FStatistic = SummResult[[1]]$`F value`[1]
    FDf1 = SummResult[[1]]$Df[1]
    FDf2 = SummResult[[1]]$Df[2]
    FPValue = SummResult[[1]]$`Pr(>F)`[1]
    
    ErrorMsg = NULL
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in abstracting information:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  
  # return results
  return(list(AnoResultRowName = AnoResultRowName, AnoResultColName = AnoResultColName, 
              AnoResult = AnoResult,  FStatistic = FStatistic,
              FDf1 = FDf1, FDf2 = FDf2, FPValue = FPValue))
}

# codes below are testing codes
rm(list=ls(all=TRUE))
String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
setwd(String)
data = read.csv('datacon.csv',stringsAsFactors=F, na.strings = c(""))
dataset = data
yname = 'pat_age'
xname = c('pat_sex', 'dp_diff', "dp_nervus")
a = MultiFactorsAnova(dataset, yname = yname, xname = xname)
