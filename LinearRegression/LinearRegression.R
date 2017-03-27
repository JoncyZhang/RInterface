LinearRegression<-function(dataset, rowname = NULL, colname = NULL, yname = NULL, xname = NULL, formulastring = NULL, 
                           intercept = TRUE,plotstr = NULL,
                           resifitname = NULL, normalqqname = NULL,
                           scallocname = NULL, resilevname = NULL){
  #-------------------------------------------------------------------------------------------------
  # File: LinearRegression.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-03-11
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # LinearRegression.R -  Perform Linear Regression of dataset. 
  #             1.yname is the name of dependent variable;xname are the names of independent variables
  #               
  #                                        
  # To run this file, call it in LinearRegression.R 
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
  # check xname
  if(!all(xname %in% colnames(dataset))){
    Missxname = xname[!(xname %in% colnames(dataset))]
    return(list(ErrorMsg = paste("Error in independent variable:", paste(Missxname, collapse = ' '), "not exist")))
  }else{
    for(i in xname){
      if(is.character(dataset[[i]])){
        dataset[[i]] = as.factor(dataset[[i]])
      }
    }
  }
  
  # check yname
  if(!all(yname %in% colnames(dataset))){
    Missxname = yname[!(yname %in% colnames(dataset))]
    return(list(ErrorMsg = paste("Error in response variable:", paste(Missxname, collapse = ' '), "not exist")))
  }else{
    for(i in yname){
      if(is.character(dataset[[i]])){
        dataset[[i]] = as.factor(dataset[[i]])
      }
    }
  }
  
  # check formulastring
  if(is.null(formulastring)){
    formulastring = paste(yname, paste(xname, collapse = '+'), sep ="~" )
    if(intercept==FALSE){
      formulastring = paste(formulastring, -1, sep =" " )
    }
   
  }
  LinearFormula = as.formula(formulastring)
  
  #############################################################################################
  ###################################### perform regression ###################################
  # Linear regression
  ErrorMsg<-tryCatch({
    result = lm(LinearFormula, data = dataset)
    ErrorMsg =NULL 
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R lm function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # abstracting information from test result
  ErrorMsg<-tryCatch({
    SummResult = summary(result)
    RegResult = round(SummResult$coefficients, digits =  2) 
    
    RegResultRowName = rownames(RegResult)
    RegResultColName = colnames(RegResult)
    
    significance = c()
    for(i in 1:nrow(RegResult)){
      if(RegResult[i,"Pr(>|t|)"]<0.1)
      {
        if(RegResult[i,"Pr(>|t|)"]<0.05 )
        {
          if(RegResult[i,"Pr(>|t|)"]<0.05)
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
    
    Rsquare = SummResult$r.squared
    AdjRsquare = SummResult$adj.r.squared
    
    MSE = sqrt(sum(result$residuals^2)/result$df.residual)
    MSEDf = result$df.residual
    
    FStatistic = SummResult$fstatistic[[1]]
    FDf1 = SummResult$fstatistic[[2]]
    FDf2 = SummResult$fstatistic[[3]]
    FPValue = 1 - pf(FStatistic, FDf1, FDf2)
    
    ErrorMsg = NULL
    
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in abstracting information:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }

  #############################################################################################
  ######################################## plot ###############################################
  ErrorMsg<-tryCatch({
  if(!is.null(plotstr) & !is.null(resifitname)){
    filename = paste(plotstr,resifitname , ".png", sep = '')
    png(file=filename, bg="white") 
    plot(result, sub = '', ask = FALSE, which = 1)
    dev.off()
  }
  if(!is.null(plotstr) & !is.null(normalqqname)){
    filename = paste(plotstr,normalqqname , ".png", sep = '')
    png(file=filename, bg="white") 
    plot(result, sub = '', ask = FALSE, which = 2)
    dev.off()
  }
  if(!is.null(plotstr) & !is.null(scallocname)){
    filename = paste(plotstr,scallocname , ".png", sep = '')
    png(file=filename, bg="white") 
    plot(result, sub = '', ask = FALSE, which = 3)
    dev.off()
  }
  if(!is.null(plotstr) & !is.null(resilevname)){
    filename = paste(plotstr,resilevname , ".png", sep = '')
    png(file=filename, bg="white") 
    plot(result, sub = '', ask = FALSE, which = 5)
    dev.off()
  }
  
  ErrorMsg = NULL
  
}, error = function(e){
  ErrorMsg = list(ErrorMsg = paste('Error in abstracting information:', conditionMessage(e)))
})
if(!is.null(ErrorMsg)){
  return(ErrorMsg)
}
  # return results
  return(list( RegResultRowName = RegResultRowName, RegResultColName = RegResultColName,RegResult,
               Rsquare = Rsquare,  AdjRsquare = AdjRsquare,
               MSE = MSE, MSEDf = MSEDf,
               FStatistic = FStatistic, FDf1 = FDf1, FDf2 =FDf2,
               FPValue = FPValue))
}   

# codes below are testing codes
rm(list=ls(all=TRUE))
String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
setwd(String)
data = read.csv('datacon.csv',stringsAsFactors=F, na.strings = c(""))
dataset = data
yname = 'pat_age'
xname = c('pat_sex','dp_nervus','dp_diff')
intercept = TRUE
plotstr = paste(String, "LinearRegression/", sep = '')
resifitname = 'myresifit'
normalqqname = 'mynormalqq'
scallocname = 'myscalloc'
resilevname = 'myresilev'
a = LinearRegression(dataset, yname = yname, xname = xname, intercept = intercept,
                     plotstr = plotstr,
                     resifitname = resifitname, normalqqname = normalqqname,
                     scallocname = scallocname, resilevname = resilevname)

