SingleTTest<-function(dataset, rowname = NULL, colname = NULL, side = "twotail", mu = 0, confidence = 0.95, 
                      varequal = FALSE){
  #-------------------------------------------------------------------------------------------------
  # File: SingleTTest.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-03-18
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # SingleTTest.R -  Perform single porputation t test of dataset. 
  #                  Testing whether the sample mean is equal to true mean(mu).
  #                 1. dataset must be a numeric vector or dataframe with one column
  #                 2. side is a string in ("TwoTail", "LeftTail", "RightTail"). Default is "TwoTail".             
  #                 3. mu is numeric representing true mean. Default is 0. 
  #                 4. confidence is numeric usually taking value between 0.9 and 0.99 representing confidence 
  #                    level. Default is 0.95.
  #                 5. varequal is logical representing whether there exists heteroscedasticity. If False, then
  #                    Welch approximation is utilized to cope with inequal variance.
  # To run this file, call it in SingleTTest.R  
  #############################################################################################
  ############################ data check #####################################################
  #Check dataset 
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

  # check is dataframe
  if(!is.data.frame(dataset)){
    return(list(ErrorMsg = "Error in input data: must be dataframe"))
  }
  
  # delete NA
  dataset = na.omit(dataset)
  
  #Check dataset ncol
  if(!ncol(dataset)==1){
    return(list(ErrorMsg = paste("Error in data: only 1 column allowed, you have", ncol(dataset))))
  }
  
  #############################################################################################
  ######################################## parameters check ###################################
  # Check side
  tpyes = c("two.sided", "less", "greater")
  mytypes = c("twotail", "lefttail", "righttail")
  if(!(side %in% mytypes)){
    return(list(ErrorMsg = "Error in side: no such value"))
  }else(
    side = tpyes[mytypes %in% side]
  )

  # Check mu
  if(!is.numeric(mu) || length(mu) != 1){
    return(list(ErrorMsg = "Error in mu: must be a single number"))
  }
  
  # Check confidence
  if(!is.numeric(confidence) || length(confidence) != 1 ){
    return(list(ErrorMsg = "Error in confidence: must be a single number"))
    if(confidence >1 || confidence< 0){
      return(list(ErrorMsg = "Error in confidence: must be in [0, 1]"))
    }
  }
  
  # Check varequal
  if(!is.logical(varequal) || length(varequal) != 1 ){
    return(list(ErrorMsg = "Error in varequal: must be a single logical"))
  }
  
  #############################################################################################
  ####################################### perform test ########################################
  # SingleTTest
  ErrorMsg<-tryCatch({
    result = t.test(dataset, alternative = side, mu = mu, conf.level = confidence, var.equal = varequal)
    ErrorMsg = NULL
  }, 
  error = function(e){
    ErrorMsg= list(ErrorMsg = paste('Error in R t.test function:', conditionMessage(e)))
    })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # abstracting information from test result
    TStatistic = result$statistic
    PValue = result$p.value
    LCI = result$conf.int[1]
    UCI = result$conf.int[2]
  
  # return result
  return(list(TStatistic = TStatistic, PValue = PValue, LCI = LCI, UCI = UCI))

}


#codes below are testing codes
# rm(list=ls(all=TRUE))
# String = "E:/WorkSpace/Rstudio/Deepaint/"
# setwd(String)
# data = read.csv('data.csv',stringsAsFactors=F, na.strings = c(""))
# dataset = as.data.frame(data$pat_age)
# write.csv(dataset,file = 'SingleTTest.csv',  row.names = FALSE)
# a = SingleTTest(dataset,confidence = 0.5)
