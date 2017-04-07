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
  if(!ncol(dataset)==2){
    return(list(ErrorMsg = paste("Error in data: exactly 2 columns allowed, you have", ncol(dataset))))
  }
  
  
  return(dataset)
}

DoublePairTTest<-function(dataset, rowname = NULL, colname = NULL, numvar1 = NULL, numvar2 = NULL, side = "twotail", mu = 0, confidence = 0.95, 
                            varequal = FALSE){
  #-------------------------------------------------------------------------------------------------
  # File: DoublePairTTest.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-03-18
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # DoublePairTTest.R -  Perform double porputation paired t test of dataset. 
  #                  Testing whether the sample mean is equal to true mean(mu).
  #                 1. dataset must be a numeric vector or dataframe with one column
  #                 2. side is a string in ("TwoTail", "LeftTail", "RightTail"). Default is "TwoTail".             
  #                 3. mu is numeric representing true mean. Default is 0. 
  #                 4. confidence is numeric usually taking value between 0.9 and 0.99 representing confidence 
  #                    level. Default is 0.95.
  #                 5. varequal is logical representing whether there exists heteroscedasticity. If False, then
  #                    Welch approximation is utilized to cope with inequal variance.
  # To run this file, call it in DoublePairTTest.R  
  #############################################################################################
  dataset = DataCheck(dataset, rowname = rowname, colname = colname)
  #############################################################################################
  ############################ parameters check ###############################################
  # check numvar1
  if(!(numvar1 %in% colnames(dataset))){
    return(list(ErrorMsg = paste("Error in variable name", numvar1, ": not exist")))
  }else{
    CharExitFlag = grep('[^0-9]',dataset[[numvar1]])
    if(length(CharExitFlag) >1){
      return(list(ErrorMsg = paste('Error in ', numvar1, ':', 'exist character in row', paste(CharExitFlag, collapse = ', '))))
    }else
      ErrorMsg<-tryCatch({
        dataset[[numvar1]] = as.numeric(dataset[[numvar1]])
        ErrorMsg = NULL
      },error = function(e){
        ErrorMsg = list(ErrorMsg = paste('Error in as.numeric(',numvar1, ') :' , conditionMessage(e)))
      })
    if(!is.null(ErrorMsg)){
      return(ErrorMsg)
    }
  }
  
  # check numvar2
  if(!(numvar2 %in% colnames(dataset))){
    return(list(ErrorMsg = paste("Error in variable name", numvar2, ": not exist")))
  }else{
    CharExitFlag = grep('[^0-9]',dataset[[numvar2]])
    if(length(CharExitFlag) >1){
      return(list(ErrorMsg = paste('Error in ', numvar2, ':', 'exist character in row', paste(CharExitFlag, collapse = ', '))))
    }else
      ErrorMsg<-tryCatch({
        dataset[[numvar2]] = as.numeric(dataset[[numvar2]])
        ErrorMsg = NULL
      },error = function(e){
        ErrorMsg = list(ErrorMsg = paste('Error in as.numeric(',numvar2, ') :' , conditionMessage(e)))
      })
    if(!is.null(ErrorMsg)){
      return(ErrorMsg)
    }
  }
  
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
  # split numvar into 2 column
  column1 = dataset[[numvar1]]
  column2 = dataset[[numvar2]]
  column1Len = length(column1)
  column2Len = length(column2)
  
  # Double Pair TTest
  ErrorMsg<-tryCatch({
    result = t.test(column1, column2, alternative = side, mu = mu, conf.level = confidence, 
                    var.equal = varequal, paired = TRUE)
    ErrorMsg = NULL
  }, 
  error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R t.test function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # abstracting information from test result
  
  TStatistic = result$statistic[[1]]
  PValue = result$p.value[[1]]
  LCI = result$conf.int[1]
  UCI = result$conf.int[2]
  
  return(list(TStatistic = TStatistic, PValue = PValue, LCI = LCI, UCI = UCI))
  
}


#codes below are testing codes
# rm(list=ls(all=TRUE))
# String = "E:/WorkSpace/Rstudio/Deepaint/"
# setwd(String)
# d = read.csv('data.csv',stringsAsFactors=F, na.strings = c(""))
# 
# Male = d$pat_age[which(d$pat_sex=='男')[1:1616]]
# Female = d$pat_age[which(d$pat_sex=='女')[1:1616]]
# dataset = data.frame(Male = Male, Female = Female )
# write.csv(dataset,file = 'DoublePairTTest.csv',  row.names = FALSE)
# numvar1 = "Male"
# numvar2 = "Female"

# a = DoublePairTTest(dataset, numvar1 = "Male", numvar2 = "Female")
