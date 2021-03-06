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
  if(!ncol(dataset)==2){
    return(list(ErrorMsg = paste("Error in data: exactly 2 columns allowed, you have", ncol(dataset))))
  }
  
  return(dataset)
}

PersonCorTest<-function(dataset,numvar1 = NULL, numvar2 = NULL,
                               side = "twotail", confidence = 0.95){
  
  #------------------------------------------------------------------------------# File: DoublePairWilcoxTest
  # Version 1.0.0
  # By chao zhang 
  # 2017-03-18
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # PersonCorTest.R -  Perform Person Correlationship Test. 
  #                  Testing whether the crorelationship  between two  population
  # To run this file, call it in PersonCorTest.R  
  #############################################################################################
  dataset = DataCheck(dataset)
  #############################################################################################
  ############################ parameters check ###############################################
  # check numvar1
  numvar1 = iconv(numvar1, to = 'gbk')
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
  numvar2 = iconv(numvar2, to = 'gbk')
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
  tpyes = c("two.sided",  "greater", "less")
  mytypes = c("twotail", "lefttail", "righttail")
  if(!(side %in% mytypes)){
    return(list(ErrorMsg = "Error in side: no such value"))
  }else(
    side = tpyes[mytypes %in% side]
  )
  
  # Check confidence
  if(!is.numeric(confidence) || length(confidence) != 1 ){
    return(list(ErrorMsg = "Error in confidence: must be a single number"))
    if(confidence >1 || confidence< 0){
      return(list(ErrorMsg = "Error in confidence: must be in [0, 1]"))
    }
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
    result = cor.test(column1, column2, alternative = side,  conf.level = confidence,
                        method = 'pearson')
    
    ErrorMsg = NULL
  },
  error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R cor.test function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # abstracting information from test result
  TStatistic = result$statistic[[1]]
  EstiRho  = result$estimate[[1]]
  PValue = result$p.value[[1]]
  if("conf.int" %in% names(result)){
    LCI = result$conf.int[[1]]
    UCI = result$conf.int[[2]]
  }else{
    LCI = 'not exists: sample less than 4'
    UCI = 'not exists: sample less than 4'
  }

  #return result
  return(list(TStatistic = TStatistic,  PValue = PValue, EstiRho = EstiRho, LCI= LCI, UCI =UCI))
  
}


#codes below are testing codes
# rm(list=ls(all=TRUE))
# String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
# setwd(String)
# d = read.csv('datacon.csv',stringsAsFactors=F, na.strings = c(""))
# dataset = d[,c(-3,-1)]
# numvar1 = 'pat_age'
# numvar2 = 'dp_nervus'
# a = PersonCorTest(dataset, numvar1 = numvar1, numvar2 = numvar2)
