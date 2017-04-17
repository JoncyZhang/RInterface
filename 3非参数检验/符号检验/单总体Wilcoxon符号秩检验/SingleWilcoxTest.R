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
  if(!ncol(dataset)==1){
    return(list(ErrorMsg = paste("Error in data: exactly 2 columns allowed, you have", ncol(dataset))))
  }
  
  
  return(dataset)
}

SingleWilcoxTest<-function(dataset, side = "twotail", 
                     mu = 0, confidence = 0.95){
  #-------------------------------------------------------------------------------------------------
  # File: SingleWilcoxTest.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-02-28
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # SingleWilcoxTest.R -  Perform one population Wilcox Signed Rank Test of dataset. 
  #                        Testing whether the sample distribution is symmetric about mu.
  #                       1. dataset must be a numeric dataframe with only one column 
  #                       3. side is a string in ("TwoTail", "LeftTail", "RightTail"). Default is "TwoTail".             
  #                       4. mu is numeric representing symmetric point. Default is 0. 
  #                       5. confidence is numeric usually taking value between 0.9 and 0.99 representing confidence 
  #                          level. Default is 0.95.
  #               
  #                                        
  # To run this file, call it in SingleWilcoxTest.R 
  #############################################################################################
  dataset = DataCheck(dataset)
  #############################################################################################
  ######################################## parameters check ###################################
  #dataset are required to be numeric
  if(!is.numeric(dataset[[1]])){
    return(list(ErrorMsg = paste("Error in data: only 1 column allowed, you have", ncol(dataset))))
  }
  
  # Check side
  tpyes = c("two.sided",  "greater", "less")
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
  
  #############################################################################################
  ####################################### perform test ########################################
  # Single Wilcox Test
  ErrorMsg<-tryCatch({
    result = wilcox.test(dataset[[1]], alternative = side, mu = mu, conf.level = confidence, 
                         conf.int = TRUE, correct = TRUE, exact = FALSE)
    ErrorMsg = NULL
  }, 
  error = function(e){
    ErrorMsg= list(ErrorMsg = paste('Error in R wilcox.test function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # abstracting information from test result
  WcoxStatistic = result$statistic[[1]]
  Median = result$estimate[[1]]
  PValue = result$p.value[[1]]
  LCI = result$conf.int[1]
  UCI = result$conf.int[2]
  
  # return result
  return(list(WcoxStatistic = WcoxStatistic,  PValue = PValue, Median = Median, LCI = LCI, UCI = UCI))
} 

#codes below are testing codes
# rm(list=ls(all=TRUE))
# String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
# setwd(String)
# data = read.csv('datacon.csv',stringsAsFactors=F, na.strings = c(""))
# dataset = as.data.frame(data$pat_age)
# a = SingleWilcoxTest(dataset,confidence = 0.5)
