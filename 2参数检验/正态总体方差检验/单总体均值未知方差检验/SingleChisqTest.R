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
    return(list(ErrorMsg = paste("Error in data: only 1 column allowed, you have", ncol(dataset))))
  }
  
  return(dataset)
}

SingleChisqTest<-function(dataset, side = "twotail", 
                          sigma2 = 1, confidence = 0.95){
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
  # SingleTTest.R -  Perform single porputation chi test of dataset. 
  #                  Testing whether the sample sd is equal to true sd(sig).
  # To run this file, call it in SingleTTest.R  
  #############################################################################################
  dataset = DataCheck(dataset)
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
  if(!is.numeric(sigma2) || length(sigma2) != 1){
    return(list(ErrorMsg = "Error in sigma2: must be a single number"))
  }
  if(sigma2<0){
    return(list(ErrorMsg = "Error in sigma2: must be non negative"))
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
  # SingleTTest
  ErrorMsg<-tryCatch({
    s2 = var(dataset[[1]])
    df = length(dataset[[1]])-1
    chi2 = df*s2/sigma2
    p = PValue = pchisq(chi2, df)
    
    if(side == "two.sided"){
      PValue =  if(p<1/2) 2*p else 2*(1-p)
      LCI = df*s2/qchisq(confidence/2, df)
      UCI = df*s2/qchisq(1-confidence/2, df)
    }
    
    if(side == "greater"){
      PValue = p
      LCI = 0
      UCI = df*s2/qchisq(1-confidence, df)
    }
    if(side == "less"){
      PValue = 1-p
      LCI = df*s2/qchisq(confidence, df)
      UCI = Inf
    }
    
    
    
    ErrorMsg = NULL
  }, 
  error = function(e){
    ErrorMsg= list(ErrorMsg = paste('Error in R t.test function:', conditionMessage(e)))
    })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # return result
  return(list(ChisqStatistic = chi2, PValue = PValue, LCI = LCI, UCI = UCI))

}


#codes below are testing codes
# rm(list=ls(all=TRUE))
# String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
# setwd(String)
# data = read.csv('datacon.csv',stringsAsFactors=F, na.strings = c(""))
# dataset = as.data.frame(data$pat_age)
# a = SingleChisqTest(dataset,confidence = 0.5, side = 'righttail')
