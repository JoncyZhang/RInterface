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

VarianceFTest<-function(dataset, numvar = NULL, 
                       chavar = NULL, side = "twotail", ratio = 1, confidence = 0.95){
  #-------------------------------------------------------------------------------------------------
  # File: VarianceFTest.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-03-18
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # VarianceFTest.R -  Perform homogeneity test of variance. 
  #                  Testing whether two pupulation have the same variance(normal distribution assumption).
  #                
  # To run this file, call it in VarianceFTest.R  
  #############################################################################################
  #############################################################################################
  dataset = DataCheck(dataset)
  #############################################################################################
  ######################################## parameters check ###################################
  # check numvar
  numvar = iconv(numvar, to = 'gbk')
  if(!(numvar %in% colnames(dataset))){
    return(list(ErrorMsg = paste("Error in variable name", numvar, ": not exist")))
  }else{
    CharExitFlag = grep('[^0-9]',dataset[[numvar]])
    if(length(CharExitFlag) >1){
      return(list(ErrorMsg = paste('Error in ', numvar, ':', 'exist character in row', paste(CharExitFlag, collapse = ', '))))
    }else
      ErrorMsg<-tryCatch({
        dataset[[numvar]] = as.numeric(dataset[[numvar]])
        ErrorMsg = NULL
      },error = function(e){
        ErrorMsg = list(ErrorMsg = paste('Error in as.numeric(',numvar, ') :' , conditionMessage(e)))
      })
    if(!is.null(ErrorMsg)){
      return(ErrorMsg)
    }
  }
  
  # check chavar
  chavar = iconv(chavar, to = 'gbk')
  if(!(chavar %in% colnames(dataset))){
    return(list(ErrorMsg = paste("Error in variable name", chavar, ": not exist")))
  }else{
    ErrorMsg<-tryCatch({
      dataset[[chavar]] = as.factor(dataset[[chavar]])
      ErrorMsg = NULL
    },error = function(e){
      ErrorMsg = list(ErrorMsg = paste('Error in as.factor(',chavar, ') :' , conditionMessage(e)))
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
  
  # Check ratio
  if(!is.numeric(ratio) || length(ratio) != 1){
    return(list(ErrorMsg = "Error in ratio: must be a single number"))
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
  # split numvar into 2 column
  faclevel = levels(dataset[[chavar]])
  if(length(faclevel) != 2 ){
    return(list(ErrorMsg = paste('Error in ',chavar, ':' , "require 2 values exactly", ',now is',length(faclevel))))
  }else{
    column1 =  dataset[which(dataset[[chavar]] == faclevel[1]),numvar]
    column2 =  dataset[which(dataset[[chavar]] == faclevel[2]),numvar]
    column1Len = length(column1)
    column2Len = length(column2)
    if(column1Len == 0){
      return(list(ErrorMsg = paste('Error in ',numvar, ':' , "no corresponding value for ", chavar, '=', faclevel[1])))
    }
    if(column2Len == 0){
      return(list(ErrorMsg = paste('Error in ',numvar, ':' , "no corresponding value for ", chavar, '=', faclevel[1])))
    }
  }
  
  # Variance F Test 
  ErrorMsg<-tryCatch({
    result = var.test(column1, column2, alternative = side, ratio = ratio, conf.level = confidence)
    ErrorMsg = NULL
  }, 
  error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R var.test function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # abstracting information from test result
  FStatistic = result$statistic[[1]]
  PValue = result$p.value[[1]]
  EstiRatio = result$estimate[[1]]
  LCI = result$conf.int[1]
  UCI = result$conf.int[2]
  
  #return result
  return(list(FStatistic = FStatistic, PValue = PValue, EstiRatio = EstiRatio, LCI = LCI, UCI = UCI))
  
}

#codes below are testing codes
rm(list=ls(all=TRUE))
String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
setwd(String)
d = read.csv('datacon.csv',stringsAsFactors=F, na.strings = c(""))
dataset = d[,c(-3,-4)]
numvar = 'pat_age'
chavar = 'pat_sex'
a = VarianceFTest(dataset, numvar = numvar, chavar = chavar)
