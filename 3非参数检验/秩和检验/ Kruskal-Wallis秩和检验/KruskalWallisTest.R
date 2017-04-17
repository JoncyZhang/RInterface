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
  if(ncol(dataset)<1){ #=2 而不是1
    return(list(ErrorMsg = paste("Error in data: only 2ß column allowed, you have", ncol(dataset))))
  }
  
  #############################################################################################
  ###################################### return check #########################################
  return(dataset)
}

KruskalWallisTest<-function(dataset, numvar = NULL, chavar = NULL){
                                 
  #-------------------------------------------------------------------------------------------------
  # File: KruskalWallisTest.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-03-18
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # KruskalWallisTest.R -  Perform double/more porputation Kruskal-Wallis test of dataset. 
  #                  Testing whether the sample distribution difference is symmetric about mu.
  # To run this file, call it in KruskalWallisTest.R  
  
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
  faclevel = levels(dataset[[chavar]])
  if(length(faclevel) < 2 ){
    return(list(ErrorMsg = paste('Error in ',chavar, ':' , "require at least 2 values", ',now is',length(faclevel))))
  }
  
  
  #############################################################################################
  ####################################### perform test ########################################
  # Kruskal-Wallis Test
  ErrorMsg<-tryCatch({
    result = kruskal.test(x= dataset[[numvar]], g = dataset[[chavar]])
    ErrorMsg = NULL
  }, 
  error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R wilcox.test function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  
  # abstracting information from test result
  KWChisqStatistic = result$statistic[[1]]
  PValue = result$p.value[[1]]
  #return result
  return(list(KWChisqStatistic = KWChisqStatistic,  PValue = PValue))
  
}


#codes below are testing codes
# rm(list=ls(all=TRUE))
# String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
# setwd(String)
# d = read.csv('datacon.csv',stringsAsFactors=F, na.strings = c(""))
# dataset = d[,c(-3,-4)]
# numvar = 'pat_age'
# chavar = 'pat_sex'
# a = KruskalWallisTest(dataset, numvar = numvar, chavar = chavar)
