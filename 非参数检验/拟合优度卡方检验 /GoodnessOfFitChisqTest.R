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
  if(!ncol(dataset)==1){
    return(list(ErrorMsg = paste("Error in data: exactly 2 columns allowed, you have", ncol(dataset))))
  }
  
  
  return(dataset)
}

GoodnessOfFitChisqTest<-function(dataset, rowname = NULL, colname = NULL, theoryp = NULL, 
                                 simulationp = FALSE){
  #-------------------------------------------------------------------------------------------------
  # File: GoodnessOfFitChisqTest.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-03-01
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # GoodnessOfFitChisqTest.R -  Perform Chi-square test of dataset. 
  #                             1. When dataset contains only one column, Chi-square test is goodness-of-fit 
  #                                test. the theoryp are used to test the difference between theory 
  #                                probability distribution and real probability distribution.
  #                             2. Notice that data in dataset type shoule be integer.
  #                             3. simulationp indicating whether p-value is computed from the asymptotic 
  #                                 chi-squared distribution of the test statistic. Default is FALSE.
  #               
  #                                        
  # To run this file, call it in GoodnessOfFitChisqTest.R 
  #############################################################################################
  dataset = DataCheck(dataset, rowname = rowname, colname = colname)
  #############################################################################################
  ############################ parameters check ###############################################
  #Check theorypß
  
  
  if(!is.null(theoryp)){
    if(!is.list(theoryp) ||!is.numeric(theoryp[[1]])){
      return(list(ErrorMsg = "Error in theoryp: must be numeric vector in list"))
    }
    if(length(theoryp) != nrow(dataset)){
      return(list(ErrorMsg = paste("Error in theoryp: theoryp length (", length(theoryp), 
                                   ') is not equal to data row length (', nrow(dataset), ")", ', maybe NA exist')))
    }
    if(any(theoryp) < 0){
      return(list(ErrorMsg = "Error in theoryp: must not be negative"))
    }
  }else{
    theoryp = rep(1/nrow(dataset), nrow(dataset))
  }
    

  #Check simulationp
  if(!is.logical(simulationp)){
    return(list(ErrorMsg = "Error in simulationp: must be a logical"))
  }
  
  #############################################################################################
  ############################ perform test ###################################################
  # SingleChisqTest
  ErrorMsg<-tryCatch({
    result = chisq.test(dataset, p = theoryp, simulate.p.value = simulationp)
    ErrorMsg =NULL 
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R chisq.test function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # abstracting information from test result
  ChisqStatistic = result$statistic
  PValue = result$p.value
  Df = result$parameter

  # return result
  return(list(ChisqStatistic = ChisqStatistic, PValue = PValue, Df = Df))
  
}

#codes below are testing codes
# rm(list=ls(all=TRUE))
# String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
# setwd(String)
# d = read.csv('datacon.csv',stringsAsFactors=F, na.strings = c(""))
# dataset = as.data.frame(d$pat_age)
# a = GoodnessOfFitChisqTest(dataset)
