DataCheck<-function(dataset, rowname = NULL, colname = NULL){
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
  if(!ncol(dataset)==2){
    return(list(ErrorMsg = paste("Error in data: exactly 2 column allowed, you have", ncol(dataset))))
  }
  
  return(dataset)
}

BartlettTest<-function(dataset, rowname = NULL, colname = NULL, numvar = NULL, 
                        chavar = NULL){
  #-------------------------------------------------------------------------------------------------
  # File: BartlettTest.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-03-18
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # BartlettTest.R -  Perform homogeneity test of variance. 
  #                  Testing whether two/or more pupulation have the same variance(normal distribution assumption).
  #                
  # To run this file, call it in BartlettTest.R  
  #############################################################################################
  #############################################################################################
  dataset = DataCheck(dataset, rowname = rowname, colname = colname)
  #############################################################################################
  ######################################## parameters check ###################################
  # check numvar
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
  # Bartlett Test 
  ErrorMsg<-tryCatch({
    result = bartlett.test(x= dataset[[numvar]], g = dataset[[chavar]])
    ErrorMsg = NULL
  }, 
  error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R bartlett.test function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # abstracting information from test result
  KsqrStatistic = result$statistic[[1]]
  PValue = result$p.value[[1]]
 
  #return result
  return(list(KsqrStatistic = KsqrStatistic, PValue = PValue))
  
}

# rm(list=ls(all=TRUE))
# String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
# setwd(String)
# d = read.csv('datacon.csv',stringsAsFactors=F, na.strings = c(""))
# dataset = d[,c(-3,-4)]
# numvar = 'pat_age'
# chavar = 'pat_sex'
# a = BartlettTest(dataset, numvar = numvar, chavar = chavar)
#   