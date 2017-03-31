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

DoubleUnPairTTest<-function(dataset, rowname = NULL, colname = NULL, numvar = NULL, chavar = NULL, 
                            side = "twotail", mu = 0, confidence = 0.95, 
                            varequal = FALSE){
  #-------------------------------------------------------------------------------------------------
  # File: DoubleUnPairTTest.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-03-18
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # DoubleUnPairTTest.R -  Perform double porputation unpaired t test of dataset. 
  #                  Testing whether the sample mean is equal to true mean(mu).
  #                 1. dataset must be a numeric vector or dataframe with one column
  #                 2. side is a string in ("TwoTail", "LeftTail", "RightTail"). Default is "TwoTail".             
  #                 3. mu is numeric representing true mean. Default is 0. 
  #                 4. confidence is numeric usually taking value between 0.9 and 0.99 representing confidence 
  #                    level. Default is 0.95.
  #                 5. varequal is logical representing whether there exists heteroscedasticity. If False, then
  #                    Welch approximation is utilized to cope with inequal variance.
  # To run this file, call it in DoubleUnPairTTest.R  
  #############################################################################################
  ########################################## data check #######################################
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
  
  # check dataset ncol
  if(!ncol(dataset)==2){
    return(list(ErrorMsg = paste("Error in data: only 2 column allowed, you have", ncol(dataset))))
  }
  
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
  # Double UnPair TTest
  ErrorMsg<-tryCatch({
    result = t.test(column1, column2, alternative = side, mu = mu, conf.level = confidence, 
                    var.equal = varequal, paired = FALSE)
    ErrorMsg = NULL
  }, 
  error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R t.test function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # abstracting information from test result
  TStatistic = result$statistic
  PValue = result$p.value
  LCI = result$conf.int[1]
  UCI = result$conf.int[2]
  
  return(list(TStatistic = TStatistic, PValue = PValue, LCI = LCI, UCI = UCI))
  
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
    return(list(ErrorMsg = paste("Error in data: only 1 column allowed, you have", ncol(dataset))))
  }
  
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
  
  TStatistic = result$statistic
  PValue = result$p.value
  LCI = result$conf.int[1]
  UCI = result$conf.int[2]
  
  return(list(TStatistic = TStatistic, PValue = PValue, LCI = LCI, UCI = UCI))
  
}

GoodnessOfFitChisqTest<-function(dataset, rowname = NULL, colname = NULL, theoryp = NULL, simulationp = FALSE){
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
  ############################ data check #####################################################
  #Check dataset completeness
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
  ############################ parameters check ###############################################
  # give count of dataset to do chisq
  if(!is.factor(dataset[[1]])){
    return(list(ErrorMsg = "Error in dataset: must be  factor"))
  }else{
    dataset = as.data.frame(table(dataset))["Freq"] 
  }
  
  #Check theoryp
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
  ChiStatistic = result$statistic
  PValue = result$p.value
  Df = result$parameter
  
  # return result
  return(list(ChiStatistic = ChiStatistic, PValue = PValue, Df = Df))
  
}

ContingencyTableChisqTest<-function(dataset, rowname = NULL, colname = NULL, chavar1 = NULL, chavar2 = NULL, 
                                    simulationp = TRUE){
  #-------------------------------------------------------------------------------------------------
  # File: ContingencyTableChisqTest.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-03-01
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # ContingencyTableChisqTest.R -  Perform Chi-square test of dataset. 
  #                                1. When dataset contains two columns, then a two-dimensional contingency 
  #                                    table test.All two columns are of the same length.
  #                                2. Notice that data in dataset type shoule be integer.
  #                                3. simulationp indicating whether p-value is computed from the asymptotic 
  #                                   chi-squared distribution of the test statistic. Default is FALSE.
  #               
  #                                        
  # To run this file, call it in ContingencyTableChisqTest.R 
  #############################################################################################
  ########################################## data check #######################################
  #Check dataset completeness
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
  
  
  # check data again
  if(!is.data.frame(dataset)){
    return(list(ErrorMsg = "Error in input data: must be dataframe"))
  }
  
  # delete NA
  dataset = na.omit(dataset)
  
  # check dataset ncol
  if(!ncol(dataset)==2){
    return(list(ErrorMsg = paste("Error in data: only 2 column allowed, you have", ncol(dataset))))
  }
  
  #############################################################################################
  ######################################## parameters check ###################################
  # check chavar1
  if(!(chavar1 %in% colnames(dataset))){
    return(list(ErrorMsg = "Error in variable", chavar1, ": not exist"))
  }else{
    dataset[[chavar1]] = as.factor(dataset[[chavar1]])
  }
  
  # check chavar2
  if(!(chavar2 %in% colnames(dataset))){
    return(list(ErrorMsg = "Error in variable", chavar2, ": not exist"))
  }else{
    dataset[[chavar2]] = as.factor(dataset[[chavar2]])
  }
  
  #Check simulationp
  if(!is.logical(simulationp)){
    return(list(ErrorMsg = "Error in simulationp: must be a logical"))
  }
  
  #############################################################################################
  ###################################### Contingency Table ####################################
  # Count table
  ErrorMsg<-tryCatch({
    dataset = dataset[,c(chavar1,chavar2)]
    Counttable = as.matrix(table(dataset))
    ErrorMsg = NULL
  },error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in computing Count table:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # Contingency Table ChisqTest
  ErrorMsg<-tryCatch({
    result = chisq.test(Counttable, simulate.p.value = simulationp)
    ErrorMsg = NULL
  }, 
  error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R chisq.test function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  #abstracting information from test result
  ErrorMsg<-tryCatch({
    Rowcount = apply(Counttable, 1, sum)
    RowFrecy = Rowcount/sum(Counttable)
    
    Colcount = apply(Counttable, 2, sum)
    ColFrecy = Colcount/sum(Counttable)
    
    Frecytable =  t( apply(Counttable, 1, function(x){x/Colcount}))
    Frecytable = round(Frecytable*100, digits = 1)
    Frecytable
    
    CounttableColName = colnames(Counttable)
    CounttableColName = paste(CounttableColName, rep("Count", length(CounttableColName)), sep = '')
    FrecytableColName = colnames(Counttable)
    FrecytableColName = paste(FrecytableColName, rep("Ratio", length(CounttableColName)), sep = '')
    
    TableRowName = rownames(Counttable)
    TableColName = c(CounttableColName, FrecytableColName)
    
    TableResults = matrix(rbind(Counttable, Frecytable), nrow = nrow(Counttable), dimnames = list(TableRowName, TableColName))
    
    ChiStatistic = result$statistic
    PValue = result$p.value
    Df = result$parameter
    ErrorMsg = NULL
  }, 
  error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in abstracting information from test result:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # return result
  if(simulationp == TRUE){
    return(list(TableRowName = TableRowName, TableColName = TableColName, TableResults = TableResults,
                ChiStatistic = ChiStatistic, PValue = PValue))
  }else{
    return(list(TableRowName = TableRowName, TableColName = TableColName, TableResults = TableResults,
                ChiStatistic = ChiStatistic, PValue = PValue, Df = Df))
  }
  
  
}

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
  if(length(yname) != 1){
    return(list(ErrorMsg = paste("Error in response variable: exactly one allowed, you have ", length(yname))))
  }
  if(!(yname %in% colnames(dataset))){
    return(list(ErrorMsg = paste("Error in response variable:", yname, "not exist")))
  }
  if(!is.numeric(dataset[[i]])){
    return(list(ErrorMsg = paste("Error in response variable:", yname, " require numeric")))
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

BinaryLogistic<-function(dataset, rowname = NULL, colname = NULL, yname=NULL, xname=NULL, formulastring=NULL, 
                         plotstr = NULL, plotname = NULL){
  #-------------------------------------------------------------------------------------------------
  # File: BinaryLogistic.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-03-01
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # BinaryLogistic.R -  Perform Logistic Regression of dataset 
  #                 1. dependent variable is a binary variable. 
  #  
  #
  # To run this file, call it in BinaryLogistic.R 
  
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
  # check yname
  if(!(yname %in% colnames(dataset))){
    return(list(ErrorMsg = paste("Error in response variable", yname, ": not exist")))
  }else{
    dataset[[yname]] = as.factor(dataset[[yname]])
  }
  ResLeves = levels(dataset[[yname]])
  if(length(ResLeves) != 2){
    return(list(ErrorMsg = paste("Error in response variable", yname, ": only binary variable allowed after omitting na, you have", paste(ResLeves, collapse = ' '))))
  }
  
  # check xname
  if(!all(xname %in% colnames(dataset))){
    Missxname = xname[!(xname %in% colnames(dataset))]
    return(list(ErrorMsg = paste("Error in response variable:", paste(Missxname, collapse = ' '), "not exist")))
  }else{
    for(i in xname){
      if(is.character(dataset[[i]])){
        dataset[[i]] = as.factor(dataset[[i]])
      }
    }
  }
  
  # check formulastring
  if(is.null(formulastring)){
    formulastring = paste(yname, paste(xname, collapse = '+'), sep ="~" )
  }
  LogiFormula = as.formula(formulastring)
  
  #############################################################################################
  ###################################### perform regression ###################################
  # Binary Logistic
  ErrorMsg<-tryCatch({
    result = glm(LogiFormula, family=binomial(link='logit'), data=dataset)
    ErrorMsg =NULL 
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R glm function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # abstracting information from test result
  ErrorMsg<-tryCatch({
    SummResult = summary(result)
    Formula = formulastring
    RegResult = round(SummResult$coefficients, digits =  2)
    
    RegResultRowName = rownames(RegResult)
    RegResultColName = colnames(RegResult)
    
    significance = c()
    for(i in 1:nrow(RegResult)){
      if(RegResult[i,"Pr(>|z|)"]<0.1)
      {
        if(RegResult[i,"Pr(>|z|)"]<0.05 )
        {
          if(RegResult[i,"Pr(>|z|)"]<0.05)
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
    AIC = SummResult$aic
    ErrorMsg = NULL
    
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in abstracting information:', conditionMessage(e)))
  })
  
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  #############################################################################################
  ########################################## plot #############################################
  ErrorMsg<-tryCatch({
    library(ROCR)
    if(!is.null(plotstr) & !is.null(plotname)){
      filename = paste(plotstr, plotname, ".png", sep = '')
      png(file=filename, bg="white")
      
      pre=predict(result,type='response')
      pred=prediction(pre, dataset[[yname]])
      perf=performance(pred,'tpr','fpr')
      plot(perf, main='ROC Curve')
      
      dev.off()
    }
    
    ErrorMsg = NULL
    
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in plot:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # return result
  return(list(RegResultRowName = RegResultRowName, RegResultColName = RegResultColName,
              RegResult = RegResult, AIC = AIC))
}

MultiNormLogistic<-function(dataset, rowname = NULL, colname = NULL, yname=NULL, xname=NULL, 
                            formulastring=NULL){
  #-------------------------------------------------------------------------------------------------
  # File: MultiNormLogistic.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-03-01
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # BinaryLogistic.R -  Perform Multinominal Logistic Regression of dataset 
  #                 1. dependent variable is a binary variable. 
  #  
  #
  # To run this file, call it in MultiNormLogistic.R 
  
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
  # check yname
  if(!(yname %in% colnames(dataset))){
    return(list(ErrorMsg = paste("Error in response variable", yname, ": not exist")))
  }else{
    dataset[[yname]] = as.factor(dataset[[yname]])
  }
  ResLeves = levels(dataset[[yname]])
  if(length(ResLeves) < 2){
    return(list(ErrorMsg = paste("Error in response variable", yname, ":at least 2 values are required after omitting na, you have", paste(ResLeves, collapse = ' '))))
  }
  
  # check xname
  if(!all(xname %in% colnames(dataset))){
    Missxname = xname[!(xname %in% colnames(dataset))]
    return(list(ErrorMsg = paste("Error in response variable:", paste(Missxname, collapse = ' '), "not exist")))
  }else{
    for(i in xname){
      if(is.character(dataset[[i]])){
        dataset[[i]] = as.factor(dataset[[i]])
      }
    }
  }
  
  # check formulastring
  if(is.null(formulastring)){
    formulastring = paste(paste(yname, " 1|", sep ="~"),paste(xname, collapse = '+'), sep ="" )
    
  }
  LogiFormula = as.formula(formulastring)
  
  #############################################################################################
  ###################################### perform regression ###################################
  # reshap data
  ErrorMsg<-tryCatch({
    library(mlogit)
    mdataset = mlogit.data(dataset, shape = "wide", choice = yname)
    ErrorMsg =NULL 
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R mlogit function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # Multi Norminal Logistic
  ErrorMsg<-tryCatch({
    result = mlogit(LogiFormula, data=mdataset)
    ErrorMsg =NULL 
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R mlogit function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # abstracting information from test result
  ErrorMsg<-tryCatch({
    SummResult = summary(result)
    Formula = formulastring
    RegResult = round(SummResult$CoefTable, digits =  2)
    
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
    
    LogLikelyhood = SummResult$logLik[[1]]
    McFaddenR2 = SummResult$mfR2[[1]]
    
    LRatioRowName = "Likelihood Ratio Test"
    LRatioColName = c("ChiStatistic", "Df", "PValue")
    ChiStatistic = SummResult$lratio$statistic[[1]]
    Df = SummResult$lratio$parameter[[1]]
    PValue = SummResult$lratio$p.value[[1]]
    LRatioTest = matrix(c(ChiStatistic, Df, PValue), nrow = 1, 
                        dimnames = list(LRatioRowName, LRatioColName))
    ErrorMsg = NULL
    
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R glm function:', conditionMessage(e)))
  })
  
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  #############################################################################################
  ######################################## return result ######################################
  # return result
  return(list(RegResultRowName = RegResultRowName, RegResultColName = RegResultColName,
              RegResult = RegResult, LogLikelyhood = LogLikelyhood, McFaddenR2 = McFaddenR2,
              LRatioRowName = LRatioRowName,  LRatioColName = LRatioColName,
              LRatioTest = LRatioTest))
  
}

MultiFactorsAnova<-function(dataset, rowname = NULL, colname = NULL,  yname = NULL, xname = NULL,
                            formulastring = NULL){
  #-------------------------------------------------------------------------------------------------
  # File: UinFactorAnova.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-02-28
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # MultiFactorsAnova.R -  Perform one-way factorial variance analysis of dataset.
  #            1. two columns are stirctly required in dataset. One of the column is a string vector
  #               representing factors, while another is numeric vector. 
  #            2. strongly suggest giving formulastring by user, otherwise system will treat string 
  #               vector as response variable and numeric vector as independent variable.xs
  #                                        
  # To run this file, call it in UinFactorAnova.R 
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
  
  # check data again
  if(!is.data.frame(dataset)){
    return(list(ErrorMsg = "Error in input data: must be dataframe"))
  }
  
  # delete NA
  dataset = na.omit(dataset)
  
  # check dataset ncol
  if(!(ncol(dataset) > 1)){
    return(list(ErrorMsg = paste("Error in data: exactly 2 column allowed, you have", ncol(dataset))))
  }
  #############################################################################################
  ######################################## parameters check ###################################
  # check xname
  if(is.null(xname)){
    return(list(ErrorMsg = paste("Error in independent variable: at least one needed")))
  }
  if(!all(xname %in% colnames(dataset))){
    return(list(ErrorMsg = paste("Error in independent variable:", xname, "not exist")))
  }
  
  for(i in xname){
    dataset[[i]] = as.factor(dataset[[i]])
  }
  
  
  # check yname
  if(is.null(yname) || length(yname) != 1){
    return(list(ErrorMsg = paste("Error in response variable: exactly one allowed, you have ", length(yname))))
  }
  if(!(yname %in% colnames(dataset))){
    return(list(ErrorMsg = paste("Error in response variable:", yname, "not exist")))
  }
  if(!is.numeric(dataset[[yname]])){
    return(list(ErrorMsg = paste("Error in response variable:", yname, " require numeric")))
  }
  
  # check formulastring
  if(is.null(formulastring)){
    formulastring = paste(yname, paste(xname, collapse = '+'), sep ="~" )
  }
  LogiFormula = as.formula(formulastring)
  #############################################################################################
  ###################################### perform anova ########################################
  # anova
  ErrorMsg<-tryCatch({
    result = aov(LogiFormula, data = dataset)
    ErrorMsg =NULL 
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R aov function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # abstracting information from test result
  ErrorMsg<-tryCatch({
    SummResult = summary(result)
    AnoResult = as.matrix(round(result$coefficients, digits =  2))
    AnoResultRowName =  rownames(AnoResult)
    AnoResultColName = "Estimate"
    
    FStatistic = SummResult[[1]]$`F value`[1]
    FDf1 = SummResult[[1]]$Df[1]
    FDf2 = SummResult[[1]]$Df[2]
    FPValue = SummResult[[1]]$`Pr(>F)`[1]
    
    ErrorMsg = NULL
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in abstracting information:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  
  # return results
  return(list(AnoResultRowName = AnoResultRowName, AnoResultColName = AnoResultColName, 
              AnoResult = AnoResult,  FStatistic = FStatistic,
              FDf1 = FDf1, FDf2 = FDf2, FPValue = FPValue))
}

FactorAnalysis<-function(dataset, rowname = NULL, colname = NULL){
  #-------------------------------------------------------------------------------------------------
  # File: FactorAnalysis.R
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
  # To run this file, call it in FactorAnalysis.R 
  #Check dataset completeness
  #############################################################################################
  ########################################## data check #######################################
  #Check dataset completeness
  # delete NA
  dataset = na.omit(dataset)
  
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
  
  # check data again
  if(!is.data.frame(dataset)){
    return(list(ErrorMsg = "Error in input data: must be dataframe"))
  }
  
  # check dataset ncol
  ColLen = dim(dataset)[2]
  if(is.null(ColLen)){
    return(list(ErrorMsg = "Error in input data: must have column"))
  }
  #############################################################################################
  ######################################### convert ###########################################
  # vualues in (0,1)  will be consider as factors
  for(i in colnames(dataset)){
    if(is.numeric(dataset[[i]])){
      temp = unique(dataset[[i]])
      if(all(temp %in% c(0,1))){
        dataset[[i]] = as.factor(dataset[[i]])
      }
    }else{
      dataset[[i]] = as.factor(dataset[[i]])
    }
  }
  #############################################################################################
  ######################################## pick test ##########################################
  if(ColLen == 1){
    # SingleTTest
    if(is.numeric(dataset[[1]])){
      result = SingleTTest(dataset)
      Method = "Single population t Test"
    }
    
    # GoodnessOfFitChisqTest
    if(is.factor(dataset[[1]])){
      result = GoodnessOfFitChisqTest(dataset)
      Method = "Goodness of fit chi-square test" 
    }  
  }
  
  if(ColLen == 2){
    
    # DoublePairTTest
    if(is.numeric(dataset[[1]]) && is.numeric(dataset[[2]])){
      numvar1 = colnames(dataset)[1]
      numvar2 = colnames(dataset)[2]
      result = DoublePairTTest(dataset, numvar1 = numvar1, numvar2 = numvar2)
      Method = "Two population paired t Test"  
      
    }else{
      # ContingencyTableChisqTest
      if(is.factor(dataset[[1]]) && is.factor(dataset[[2]])){
        chavar1 = colnames(dataset)[1] 
        chavar2 = colnames(dataset)[2]
        result = ContingencyTableChisqTest(dataset,chavar1 = chavar1, chavar2 = chavar2)
        Method = "Contingency table chi-square test"  
      }else{
        #DoubleUnPairTTest
        if(is.numeric(dataset[[1]])){
          numvar = colnames(dataset)[1]
          chavar = colnames(dataset)[2]
        }else{
          numvar = colnames(dataset)[2]
          chavar = colnames(dataset)[1]
        }
          result = DoubleUnPairTTest(dataset, numvar = numvar, chavar = chavar)
          Method = "Two population t Test"   
      }
    }
  }
  
  if(ColLen > 2){
    
    numvar = c()
    chavar = c()
    for(i in colnames(dataset)){
      if(is.numeric(dataset[[i]])){
        numvar = c(numvar, i)
      }else{
        chavar = c(chavar, i)
      }
    }
    
    yname = colnames(dataset)[ColLen]
    xname = colnames(dataset)[-ColLen]
    # when last column is numeric
    if(is.numeric(dataset[[ColLen]])){
      # multiple factors anova
      if(!is.null(chavar) && length(chavar) == (ColLen-1)){
        result = MultiFactorsAnova(dataset, yname = yname, xname = xname)
        Method = "Multi factors anova" 
      }else{
        # linear regression
        result = LinearRegression(dataset, yname = yname, xname = xname)
        Method = "Linear regression" 
      }
    }
    
    # when last column is factor
    if(is.factor(dataset[[ColLen]])){
      
      if(length(levels(dataset[[ColLen]]) == 1)){
        result = "Error: last column has the same value"
        Method = "No method" 
      }
      
      # BinaryLogistic
      if(length(levels(dataset[[ColLen]]) == 2 )){
        result = BinaryLogistic(dataset, yname = yname, xname = xname)
        Method = "Binary regression" 
      }
      
      # MultiNormLogistic
      if(length(levels(dataset[[ColLen]])) >2){
        result = MultiNormLogistic(dataset, yname = yname, xname = xname)
        Method = "Multi norminal regression" 
      }
    }
    
  }
  
  # return result
  result$Method = Method
  return(result)
}

# codes below are testing codes
rm(list=ls(all=TRUE))
String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
setwd(String)
data = read.csv('datacon.csv',stringsAsFactors=F, na.strings = c(""))
dataset = as.data.frame(data[,4])
dataset = data[,c(1,4)]
yname = 'pat_age'
xname = 'dp_nervus'

a = FactorAnalysis(dataset)
