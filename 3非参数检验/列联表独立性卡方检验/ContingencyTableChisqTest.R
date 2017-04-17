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

ContingencyTableChisqTest<-function(dataset, chavar1 = NULL, chavar2 = NULL, 
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
  dataset = DataCheck(dataset)
  #############################################################################################
  ######################################## parameters check ###################################
  # check chavar1
  chavar1 = iconv(chavar1, to = 'gbk')
  if(!(chavar1 %in% colnames(dataset))){
    return(list(ErrorMsg = "Error in variable", chavar1, ": not exist"))
  }else{
    dataset[[chavar1]] = as.factor(dataset[[chavar1]])
  }

  # check chavar2
  chavar2 = iconv(chavar2, to = 'gbk')
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
    
    ChisqStatistic = result$statistic
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
                ChisqStatistic = ChisqStatistic, PValue = PValue))
  }else{
    return(list(TableRowName = TableRowName, TableColName = TableColName, TableResults = TableResults,
                ChisqStatistic = ChisqStatistic, PValue = PValue, Df = Df))
  }
  

}

#codes below are testing codes
 # rm(list=ls(all=TRUE))
 # String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
 # setwd(String)
 # d = read.csv('datacon.csv',stringsAsFactors=F, na.strings = c(""))
 # dataset = d[,c(-2,-4)]
 # write.csv(dataset, file = "ContingencyTableChisqTest.csv", row.names = FALSE,na = "")
 # chavar1 = 'dp_diff'
 # chavar2 = 'pat_sex'
 # a = ContingencyTableChisqTest(dataset, chavar1 = chavar1, chavar2 = chavar2)


