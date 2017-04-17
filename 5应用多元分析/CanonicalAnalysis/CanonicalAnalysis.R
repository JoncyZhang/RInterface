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
  if(ncol(dataset)<4){
    return(list(ErrorMsg = paste("Error in data: at least 4 columns allowed, you have", ncol(dataset))))
  }
  
  
  return(dataset)
}

CanonicalAnalysis<-function(dataset, yname = NULL, xname = NULL, scale=TRUE){
  #-------------------------------------------------------------------------------------------------
  # File: CanonicalAnalysis.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-03-14
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # CanonicalAnalysis.R -  Perform Factor Analysis. 
  #                                        
  # To run this file, call it in CanonicalAnalysis.R 
  #############################################################################################
  dataset = DataCheck(dataset)
  #############################################################################################
  ######################################## parameters check ###################################
  # check xname
  xname = iconv(xname, to = 'gbk')
  if(!all(xname %in% colnames(dataset))){
    Missxname = xname[!(xname %in% colnames(dataset))]
    return(list(ErrorMsg = paste("Error in xname:", paste(Missxname, collapse = ' '), "not exist")))
  }else{
    for(i in xname){
      CharExitFlag = is.na(as.numeric(dataset[[i]]))
      if(any(CharExitFlag)){
        return(list(ErrorMsg = paste('Error in ', xname, ':', 'exist character in row'))) 
      }
      dataset[[i]] = as.numeric(dataset[[i]])
    }
  } 
  
  # check yname
  yname = iconv(yname, to = 'gbk')
  if(!all(yname %in% colnames(dataset))){
    Missxname = yname[!(yname %in% colnames(dataset))]
    return(list(ErrorMsg = paste("Error in yname:", paste(Missxname, collapse = ' '), "not exist")))
  }else{
    for(i in yname){
      CharExitFlag = is.na(as.numeric(dataset[[i]]))
      if(any(CharExitFlag)){
        return(list(ErrorMsg = paste('Error in ', yname, ':', 'exist character in row'))) 
      }
      dataset[[i]] = as.numeric(dataset[[i]])
    }
  }  
  
  # check scale
  if(scale == TRUE){
    dataset = as.data.frame(scale(dataset))
  }
  #############################################################################################
  ###################################### perform CAA ###########################################
  # perform CAA
  ErrorMsg<-tryCatch({
    caa<-cancor(x = dataset[xname], y = dataset[yname], xcenter = F, ycenter = F)
    ErrorMsg = NULL
  }, 
  error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R princomp function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # abstracting information from test result
  CorResult = as.matrix(caa$cor)
  CorResultRowName = paste("comp.", 1:length(caa$cor), sep = '') 
  CorResultColName = 'Cor'
    
  XLoadResult = as.matrix(caa$xcoef)
  XLoadResultRowName = rownames(XLoadResult)
  XLoadResultColName = paste("xcomp.", 1:length(caa$cor), sep = '') 
  
  YLoadResult = as.matrix(caa$ycoef)
  YLoadResultRowName = rownames(YLoadResult)
  YLoadResultColName =paste("ycomp.", 1:length(caa$cor), sep = '') 
  
  # return
  return(list( CorResultRowName = CorResultRowName, CorResultColName = CorResultColName, 
               CorResult = CorResult, XLoadResultRowName = XLoadResultRowName,
               XLoadResultColName = XLoadResultColName, XLoadResult = XLoadResult,
               YLoadResultRowName = YLoadResultRowName, YLoadResultColName = YLoadResultColName,
               YLoadResult = YLoadResult))
}

#codes below are testing codes
rm(list=ls(all=TRUE))
String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
setwd(String)
data(USArrests)
dataset = USArrests
dataset$add1 = rnorm(nrow(dataset), mean = 10, sd = 2)
dataset$add2 = rnorm(nrow(dataset), mean = 11, sd = 3)
plotstr = String
xname = c("Murder", "Assault", "add1")
yname = c( "UrbanPop","Rape","add2")
a = CanonicalAnalysis(dataset, xname = xname, yname = yname)
