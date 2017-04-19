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
        rowname = iconv(rowname,to = 'gbk')
        rownames(dataset) = rowname
      }
      if(!is.null(colname)){
        colname = iconv(colname,to = 'gbk')
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
  # if(ncol(dataset)<1){
  #   return(list(ErrorMsg = paste("Error in data: at least 2 columns allowed, you have", ncol(dataset))))
  # }
  
  return(dataset)
}

SVM<-function(traindata,trainrowname = NULL, traincolname = NULL, 
              testdata = NULL, testrowname = NULL, testcolname = NULL, 
              yname=NULL, xname=NULL,scale = TRUE,
              costrange = NULL, gammarange = NULL, sampling = "fix",
              plotstr = plotstr, svmname = svmname){
  #-------------------------------------------------------------------------------------------------
  # File: SVM.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-03-14
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # SVM.R -  Perform SVM classification of dataset. 
  #                    
  #                                        
  # To run this file, call it in SVM.R 
  #############################################################################################
  traindata = DataCheck(traindata)
  #############################################################################################
  ######################################## parameters check ###################################
  # check yname
  if(is.null(yname)){
    return(return(list(ErrorMsg ="Error in yname: not exist" )))
  }else{
    yname = iconv(yname, to = 'gbk')
  }
  if(length(yname) != 1){
    return(list(ErrorMsg = paste("Error in yname: exactly one allowed, you have ", length(yname))))
  }
  if(!(yname %in% colnames(traindata))){
    return(list(ErrorMsg = paste("Error in response variable", yname, ": not exist")))
  }else{
    traindata[[yname]] = as.factor(traindata[[yname]])
  }
  
  # check xname
  if(is.null(xname)){
    xname = colnames(traindata)[-which(colnames(traindata) == yname)]
  }else{
    xname = iconv(xname, to = 'gbk')
  }
  
  if(!all(xname %in% colnames(traindata))){
    Missxname = xname[!(xname %in% colnames(traindata))]
    return(list(ErrorMsg = paste("Error in xname:", paste(Missxname, collapse = ' '), "not exist")))
  }
  
  # xname are required to be  numeric
  for(i in xname){
    CharExitFlag = is.na(as.numeric(traindata[[i]]))
    if(any(CharExitFlag)){
      traindata[[i]] = as.numeric(as.factor(traindata[[i]]))
    }else{
      traindata[[i]]  = as.numeric(traindata[[i]])
    }
  }
  
  #test data
  if(!is.null(testdata)){
    nxname = colnames(testdata)
    flag1 = length(nxname) == length(xname)
    falg2 = all(nxname %in% xname)
    if(flag1 & falg2){
      testdata = DataCheck(testdata)
    }else{
      return(return(list(ErrorMsg = "Error in testdata: colname doesn't match xname")))
    }
    
    # nxname are required to be numeic
    for(i in nxname){
      CharExitFlag = is.na(as.numeric(testdata[[i]]))
      if(any(CharExitFlag)){
        testdata[[i]] = as.numeric(as.factor(testdata[[i]]))
      }else{
        testdata[[i]]  = as.numeric(testdata[[i]])
      }
    }
  }
  
  # check scale
  if(scale == TRUE){
    traindata[xname] = as.data.frame(scale(traindata[xname]))
    if(!is.null(testdata)){
      testdata = as.data.frame(scale(testdata[xname]))
    }
  }
  
  # check cost and sigma
  if(is.null(costrange)){
    c = c(-5,5)
  }else{
    
    c = as.numeric(costrange)
    c[1] = costrange[1]
    c[2] = if(length(costrange) > 1) costrange[2] else costrange[1]
  }
  
  if(is.null(gammarange)){
    s = c(-5,5)
  }else{
    s = as.numeric(gammarange)
    s[1] = gammarange[1]
    s[2] = if(length(gammarange) > 1) gammarange[2] else gammarange[1]
  }
  
  # check formulastring
  formulastring = paste(yname, paste(xname, collapse = '+'), sep ="~" )
  LinearFormula = as.formula(formulastring)
  #############################################################################################
  ###################################### perform SVM ##########################################
  # SVM
  ErrorMsg<-tryCatch({
    
    library(e1071)
    obj <- tune(svm,LinearFormula, data = traindata,
                ranges = list(gamma = 2^(s[1]:s[2]), cost = 2^(c[2]:c[1])),
                tunecontrol = tune.control(sampling = sampling))
    
    bestg = obj$best.parameters$gamma
    bestc = obj$best.parameters$cost
    besttrainerror = obj$best.performance
    
    result = svm(LinearFormula, data = traindata, kernel = 'radial',
                 gamma = bestg, cost = bestc)
    
    ErrorMsg =NULL 
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R svm function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # abstracting information from test result
  label = predict(result, traindata[xname]) 
  Confusion = table(traindata[[yname]], label)
  ConfusionRowName = rownames(Confusion)
  ConfusionColName = rownames(Confusion)
  
  # test
  if(!is.null(testdata)){
    Prediction = as.matrix(predict(result, testdata))
    colnames(Prediction) = 'label'
    PredictionRowName = rownames(testdata)
    PredictionColName = 'label'
    pred = list(PredictionRowName = PredictionRowName,
                PredictionColName = PredictionColName,
                Prediction = Prediction)
  }else{
    pred = list()
  }
  
  #############################################################################################
  ########################################## plot #############################################
  if(!is.null(plotstr) & !is.null(svmname)){
    filename = paste(plotstr,svmname , ".png", sep = '')
    png(file=filename, bg="white") 
    plot(obj, transform.x = log2, transform.y = log2, type = "contour",
         main = "Training Performance")
    
    dev.off()
  }
  
  #return
  return(c(list(BestGamma = bestg, BestCost = bestc, BestTrainError = besttrainerror,
              ConfusionRowName = ConfusionRowName, ConfusionColName = ConfusionColName,
              colnames), pred))
  
}


# codes below are testing codes
rm(list=ls(all=TRUE))
String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
setwd(String)
data = read.csv('datacon.csv',stringsAsFactors=F, na.strings = c(""))
traindata = data
yname = 'dp_nervus'
xname = c('pat_sex','dp_diff','pat_age')
testdata = traindata[,xname]
scale = TRUE
plotstr = String
svmname = "trainerror"

a = SVM(traindata, testdata = testdata, yname = yname, xname = xname,
        scale = scale, costrange = NULL, gammarange = NULL,
        plotstr = plotstr, svmname = svmname)
