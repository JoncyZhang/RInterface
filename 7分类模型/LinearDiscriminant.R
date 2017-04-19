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
  # if(ncol(dataset)<1){
  #   return(list(ErrorMsg = paste("Error in data: at least 2 columns allowed, you have", ncol(dataset))))
  # }
  
  
  return(dataset)
}

LinearDiscriminant<-function(traindata,trainrowname = NULL, traincolname = NULL, 
                             testdata = NULL, testrowname = NULL, testcolname = NULL, 
                             yname=NULL, xname=NULL,
                             scale = TRUE, plotstr = NULL, ldaname = NULL){
  #-------------------------------------------------------------------------------------------------
  # File: DiscriminantAnalysis.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-03-01
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # DiscriminantAnalysis.R -  Perform DiscriminantAnalysis of dataset 
  #  
  #
  # To run this file, call it in DiscriminantAnalysis.R 
  
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
  #############################################################################################
  ###################################### perform LAD ##########################################
  # LAD
  ErrorMsg<-tryCatch({
    library(MASS)
    result = lda(x=traindata[xname], grouping = traindata[[yname]])
    ErrorMsg =NULL 
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R lda function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # abstracting information from test result
  LADMean = result$means
  LADMeanRowName = rownames(LADMean)
  LADMeanColName = colnames(LADMean)
  
  LADCoef = result$scaling
  LADCoefRowName = rownames(LADMean)
  LADCoefColName = colnames(LADMean)
  
  label = predict(result, traindata[xname]) 
  Confusion = table(traindata[[yname]], label$class)
  ConfusionRowName = rownames(Confusion)
  ConfusionColName = rownames(Confusion)
  
  # test
  if(!is.null(testdata)){
    Prediction = as.matrix(predict(result, testdata)$class)
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
  if(!is.null(plotstr) & !is.null(ldaname)){
    filename = paste(plotstr,ldaname , ".png", sep = '')
    png(file=filename, bg="white") 
    plot(result)
    dev.off()
  }
  #return
  return(c(list(LADMeanRowName = LADMeanRowName, LADMeanColName = LADMeanColName,
                LADMean = LADMean, LADCoefRowName = LADCoefRowName,
                LADCoefColName = LADCoefColName, LADCoef= LADCoef,
                ConfusionRowName = ConfusionRowName, ConfusionColName = ConfusionColName,
                Confusion = Confusion) , pred))
  
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
ldaname = 'myscatter'


rm(list=ls(all=TRUE))
String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
setwd(String)
traindata = data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]), Sp = rep(c("s","c","v"), rep(50,3)))
yname = 'Sp'
xname = c('Sepal.L.','Sepal.W.','Petal.L.','Petal.W.')
testdata = traindata[,xname]
scale = TRUE
plotstr = String
ldaname = 'myscatter'
a = LinearDiscriminant(traindata, testdata = testdata, yname = yname, xname = xname,
                       scale = scale, plotstr = plotstr, ldaname = ldaname)
                    
