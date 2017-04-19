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
  if(ncol(dataset)<1){
    return(list(ErrorMsg = paste("Error in data: at least 2 columns allowed, you have", ncol(dataset))))
  }
  
  
  return(dataset)
}

DecisionTree<-function(traindata,trainrowname = NULL, traincolname = NULL, 
                       testdata = NULL, testrowname = NULL, testcolname = NULL, 
                       yname=NULL, xname=NULL,
                       plotstr = NULL, treename = NULL){
  #-------------------------------------------------------------------------------------------------
  # File: DecisionTree.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-03-01
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # DecisionTree.R -  Perform DecisionTree.R of dataset 
  #  
  #
  # To run this file, call it in DecisionTree.R 
  
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
  
  # xname are required to be factor or numeric/factor
  chanames = c()
  numnames = c()
  for(i in xname){
    if(is.character(traindata[[i]])){
      chanames = c(chanames, i)
      traindata[[i]] = as.factor(traindata[[i]])
    }else{
      numnames = c(numnames, i)
      traindata[[i]] = as.numeric(traindata[[i]])
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
    
    # nxname are required to be numeic/factor
    for(i in nxname){
      if(i %in%  chanames){
        testdata[[i]] = as.factor(testdata[[i]])
      }else{
        testdata[[i]] = as.numeric(testdata[[i]])
      }
    }
  }
  
  #formulastring
  formulastring = paste(yname, paste(xname, collapse = '+'), sep ="~" )
  LinearFormula = as.formula(formulastring)
  #############################################################################################
  ###################################### perform DT ###################################
  # DT
  ErrorMsg<-tryCatch({
    library(rpart)
    result = rpart(LinearFormula, data = traindata, method="class")
    ErrorMsg =NULL 
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R glm function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  

  # abstracting information from test result
  label = predict(result, traindata[xname])
  pre = as.factor(apply(label, 1, function(x){colnames(label)[which.max(x)]}))
  Confusion = table(traindata[[yname]], pre)
  ConfusionRowName = rownames(Confusion)
  ConfusionColName = rownames(Confusion)
  
  # test
  if(!is.null(testdata)){
    testlabel = predict(result, testdata[xname])
    testpre = as.factor(apply(testlabel, 1, function(x){colnames(label)[which.max(x)]}))
    
    Prediction = as.matrix(testpre)
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
  if(!is.null(plotstr) & !is.null(treename)){
    library(rpart.plot)
    filename = paste(plotstr,treename , ".png", sep = '')
    png(file=filename, bg="white") 
    rpart.plot(result)
    dev.off()
  }
  #return
  return(c(list(ConfusionRowName = ConfusionRowName, 
                ConfusionColName = ConfusionColName,
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
#testdata = traindata[,xname]
plotstr = String
treename = 'mydecisiontree'
a = DecisionTree(traindata,yname = yname, xname = xname,
                 plotstr = plotstr, treename = treename)

rm(list=ls(all=TRUE))
String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
setwd(String)
traindata = data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]), Sp = rep(c("s","c","v"), rep(50,3)))
yname = 'Sp'
xname = c('Sepal.L.','Sepal.W.','Petal.L.','Petal.W.')
testdata = traindata[,xname]
plotstr = String
treename = 'mydecisiontree'
a = DecisionTree(traindata, testdata = testdata, yname = yname, xname = xname,
                       plotstr = plotstr, treename = treename)

