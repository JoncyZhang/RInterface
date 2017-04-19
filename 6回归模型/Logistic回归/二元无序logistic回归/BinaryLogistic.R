RocPlot<-function(score, label, plotstr = NULL, rocname = NULL){
   #############################################################################################
   ########################################## plot #############################################
   ErrorMsg<-tryCatch({
     library(ROCR)
     pred = prediction(score, label) #
     perf= performance(pred,'tpr','fpr')
     auc = performance(pred, 'auc')
     auc = round(unlist(slot(auc, 'y.values')), digits = 2)
     acc = performance(pred, 'acc')
     accxvalue = unlist(slot(acc, 'x.values'))[-1]
     accyvalue = unlist(slot(acc, 'y.values'))[-1]
     maxacc = round(max(accyvalue),digits = 2)
     bestscore = round(accxvalue[[which.max(accyvalue)]],digits = 2)
     
     filename = paste(plotstr, rocname, ".png", sep = '')
     png(file=filename, bg="white")
     plot(perf, lwd = 2, colorize=T)
     lines(x = c(0,1), y = c(0,1), col = 'black',lwd = 1)
     text(x = 0.75, y = 0.25, labels = paste("auc = ",auc,sep = ''))
     title(sub=paste("Threshold = ", bestscore, " Accuracy = ",maxacc, sep = ''))
     dev.off()

     
#     library(ROCR)
#     library(ggplot2)
#     if(!is.null(plotstr) & !is.null(plotname)){
#       
#       filename = paste(plotstr, plotname, ".png", sep = '')
#       ggsave(file = filename)
#       png(file=filename, bg="white")
#       
#       rocplot<-ggplot(roc)+
#         theme(panel.grid=element_blank(), 
#               panel.background = element_rect(fill  = 'transparent'),
#               panel.border = element_rect(fill  = 'transparent', color = "black"))+
#               geom_line(aes(x = xvalue, y = yvalue, colour = method)) +
#               labs(x = "False Positive Rate", y="True Positive Rate")+
#               annotate("text", x = 0.75, y=0.25, label=paste("AUC = ",auc,sep = ''))
#       rocplot
#       dev.off()
#    }

    ErrorMsg = NULL

  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in plot:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
   # return
   return(list(AUC = auc, BestAccuracy = maxacc, BestThreshold = bestscore))
}

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

Sig<-function(v){
  v = as.numeric(v)
  significance = c()
  for(i in v){
    if(is.na(i)){
      significance = c(significance, NA)
      next
    }
    
    if(i<=0.01){
      significance = c(significance, '***')
      next
    }
    
    if(i>0.01 & i<=0.05){
      significance = c(significance, '**')
      next
    }
    
    if(i>0.05 & i<=0.1){
      significance = c(significance, '*')
      next
    }
    if(i>0.1){
      significance = c(significance, " ")
      next
    }
    
  }
  return(significance)
}


BinaryLogistic<-function(dataset, rowname = NULL, colname = NULL, yname=NULL, xname=NULL, 
                         formulastring=NULL, plotstr = NULL, rocname = NULL){
                         
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
  dataset = DataCheck(dataset)
  #############################################################################################
  ######################################## parameters check ###################################
  # check yname
  yname = iconv(yname, to = 'gbk')
  if(length(yname) != 1){
    return(list(ErrorMsg = paste("Error in yname: exactly one allowed, you have ", length(yname))))
  }
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
  xname = iconv(xname, to = 'gbk')
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
    options(digits = 3L)
    SummResult = summary(result)
    Formula = formulastring
    RegResult = SummResult$coefficients
  
    RegResultRowName = rownames(RegResult)
    RegResultColName = colnames(RegResult)
  
    
    significance = Sig(RegResult[,"Pr(>|z|)"])
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
   
    if(!is.null(plotstr) & !is.null(rocname)){
      score = predict(result,type='response')  # classification result
      label = dataset[[yname]]
      RocResult = RocPlot(score, label, plotstr = plotstr, rocname = rocname)
    }
    else{
      RocResult = list()
    }

    ErrorMsg = NULL
    
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in plot:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # return result
  
  return(c(list(RegResultRowName = RegResultRowName, RegResultColName = RegResultColName,
              RegResult = RegResult, AIC = AIC), RocResult))
}

# codes below are testing codes
rm(list=ls(all=TRUE))
String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
setwd(String)
data = read.csv('datacon.csv',stringsAsFactors=F, na.strings = c(""))
dataset = data
yname = 'dp_nervus'
xname = c('pat_sex','pat_age','dp_diff')
plotstr = String
rocname = "ROC"
formulastring = 'dp_nervus ~ pat_sex + pat_age'
a = BinaryLogistic(dataset, yname = yname, xname = xname, formulastring = formulastring,
                   plotstr = plotstr, rocname = rocname)


