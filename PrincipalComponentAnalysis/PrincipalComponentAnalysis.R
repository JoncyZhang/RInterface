PrincipalComponentAnalysis<-function(dataset, rowname = NULL, colname = NULL, cor = FALSE, 
                                     plotstr = NULL, plotname = NULL){
  #-------------------------------------------------------------------------------------------------
  # File: PrincipalComponentAnalysis.R
  # Version 1.0.0
  # By chao zhang 
  # 2017-03-14
  # E-mail:chaozhang1209@gmail.comn 
  #-------------------------------------------------------------------------------------------------
  # Restricted Materials - Property of Deepaint Co.,Ltd.
  #
  # Use, duplication or disclosure is restricted 
  #-------------------------------------------------------------------------------------------------
  # PrincipalComponentAnalysis.R -  Perform Principal Component Analysis of dataset. 
  #                                 1.cor is a logical value indicating whether the calculation should 
  #                                   use the correlation  matrix or the covariance matrix. 
  #                                   (The correlation matrix can only be used if there are no constant 
  #                                   variables.)
  #               
  #                                        
  # To run this file, call it in PrincipalComponentAnalysis.R 
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
    return(list(ErrorMsg = paste("Error in input data: at least 2 column allowed, you have", ncol(dataset))))
  }
  
  #############################################################################################
  ######################################## parameters check ###################################
  # Only numeric are allowed
  for(i in colnames(dataset)){
    if(!is.numeric(dataset[[i]])){
      return(list(ErrorMsg =paste("Error in variable", i, ": not numeric")))
    }    
  }
  
  # check cor
  if(!is.logical(cor)){
    return(list(ErrorMsg = "Error in cor: must be a logical"))
  }
  
  #check plotstr
  if(is.null(plotstr)){
    return(list(ErrorMsg = "Error in plotstr: doesn't exist"))
  }
  
  #check plotstr
  if(is.null(plotstr)){
    return(list(ErrorMsg = "Error in plotstr: doesn't exist"))
  }
  
  #check plotname
  if(is.null(plotname)){
    return(list(ErrorMsg = "Error in plotname: doesn't exist"))
  }
  
  #############################################################################################
  ###################################### perform PCA####### ###################################
  # perform PCA
  ErrorMsg<-tryCatch({
    pca<-princomp(dataset, cor)
    ErrorMsg = NULL
  }, 
  error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R princomp function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  # abstracting information from test result
  ErrorMsg<-tryCatch({
    pcasummary = summary(pca,loadings=TRUE) 
    pcapredict = predict(pca)
    SD = t(as.matrix(pca$sdev))
    rownames(SD) = "SD"
    Deviation = t(as.matrix((pca$sdev)^2))
    rownames(Deviation) = "Deviation"
    ProportionalVar  =Deviation/sum(Deviation)
    rownames(ProportionalVar) = "ProportionalVar"
    CumulativeVar  = t(as.matrix(cumsum((pca$sdev)^2)/sum((pca$sdev)^2)))
    rownames(CumulativeVar) = "CumulativeVar"
    
    PCAResult = rbind(SD, Deviation, ProportionalVar, CumulativeVar)
    PCAResultRowName = rownames(PCAResult) 
    PCAResultColName = colnames(PCAResult)
    ErrorMsg = NULL
  }, 
  error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in R princomp function:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  
  #############################################################################################
  ########################################## plot #############################################
  ErrorMsg<-tryCatch({
    filename = paste(plotstr, plotname, ".png", sep = '')
    png(file=filename, bg="white")
    screeplot(pca, type = 'line', main='Screep Plot')
    dev.off()
    ErrorMsg = NULL
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in plot:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
    return(ErrorMsg)
  }
  # return
  return(list( PCAResultRowName = PCAResultRowName, PCAResultColName = PCAResultColName, 
               PCAResult = PCAResult))
}

# codes below are testing codes
# rm(list=ls(all=TRUE))
# String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
# plotstr = paste(String,"PrincipalComponentAnalysis/",sep = '')
# plotname = 'Screep Plot'
# setwd(String)
# data(USArrests)
# USArrests
# USArrests$a = rep(1,nrow(USArrests))
# cor = FALSE
# a = PrincipalComponentAnalysis(USArrests,cor = FALSE,
#                                plotstr = plotstr, plotname = plotname)



