UinFactorAnova<-function(dataset, rowname = NULL, colname = NULL,  yname = NULL, xname = NULL,
                         formulastring = NULL, plotstr = NULL, plotname = NULL){
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
  # UinFactorAnova.R -  Perform one-way factorial variance analysis of dataset.
  #            1. two columns are stirctly required in dataset. One of the column is a string vector
  #               representing factors, while another is numeric vector. 
  #            2. strongly suggest giving formulastring by user, otherwise system will treat string 
  #               vector as response variable and numeric vector as independent variable.
  #            3. two kinds of results(AovSummary and AovResult) are returned by list. AovSummary is
  #               suitable for displaying on screen, while AovResult conatains all details of Anova,
  #               such qr matrix, coefficients. By the way, AovResult is also a list which can be 
  #               reconginzed by rpy2.py. 
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
  if(length(xname) != 1){
    return(list(ErrorMsg = paste("Error in independent variable: exactly one allowed, you have ", length(xname))))
  }
  if(!(xname %in% colnames(dataset))){
    return(list(ErrorMsg = paste("Error in independent variable:", xname, "not exist")))
  }
  dataset[[xname]] = as.factor(dataset[[xname]])
  
  # check yname
  if(length(yname) != 1){
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
    AnoResultRowName = "Estimate"
    AnoResultColName = colnames(AnoResult)
    
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
  
  #############################################################################################
  ######################################## plot ###############################################
  if(!is.null(plotstr) & !is.null(plotname)){
    filename = paste(plotstr, plotname , ".png", sep = '')
    png(file=filename, bg="white") 
    
    library(ggplot2)
    AovBoxPlot = ggplot(dataset)+
      geom_boxplot(aes_string(x= xname, y= yname, fill = xname))+
      theme(axis.text.x= element_text(angle = 90, vjust=0, hjust=1))+
      labs(x = xname, y = yname) +
      theme(axis.title.x=element_text(size=12, face="bold", colour="black"), 
            axis.title.y=element_text(size=12, face="bold", colour="black"))
    
    print(AovBoxPlot)
    dev.off()
  }
  
  # grid.newpage()
  # pushviewport(viewport(layout = grid.layout(2,1)))
  # vplayout <- function(x, y){
  #   viewport(layout.pos.row = x, layout.pos.col = y)
  # }
  
  
  # return results
  return(list(AnoResultRowName = AnoResultRowName, AnoResultColName = AnoResultColName, 
              AnoResult = AnoResult,  FStatistic = FStatistic,
              FDf1 = FDf1, FDf2 = FDf2, FPValue = FPValue))
}

# codes below are testing codes
# rm(list=ls(all=TRUE))
# String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
# setwd(String)
# data = read.csv('datacon.csv',stringsAsFactors=F, na.strings = c(""))
# dataset = data
# yname = 'pat_age'
# xname = 'pat_sex'
# plotstr = paste(String, "Anova/", sep = '')
# plotname = 'boxplot'
# a = UinFactorAnova(dataset, yname = yname, xname = xname, plotstr = plotstr, plotname = plotname)
