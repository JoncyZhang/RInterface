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

UinFactorAnova<-function(dataset, numvar = NULL, chavar = NULL,
                         formulastring = NULL, plotstr = NULL, boxname = NULL){
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
  dataset = DataCheck(dataset)
  #############################################################################################
  ######################################## parameters check ###################################
  # check chavar
  chavar = iconv(chavar, to = 'gbk')
  if(!(chavar %in% colnames(dataset))){
    return(list(ErrorMsg = paste("Error in chavar:", chavar, ": not exist")))
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
  
  # check numvar
  numvar = iconv(numvar, 'gbk')
  if(!(numvar %in% colnames(dataset))){
    return(list(ErrorMsg = paste("Error in numvar:ß", numvar, ": not exist")))
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
  
  # check formulastring
  if(is.null(formulastring)){
    formulastring = paste(numvar, paste(chavar, collapse = '+'), sep ="~" )
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
    AovResultRowName = c(rownames(SummResult[[1]]), "Total")
    AovResultColName = c(colnames(SummResult[[1]]), "Significance")
    
    Df = c(SummResult[[1]]$Df, sum(SummResult[[1]]$Df))
    SumSq = round(c(SummResult[[1]]$`Sum Sq`, sum(SummResult[[1]]$`Sum Sq`)), digits=2)  
    MeanSq = round(c(SummResult[[1]]$`Mean Sq`, SumSq[length(SumSq)]/Df[length(Df)]), digits=2)  
    FStatistic = c(SummResult[[1]]$`F value`, NA)
    PValue =  c(SummResult[[1]]$`Pr(>F)`, NA)
    Significance = Sig(PValue)
    
    AovResult = data.frame(Df, SumSq, MeanSq, FStatistic, PValue, Significance, row.names = AovResultRowName)
    AovResult = as.matrix(AovResult)  
    
    ErrorMsg = NULL
  }, error = function(e){
    ErrorMsg = list(ErrorMsg = paste('Error in abstracting information:', conditionMessage(e)))
  })
  if(!is.null(ErrorMsg)){
   return(ErrorMsg)
  }
  
  #############################################################################################
  ######################################## plot ###############################################
  if(!is.null(plotstr) & !is.null(boxname)){
    filename = paste(plotstr, boxname , ".png", sep = '')
    png(file=filename, bg="white") 
    library(ggplot2)
    AovBoxPlot = ggplot(dataset)+
      geom_boxplot(aes_string(x= chavar, y= numvar, fill = chavar))+
      theme(axis.text.x= element_text(angle = 90, vjust=0, hjust=1))+
      labs(x = chavar, y = numvar) +
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
  return(list(AovResultRowName = AovResultRowName, AovResultColName =AovResultColName,
              AovResult = AovResult))
              
}

# codes below are testing codes
rm(list=ls(all=TRUE))
String = "/Users/joncy/WorkSpace/RStudio/Deepaint/"
setwd(String)
data = read.csv('datacon.csv',stringsAsFactors=F, na.strings = c(""))
dataset = data[c(1,2)]
numvar = 'pat_age'
chavar = 'pat_sex'
plotstr = String
boxname = 'boxplot'
a = UinFactorAnova(dataset, numvar = numvar, chavar = chavar, plotstr = plotstr, boxname = boxname)
