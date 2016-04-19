library('ROCR') ## for AUC_func
Rstudio <- TRUE

argParser_func <- function(argName, argRng){
  argindex <- 0
  argList <- list()
  
  ## Init arguments list
  for( i in c(1:length(argName)) ){
    argList[[argName[[i]]]] <- character(0)
  }
  
  ## Parse arguments
  for( i in c(1:length(args)) ){
    isArg <- FALSE
    for( j in c(1:length(argName)) ){
      if(args[[i]] == argName[[j]]){
        argindex = j
        isArg <- TRUE
        break
      } 
    }
    if(isArg){
      next
    }
    else{
      if( substr(args[[i]],1,1) == "-"){
        stop("Wrong Argument. USAGE: Rscript hw2.R -target  male/female -query F1 AUC sensitivity specificity -files set1 set2 … setx –out out_folder", call.=FALSE)
      }
      else{
        argList[[ argName[[argindex]] ]][[ length(argList[[ argName[[argindex]] ]]) + 1  ]] <- args[[i]]
      }
    }
  }
  
  ## Check arguments Number
  for( i in c(1:length(argList)) ){
    if( length(argList[[ argName[[i]] ]]) > argRng[[i]][[2]] | length(argList[[ argName[[i]] ]]) < argRng[[i]][[1]] ){
      if( argRng[[i]][[1]] == argRng[[i]][[2]] ){
        errorStr <- paste( "Wrong Argument Number. The number of ", argName[[i]] ," must be ", argRng[[i]][[1]] ,".",sep = "") 
      }
      else{
        errorStr <- paste( "Wrong Argument Number. The number of ", argName[[i]] ," must between ", argRng[[i]][[1]] ," to ", argRng[[i]][[2]], "." ,sep = "")   
      }
      stop( errorStr, call.=FALSE)
    }
  }
  
  return(argList)
}

checkPar_func <- function(parName, parList, inputList){
  for( i in c(1:length(inputList))){
    if( inputList[[i]] %in% parList){
      next;
    }
    else{
      errorStr <- paste("Wrong ", parName, " parameter. Parameters can use in ", parName, " are ", paste(parList,collapse = ", ") ,".",sep="")
      stop( errorStr, call.=FALSE)
    }
  }
}

resultSummary_func <- function(target, data){
  summary <- list(tp=0,tn=0,fp=0,fn=0)
  for( i in c(1:length(data[[1]])) ){
    curP <- data[["prediction"]][[i]]
    curR <- data[["reference"]][[i]]
    if( curR == target ){
      if( curP == curR ){
        summary$tp = summary$tp + 1
      }
      else{
        summary$fp = summary$fp + 1
      }
    }
    else{
      if( curP == curR ){
        summary$tn = summary$tn + 1
      }
      else{
        summary$fn = summary$fn + 1
      }
    }
  }
  return(summary)
}

F1_func <-function(summary){
  precision <- summary$tp/(summary$tp+summary$fp)
  recall <- summary$tp/(summary$tp+summary$fn)
  return(2*precision*recall/(precision+recall))
}

sensitivity_func <- function(summary){
  return(summary$tp / (summary$tp + summary$fn))
}

specificity_func <- function(summary){
  return(summary$tn / (summary$tn + summary$tp))
}

AUC_func <- function(target,data,index,p){
  testor <- list(predictions<-c(),labels<-c())
  
  ## Build lables
  for( i in c(1:length(data[[1]])) ){
    if( data$reference[[i]] == target ){
      testor$labels <- c(testor$label,TRUE)
    }
    else{
      testor$labels <- c(testor$label,FALSE)
    }
  }
  
  ## Get scores
  testor$predictions <- data$pred.score
  
  pred <- prediction(testor$predictions,testor$labels)
  
  ## Plot ROC Curve Picture
  if(p){
    if (index==1){
      plot(performance(pred,"tpr","fpr"), col=rainbow(10)[[index]])
      title(main="ROC Curve of 10 Methods")
    }
    else{
      plot(performance(pred,"tpr","fpr"), add = TRUE, col=rainbow(10)[[index]])
    }
    if(index<=5){
      legend(0,1-0.04*(index-1),legend=c(paste("M",index)),col=rainbow(10)[[index]],lty=1,cex=0.4,box.lty=0)
    }
    else{
      legend(0.08,1-0.04*(index-6),legend=c(paste("M",index)),col=rainbow(10)[[index]],lty=1,cex=0.4,box.lty=0)
    } 
  }
  
  return(attributes(performance(pred,'auc'))$y.values[[1]])
}

query_func <-function(func,target,data,index){
  if(func=="F1"){
    return(F1_func(resultSummary_func(target,data)))
  }
  else if(func=="sensitivity"){
    return(sensitivity_func(resultSummary_func(target,data)))
  }
  else if(func=="specificity"){
    return(specificity_func(resultSummary_func(target,data)))
  }
  else if(func=="AUC"){
    return(AUC_func(target,data,index,TRUE))
    ##return(0)
  }
}

significantTest_func <- function(target,data){
  if( AUC_func(target,data,1,FALSE) < 0.5 ){
    return("no")
  }
  else{
    return("yes")
  }
}

firstUp <- function(str){
  return(paste(toupper(substring(str,1,1)),tolower(substring(str,2)),sep=""))
}

if(Rstudio){
  args <-c("-target","male",
          "-query","F1","AUC","sensitivity","specificity",
           "-files",
           "~/Documents/NCCU/1042/DSP/HW/Homework2/Data/set1",
           "~/Documents/NCCU/1042/DSP/HW/Homework2/Data/set2",
           "~/Documents/NCCU/1042/DSP/HW/Homework2/Data/set3",
           "~/Documents/NCCU/1042/DSP/HW/Homework2/Data/set4",
           "~/Documents/NCCU/1042/DSP/HW/Homework2/Data/set5",
           "-out","~/Documents/NCCU/1042/DSP/HW/Homework2/Results")
} else{
  args = commandArgs(trailingOnly=TRUE) 
}
if (length(args)==0) {
  stop("USAGE: Rscript hw2.R -target  male/female -query F1 AUC sensitivity specificity -files set1 set2 … setx –out out_folder", call.=FALSE)
} else {
  
  ## Get arguments
  argName <- c("-target","-query","-files","-out")
  argRng <- list( c(1,1) , c(1,4) , c(1,255), c(1,1)) ## Unlimited can use Inf
  argList <- argParser_func(argName,argRng)
  
  ## Check query and target
  parList <- c()
  parList$target <- c("Male","Female")
  parList$query <- c("F1","AUC","sensitivity","specificity")
  checkPar_func("target",parList$target ,firstUp(argList[["-target"]][[1]]))
  checkPar_func("query",parList$query ,argList[["-query"]])
  
  target <- firstUp(argList[["-target"]][[1]])
  query <- argList[["-query"]]
  files <- argList[["-files"]]
  out <- argList[["-out"]]
  
  ## Read input files
  input <- list()
  for( i in c(1:length(files)) ){
    input[[i]] <- list()
    for( j in 1:10 ){
      current <- paste(files[[i]],"/method",j,".csv",sep="");
      input[[i]][[j]] <- read.table(current,sep=",",stringsAsFactors=F,header=T)
    }
  }
  
  ## Compute Answer
  output <- list()
  output$csv <- list()
  output$plot <- list()
  for( i in c(1:length(input)) ){
    output$csv[[i]] <- data.frame()
    
    ## Init png device if query has AUC
    if( "AUC" %in% query ){
      png(filename=paste(out,"/",basename(files[[i]]),"_ROC.png",sep=""),width = 480, height = 480)
      ## Must assign the device before plot, because the recordPlot also record the device too.
      ## The replayPlot function can not replay the record to the specified or current device. It will create a new device
      ## which was recording when recordPlot was called.
    }
    
    ## Call query_func
    for( j in c(1:length(input[[i]])) ){
      current <- data.frame(method=paste("method",j))
      for( k in c(1:length(query)) ){
        res <- query_func(query[[k]],target,input[[i]][[j]],j)
        current[[query[[k]]]] <- round(res, digits = 2)
      }
      
      ## If AUC of ROC curve smaller than 0.5 is not significant
      current$significant <- significantTest_func(target,input[[i]][[j]])
      output$csv[[i]] <- rbind(output$csv[[i]],current)
    }
    
    ## Get the highest
    highest <- data.frame(method="highest",significant="nan")
    for( k in c(1:length(query)) ){
      highest[[query[[k]]]] <- paste("method",which.max(output$csv[[i]][[query[[k]]]]),sep="")
    }
    
    ## Save plot and csv to output
    output$csv[[i]] <- rbind(output$csv[[i]],highest)
    if( "AUC" %in% query ){
      output$plot[[i]] <- recordPlot() ## Record the current plot to output$plot[[i]]
    }
  }
  
  ## Write output files
  for( i in c(1:length(output$csv)) ){
    write.table(output$csv[[i]], file=paste(out,"/",basename(files[[i]]),".csv",sep=""), sep=",", row.names=FALSE, quote=FALSE)
    if( "AUC" %in% query ){
      replayPlot(output$plot[[i]]) ##Replay the plot record
      dev.off()
    }
  }
}

