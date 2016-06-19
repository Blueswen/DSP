library(e1071)
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
        stop("Wrong Argument. USAGE: Rscript hw4.R -input Archaeal_tfpssm.csv -fold n -out performance.csv", call.=FALSE)
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

divideByLoc_func <- function(locList, data){
  result <- list()
  for( i in c(1:length(locList))){
    result[[locList[[i]]]] <- c()
  }
  for( i in c(1:length(data[[1]]))){
    result[[data[[2]][[i]]]] <- c(result[[data[[2]][[i]]]], i)
  }
  return(result)
}

tableAccuracy_func <- function(table){
  len <- sqrt(length(table))
  error <- 0
  TP <- 0
  for ( i in 1:len ){
    for( j in 1:len ){
      if(i==j){
        TP <- TP+table[(i-1)*4+j]
      }
      else{
        error <- error+table[(i-1)*4+j]
      }
    }
  }
  return(round(TP/(TP+error),digits = 4))
}

tablePrecision_func <- function(table){
  len <- sqrt(length(table))
  FP <- 0
  TP <- 0
  for ( i in 1:len ){
    for( j in 1:len ){
      if(i==j){
        TP <- TP+table[(i-1)*4+j]
      }
      else{
        error <- error+table[(i-1)*4+j]
      }
    }
  }
  return(TP/(TP+FP))
}

tableRecall_func <- function(table){
  len <- sqrt(length(table))
  FN <- 0
  TP <- 0
  for ( i in 1:len ){
    for( j in 1:len ){
      if(i==j){
        TP <- TP+table[(i-1)*4+j]
      }
      else {
        FN <- FN+table[(i-1)*4+j]
      }
    }
  }
  return(TP/(TP+FN))
}

tableF_func <- function(table){
  
}

if(Rstudio){
  args <-c("-fold",5,
           "-input",
           "~/Documents/NCCU/1042/DSP/HW/Homework4/Data/Archaeal_tfpssm.csv",
           "-out","~/Documents/NCCU/1042/DSP/HW/Homework4/Results/performance.csv")
} else{
  args = commandArgs(trailingOnly=TRUE) 
}
if (length(args)==0) {
  stop("USAGE: Rscript hw4.R -input Archaeal_tfpssm.csv -fold n -out performance.csv", call.=FALSE)
} else {
  
  ## Get arguments
  argName <- c("-input","-fold","-out")
  argRng <- list( c(1,1) , c(1,1) , c(1,1)) ## Unlimited can use Inf
  argList <- argParser_func(argName,argRng)
  
  locList <- c("CP","CW","EC","IM")
  calibrationTable <- c()
  trainingTable <- list()
  
  input <- argList[["-input"]]
  fold <- as.numeric(argList[["-fold"]])
  out <- argList[["-out"]]
  
  if( is.na(fold) ){
    stop("Wrong Argument. The argument of fold must be a numeric. USAGE: Rscript hw4.R -input Archaeal_tfpssm.csv -fold n -out performance.csv", call.=FALSE)
  }
  
  ## Read input files
  if (exists("org_data") == FALSE){
    print("Loading file.")
    org_data <- read.table(input[[1]], sep=",",stringsAsFactors=F,header=F)
  }
  else{
    print("File has been loaded. Skip loading file.")
  }
  
  ## Divide data by loc
  locTable <- divideByLoc_func(locList, org_data)
  
  ## Choose training and calibration
  tmpLocTable <- locTable
  for( j in 1:fold ){
    trainingTable[[j]] <- c(numeric())
  }
  for( i in 1:length(locList) ){
    for( j in 1:floor(length(locTable[[locList[[i]]]])/10) ){
      current <- sample(1:length(tmpLocTable[[locList[[i]]]]),1)
      calibrationTable <- c(calibrationTable, tmpLocTable[[locList[[i]]]][[current]])
      tmpLocTable[[locList[[i]]]] <- tmpLocTable[[locList[[i]]]][-current]
    }
    ## Devide to n fold
    for( j in 1:length(tmpLocTable[[locList[[i]]]]) ){
      trainingTable[[j%%fold+1]] <- c(trainingTable[[j%%fold+1]], tmpLocTable[[locList[[i]]]][[j]])
    }
  }
  
  ## K-fold cross validation
  bestModel <- NULL
  for( i in 1:fold ){
    ## i is test
    test <- org_data[trainingTable[[i]],]
    x.test <- test[,3:5602]
    y.test <- as.factor(test[,2])
    
    ## others are train
    train <- NULL
    for( j in 1:fold ){
      if( j != i){
        if( is.null(train) ){
          train <- org_data[trainingTable[[j]],]
        }
        else{
          train <- rbind(train, org_data[trainingTable[[j]],])  
        }
      }
    }
    x.train <- train[,3:5602]
    y.train <- as.factor(train[,2])
    
    ## Train model
    print(paste("Model",i,"is trainning."))
    model <- svm(x.train,y.train)
    
    ## Predict train and test
    train_result <- predict(model,x.train)
    test_result <- predict(model,x.test)
    
    if( is.null(bestModel) ){
      bestModel <- list(model= model,
                        trainAccuracy = tableAccuracy_func(table(train_result, y.train)),
                        testAccuracy = tableAccuracy_func(table(test_result, y.test)))
    }
    else{
      if( tableAccuracy_func(table(test_result, y.test)) > bestModel$testAccuracy ){
        bestModel <- list(model= model,
                          trainAccuracy = tableAccuracy_func(table(train_result, y.train)),
                          testAccuracy = tableAccuracy_func(table(test_result, y.test)))
        ##print(paste("Current high is Model",i))
      }
    }
  }

  ## Do calibration prediction
  calibration <- org_data[calibrationTable,]
  x.cal <- calibration[,3:5602]
  y.cal <- as.factor(calibration[,2])
  cal_result <- predict(bestModel$model,x.cal)
  bestModel[["calAccuracy"]] <- tableAccuracy_func(table(cal_result, y.cal))
  
  ## Write output files
  output <- data.frame(set=character(), Accuracy=numeric())
  output <- rbind(output, data.frame( set="trainning", Accuracy= bestModel$trainAccuracy))
  output <- rbind(output, data.frame( set="calibration", Accuracy= bestModel$calAccuracy))
  output <- rbind(output, data.frame( set="test", Accuracy= bestModel$testAccuracy))
  write.table(output, file=out, sep=",", row.names=FALSE, quote=FALSE)
  
}

