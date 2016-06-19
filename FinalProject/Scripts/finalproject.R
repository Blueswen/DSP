library(e1071)
library(dummies)
Rstudio <- TRUE
QuickTest <- FALSE
Main <- TRUE

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
        stop("Wrong Argument. USAGE: Rscript finalproject.R -input kamera.csv -out performance.csv", call.=FALSE)
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

MHSim <- function(pre, ref) {
  if(length(pre)!=length(ref)){
    print("wrong number")
    return(NULL)
  }
  len <- length(pre)
  xy <- 0
  x2 <- 0
  y2 <- 0
  for (i in 1:len) {
    xy <- xy + 2*pre[i]*ref[i]
    x2 <- x2 + ref[i]*ref[i]
    y2 <- y2 + pre[i]*pre[i]
  }
  return(xy/(x2+y2)) 
}

Rsquared <- function(pre, ref) {
  if(length(pre)!=length(ref)){
    print("wrong number")
    return(NULL)
  }
  len <- length(pre)
  r2 <- 0
  SSreg <- 0
  SStotal <- 0
  ref.mean <- mean(ref)
  for (i in 1:len) {
    SSreg <- SSreg + (pre[i]-ref.mean)^2
    SStotal <- SStotal + (ref[i]-ref.mean)^2
  }
  r2 <- SSreg/SStotal
  return(r2)
}

AdjRsquared <- function(r2, n, p) {
  return( 1 - (1-r2)*(n-1)/(n-p-1) )
}

divideBy_func <- function(df, ind) {
  dataList <- levels(as.factor(df[[ind]]))
  result <- list()
  for (i in 1:length(dataList) ) {
    result[[i]] <- c(numeric())
  }
  for (i in 1:length(df[[ind]]) ) {
    for (j in 1:length(dataList)) {
      if( as.character(df[[ind]][[i]]) == as.character(dataList[[j]]) ) {
        result[[j]] <- c( rownames(df[i,]), result[[j]] )
        next
      }
    }
  }
  return(result)
}

dateToMonth <- function(df, colname) {
  f <- function(x) {
    return(as.numeric(strsplit(x,"-")[[1]][[2]]))
  }
  df[colname]<-apply(df, match(colname, colnames(df)), f)
  return(df)
}

tzToNumeric <- function(df, colname) {
  #tzList <- levels(as.factor(df[[colname]]))
  tzList <- c("[0,4)", "[4,8)", "[8,12)", "[12,16)", "[16,20)", "[20,24)" )
  for (i in 1:length(df[[colname]])) {
    for (j in 1:length(tzList) ) {
      if( df[[colname]][i] == tzList[[j]] ) {
        df[[colname]][i] <- j
        break
      }
    }
  }
  df[colname]<-lapply(df[colname], function(x) as.numeric(x))
  return(df)
}

if(Rstudio){
  args <-c("-input",
           "~/Documents/NCCU/1042/DSP/HW/FinalProject/Data/kamera.csv",
           "-out","~/Documents/NCCU/1042/DSP/HW/FinalProject/Results/performance.csv")
} else{
  args = commandArgs(trailingOnly=TRUE) 
}
if (length(args)==0) {
  stop("USAGE: Rscript finalproject.R -input kamera.csv -out performance.csv", call.=FALSE)
} else {
  
  ## Get arguments
  argName <- c("-input","-out")
  argRng <- list( c(1,1) , c(1,1)) ## Unlimited can use Inf
  argList <- argParser_func(argName,argRng)
  
  input <- argList[["-input"]]
  out <- argList[["-out"]]
  
  ## Read input files
  if (exists("org_data") == FALSE){
    print("Loading file.")
    org_data <- read.table(input[[1]], sep=",",stringsAsFactors=F,header=T)
  }
  else{
    print("File has been loaded. Skip loading file.")
  }
  
  if (QuickTest) {
    qt.featureList <- c("date", "tz", "Hospital_PK", "Level")
    #qt.targetList <- c("PDR", "PBR", "A12", "A15", "Light", "total")
    qt.targetList <- c("PDR", "PBR", "total")
    qt.col <- c(qt.featureList, qt.targetList)
    qt.df <- org_data[qt.col]
    qt.df <- qt.df[qt.df$Hospital_PK==1,]
    qt.models <- list()
    qt.train_result <- list()
    qt.test_result <- list()
    qt.x.train <- list()
    qt.y.train <- list()
    qt.results <- data.frame( target<-character(),
                              type<-character(),
                              note<-character(),
                              MHS<-numeric(),
                              RSquared<-numeric(),
                              AdjustedRSquared<-numeric())
    
    qt.df <- dateToMonth(qt.df, "date")
    qt.df <- tzToNumeric(qt.df, "tz")
    qt.df$date <- factor(qt.df$date)
    qt.df$tz <- factor(qt.df$tz)
    qt.df <- dummy.data.frame(qt.df)
    
    
    qt.featureList <- c(colnames(get.dummy(qt.df,'date')),
                        colnames(get.dummy(qt.df,'tz')),
                        "Level")
    qt.col <- c(qt.featureList, qt.targetList)
    qt.df <- qt.df[qt.col]
    
    qt.pick_number <- floor(length(qt.df[[1]])/2)
    qt.trainid <- sample(nrow(qt.df), qt.pick_number)
    qt.traindf <- qt.df[ qt.trainid, ]
    qt.testdf <- qt.df[ -qt.trainid, ]
    
    
    
    qt.models <- list()
    for (i in 1:length(qt.targetList)) {
      # without remove feature
      qt.models[[i]] <- svm(qt.traindf[qt.featureList],qt.traindf[qt.targetList[[i]]], type = "eps-regression")
      qt.train_result[[i]] <- predict(qt.models[[i]], qt.traindf[qt.featureList])
      qt.test_result[[i]] <- predict(qt.models[[i]], qt.testdf[qt.featureList])
      qt.results <- rbind( qt.results, data.frame( target = qt.targetList[[i]],
                                                   type = "train",
                                                   note = "remove none",
                                                   MHS = MHSim(qt.train_result[[i]], qt.traindf[[qt.targetList[[i]]]]), 
                                                   RSquared = Rsquared(qt.train_result[[i]], qt.traindf[[qt.targetList[[i]]]]),
                                                   AdjustedRSquared = AdjRsquared(Rsquared(qt.train_result[[i]], qt.traindf[[qt.targetList[[i]]]]), qt.pick_number, 5)))
      qt.results <- rbind( qt.results, data.frame( target = qt.targetList[[i]],
                                                   type = "test",
                                                   note = "remove none",
                                                   MHS = MHSim(qt.test_result[[i]], qt.testdf[[qt.targetList[[i]]]]), 
                                                   RSquared = Rsquared(qt.test_result[[i]], qt.testdf[[qt.targetList[[i]]]]),
                                                   AdjustedRSquared = AdjRsquared(Rsquared(qt.test_result[[i]], qt.testdf[[qt.targetList[[i]]]]), qt.pick_number, 5)))
    }
    
    qt.models <- list()
    for (i in 1:2) {
      qt.models[[i]] <- svm(qt.traindf[qt.featureList],qt.traindf[qt.targetList[[i]]], type = "eps-regression")
      qt.train_result[[i]] <- predict(qt.models[[i]], qt.traindf[qt.featureList])
      qt.test_result[[i]] <- predict(qt.models[[i]], qt.testdf[qt.featureList])
      qt.traindf[paste("pre",qt.targetList[[i]],sep="")] <- qt.train_result[[i]]
      qt.testdf[paste("pre",qt.targetList[[i]],sep="")] <- qt.test_result[[i]]
    }
    qt.featureList <- c(qt.featureList, "prePBR", "prePDR")
    for (i in 3:length(qt.targetList)) {
      qt.models[[i]] <- svm(qt.traindf[qt.featureList],qt.traindf[qt.targetList[[i]]], type = "eps-regression")
      qt.train_result[[i]] <- predict(qt.models[[i]], qt.traindf[qt.featureList])
      qt.test_result[[i]] <- predict(qt.models[[i]], qt.testdf[qt.featureList])
      qt.results <- rbind( qt.results, data.frame( target = qt.targetList[[i]],
                                                   type = "train",
                                                   note = "add PDR, PBR",
                                                   MHS = MHSim(qt.train_result[[i]], qt.traindf[[qt.targetList[[i]]]]), 
                                                   RSquared = Rsquared(qt.train_result[[i]], qt.traindf[[qt.targetList[[i]]]]),
                                                   AdjustedRSquared = AdjRsquared(Rsquared(qt.train_result[[i]], qt.traindf[[qt.targetList[[i]]]]), qt.pick_number, 5)))
      qt.results <- rbind( qt.results, data.frame( target = qt.targetList[[i]],
                                                   type = "test",
                                                   note = "add PDR, PBR",
                                                   MHS = MHSim(qt.test_result[[i]], qt.testdf[[qt.targetList[[i]]]]), 
                                                   RSquared = Rsquared(qt.test_result[[i]], qt.testdf[[qt.targetList[[i]]]]),
                                                   AdjustedRSquared = AdjRsquared(Rsquared(qt.test_result[[i]], qt.testdf[[qt.targetList[[i]]]]), qt.pick_number, 5)))
    }
    ## Write output files
    #write.table(qt.results, file=out, sep=",", row.names=FALSE, quote=FALSE)
  }
  
  
  if (Main) {
    # 1~6 are features, 7~10 are prediction targets
    main.col <- c("date", "tz", "Hospital_PK", "Level", "PDR", "PBR", "A12", "A15", "Light", "total")
    #main.featureList <- c("date", "tz", "Hospital_PK", "Level", "PDR", "PBR")
    main.featureList <- c("date", "tz", "Hospital_PK", "Level")
    #main.addFeature <- c("prePDR","prePBR")
    main.targetList <- c("A12", "A15", "Light", "total")
    #main.targetList <- c("PDR", "PBR", "total")
    main.trainpercent <- c(0.9, 0.8, 0.7, 0.6, 0.5)
    #main.trainpercent <- c(0.9)
    main.folds <- c(5, 10)
    main.trainTables <- list()
    main.pickTablesID <- list()
    main.calibrationTables <- list()
    main.df <- org_data[c(main.featureList, main.targetList)]
    main.bestmodels <- list()
    main.results <- data.frame( target <- character(),
                                TrainingPercent <- character(),
                                folds <- numeric(),
                                testMHS <- numeric(),
                                trainMHS <- numeric(),
                                calibrationMHS <- numeric(),
                                AdjustedRSquared <- numeric())
    
    main.df <- dateToMonth(main.df, "date")
    main.df <- tzToNumeric(main.df, "tz")
    
    main.df$date <- factor(main.df$date)
    main.df$Hospital_PK <- factor(main.df$Hospital_PK)
    main.df$tz <- factor(main.df$tz)
    main.df <- dummy.data.frame(main.df)
    
    
    main.featureList <- c(colnames(get.dummy(main.df,'date')),
                        colnames(get.dummy(main.df,'Hospital_PK')),
                        colnames(get.dummy(main.df,'tz')),
                        "Level")
    main.col <- c(main.featureList, main.targetList)
    
    
    for (i in 1:length(main.trainpercent)) {
      # Choose Train and Calibration Data
      main.pick_number <- length(main.df[[1]])*main.trainpercent[[i]]
      main.pickTablesID <- sample(nrow(main.df), main.pick_number)
      main.trainTables[[i]] <- main.df[ main.pickTablesID, ]
      main.calibrationTables[[i]] <- main.df[ -main.pickTablesID, ]
      main.bestmodels[[i]] <- list()
      
      # Do K-fold Cross Validation
      for (j in 1:length(main.folds)) {
        main.bestmodels[[i]][[j]] <- list()
        for (l in 1:length(main.targetList)) {
          main.bestmodels[[i]][[j]][[l]] <- list()
        }
        
        # start predict real target
        for (k in 1:main.folds[j]) {
          
          start <- floor(length(main.pickTablesID)/main.folds[j])*(k-1)+1
          end <- floor(length(main.pickTablesID)/main.folds[j])*k+1
          if( end-1 >= length(main.pickTablesID)){
            end <- length(main.pickTablesID)
          }
          # Train different model for different targets
          for (l in 1:length(main.targetList)) {
            current.train <- main.trainTables[[i]][start:end, ]
            current.train.x <- current.train[, c(main.featureList)]
            current.train.y <- current.train[, main.targetList[[l]]]
            current.test <- main.trainTables[[i]][-(start:end), ]
            current.test.x <- current.test[, c(main.featureList)]
            current.test.y <- current.test[, main.targetList[[l]]]
            
            current.model <- svm(current.train.x, current.train.y, type = "nu-regression")
            
            current.train.result <- predict(current.model, current.train.x)
            current.test.result <- predict(current.model, current.test.x)
            if( length(main.bestmodels[[i]][[j]][[l]]) == 0 ){
              main.bestmodels[[i]][[j]][[l]] <- list(model = current.model,
                                                     trainMHS = MHSim(current.train.result, current.train.y),
                                                     testMHS = MHSim(current.test.result, current.test.y),
                                                     AdjustedRSquared = AdjRsquared(Rsquared(current.test.result, current.test.y),
                                                                                    main.pick_number, length(main.featureList)))
            }
            else{
              if( MHSim(current.test.result, current.test.y) > main.bestmodels[[i]][[j]][[l]]$testMHS ){
                main.bestmodels[[i]][[j]][[l]] <- list(model = current.model,
                                                       trainMHS = MHSim(current.train.result, current.train.y),
                                                       testMHS = MHSim(current.test.result, current.test.y),
                                                       AdjustedRSquared = AdjRsquared(Rsquared(current.test.result, current.test.y),
                                                                                      main.pick_number, length(main.featureList)))
              }
            }
            print(paste("Finish",main.trainpercent[[i]]*100,"% for training",main.folds[j],"fold's fold",k,"perdict",main.targetList[[l]]))
          }# Finish different target
        }# Finish K-fold Cross Validation
        
        for (l in 1:length(main.targetList)) {
          calibartion <- main.calibrationTables[[1]]
          calibartion.x <- calibartion[, c(main.featureList)]
          calibartion.y <- calibartion[, main.targetList[[l]]]
          calibration.result <- predict(main.bestmodels[[i]][[j]][[l]]$model, calibartion.x)
          main.results <- rbind(main.results, data.frame(target = main.targetList[[l]],
                                                         TrainingPercent = main.trainpercent[[i]],
                                                         folds = main.folds[[j]],
                                                         trainMHS = main.bestmodels[[i]][[j]][[l]]$trainMHS,
                                                         testMHS = main.bestmodels[[i]][[j]][[l]]$testMHS,
                                                         calibrationMHS = MHSim(calibration.result, calibartion.y),
                                                         AdjustedRSquared = main.bestmodels[[i]][[j]][[l]]$AdjustedRSquared))
        }
      }
    }
    ## Write output files
    write.table(main.results, file=out, sep=",", row.names=FALSE, quote=FALSE)
  }
  
}

