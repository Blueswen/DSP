##args <-c("-query","max","-files",
##         "~/Documents/NCCU/1042/DSP/HW/Homework1/Data/test.1.csv",
##         "~/Documents/NCCU/1042/DSP/HW/Homework1/Data/test.2.csv",
##         "~/Documents/NCCU/1042/DSP/HW/Homework1/Data/test.3.csv",
##         "-out","out.csv")
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw1.R -query max/min -files file1 file2 –out out.csv", call.=FALSE)
} else {
  argName <- c("-query","-files","-out")
  argRng <- list( c(1,1) , c(1,256) , c(1,1)) ## Unlimited can use Inf
  argList <- list()
  query <- c()
  files <- c()
  out <- c()
  input <- list()
  argindex <- 0
  
  ## Init arguments data frame
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
        stop("Wrong Argument. USAGE: Rscript hw1.R -query max/min -files file1 file2 –out out.csv", call.=FALSE)
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
  
  ## Assing arguments
  query <- argList[["-query"]][[1]]
  files <- argList[["-files"]]
  out <-  argList[["-out"]][[1]]
  
  
  ## Read input files
  for( i in c(1:length(files)) ){
    input[[i]] <- read.table(files[[i]],sep=",",stringsAsFactors=F,header=T)
  }
  
  ## Rename files
  for( i in c(1:length(files)) ){
    files[[i]] <- substr( files[[i]], 1, nchar(files[[i]])-4)
  }
  
  ## Init output and Find max or min
  output <- data.frame(Type=character(0))
  weight <- data.frame(Type="weight")
  height <- data.frame(Type="height")
  fw <- c()
  fh <- c()
  for( i in c(1:length(files)) ){
    output[[files[i]]] <- numeric(0)
    if( query == "max" ){
      weight[[files[i]]] <- max(input[[i]]$weight)
      height[[files[i]]] <- max(input[[i]]$height)
      fw <- c( fw, max(input[[i]]$weight))
      fh <- c( fh, max(input[[i]]$height))
    }
    else if( query == "min" ){
      weight[[files[i]]] <- min(input[[i]]$weight)
      height[[files[i]]] <- min(input[[i]]$height)
      fw <- c( fw, min(input[[i]]$weight))
      fh <- c( fh, min(input[[i]]$height))
    }
  }
  output[[query]] <- character(0)
  if( query[[1]] == "max" ){
    weight[[query]] <- files[[which.max(fw)]]
    height[[query]] <- files[[which.max(fh)]]
  }
  else if( query == "min" ){
    weight[[query]] <- files[[which.min(fw)]]
    height[[query]] <- files[[which.min(fh)]]
  }
  output <- rbind(output, weight, height)
  
  ## Write output files
  write.table(output, file=out, sep=",", row.names=FALSE, quote=FALSE)
}

