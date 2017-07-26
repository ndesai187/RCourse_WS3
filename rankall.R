rankhosp <- function(state, outcome, num = "best"){
  care.measure <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", colClasses = "character")
  outcome.option <- c("heart attack", "heart failure", "pneumonia")
  
  if(!(outcome %in% outcome.option)){
    stop("invalid outcome")
  } 
  
  if (outcome == outcome.option[1]){
    cm.filtered <- subset(care.measure,grepl(state,care.measure$State), select = c(2,11))
  } else if (outcome == outcome.option[2]){
    cm.filtered <- subset(care.measure,grepl(state,care.measure$State), select = c(2,17))
  } else {
    cm.filtered <- subset(care.measure,grepl(state,care.measure$State), select = c(2,23))
  }
  #sort by measure and name
  cm.sorted<-cm.filtered[order(as.numeric(cm.filtered[,2]),cm.filtered[,1]),]
  
  #product result
  cm.length <- length(cm.sorted$Hospital.Name)
  cm.length.exl.na <- length(cm.sorted$Hospital.Name[!is.na(cm.sorted[,2])])
  
  
  if(is.numeric(num) > cm.length){
    return(NA);
  } else if(num == "worst") {
    cm.sorted[cm.length.exl.na,]
  } else if(num == "best") {
    cm.sorted[1,]
  } else{
    cm.sorted[num,]
  }
} 

rankall <- function(outcome, num = "best"){
  care.measure <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", colClasses = "character")
  state.v <- sort(unique(care.measure$State))
  
  
  result <- lapply(state.v, FUN = rankhosp, outcome=outcome, num = num)
  print(length(result))
  print(typeof(result))
  print(result$Hospital.name)
  result
}    