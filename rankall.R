sort.Hospital <- function(state, outcome, data.measure = data.frame(), num = "best"){
  outcome.option <- c("heart attack", "heart failure", "pneumonia")
  
  if (outcome == outcome.option[1]){
    cm.filtered <- subset(data.measure,grepl(state,data.measure$State), select = c(1,3,2))
  } else if (outcome == outcome.option[2]){
    cm.filtered <- subset(data.measure,grepl(state,data.measure$State), select = c(1,4,2))
  } else {
    cm.filtered <- subset(data.measure,grepl(state,data.measure$State), select = c(1,5,2))
  }
  
  #sort by measure and name
  cm.sorted<-cm.filtered[order(as.numeric(cm.filtered[,2]),cm.filtered[,1]),]
  
  #product result
  cm.length <- length(cm.sorted$Hospital.Name)
  cm.length.exl.na <- length(cm.sorted$Hospital.Name[!is.na(cm.sorted[,2])])
  
  if(is.numeric(num) > cm.length){
    cm.sorted[num,1]<-NA
    cm.sorted[num,2]<-NA
    cm.sorted[num,3]<-state
    return (cm.sorted[num,])
  } else if(num == "worst") {
    cm.sorted[cm.length.exl.na,]
  } else if(num == "best") {
    cm.sorted[1,]
  } else{
    cm.sorted[num,3]<-state
    return (cm.sorted[num,])
  }
} 

rankall <- function(outcome, num = "best"){
  care.measure <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", colClasses = "character")
  state.v <- sort(unique(care.measure$State))
  outcome.option <- c("heart attack", "heart failure", "pneumonia")
  care.sel <- as.data.frame(care.measure[,c(2,7,11,17,23)])

  if(!(outcome %in% outcome.option)){
    stop("invalid outcome")
  }
  
  temp.out <- lapply(state.v, FUN = sort.Hospital,outcome = outcome, data.measure = care.sel, num = num)
  result<-matrix(unlist(temp.out),nrow = 54,byrow = T)
  colnames(result) <- c("Hospital Name","Measure Rate","State")
  result[,c(1,3)]
}