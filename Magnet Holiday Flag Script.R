library("readxl")
library(rstudioapi)
library(dplyr)
library(lubridate)
library(readxl)
library(openxlsx)
library(bit64)
library(readr)
library(zoo)
require(dplyr)

setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path)))
setwd("../Data/")

mhf<- read_excel("Magnet Holiday Flag.xlsx")

mhf <- mhf[ -c(6) ]
mhf[,5][is.na(mhf[,5])] = -999

j<-6
for(i in 1:52){
  if(mhf[i,5]==-999){
    
  } else {
    mhf[,j]=""
    names(mhf)[j] <- mhf[i,5]
    j=j+1
  }
}

p <- ncol(mhf)
for(k in 1:p)
  if (length(strsplit(names(mhf)[k],split = ",")[[1]]) >1){
    for ( i in 1:length(strsplit(names(mhf)[k],split = ",")[[1]])){
      mhf[,ncol(mhf)+1]=""
      names(mhf)[ncol(mhf)] <- strsplit(names(mhf)[k],split = ",")[[1]][i]
    }
    mhf <- mhf[ -c(k) ]
  }

for(j in 6:ncol(mhf)){
  for(i in 1:nrow(mhf)){
    a <- grepl(names(mhf)[j], mhf[i,5])
    mhf[i,j] <- 0
    mhf[i,j][a] <- 1
  }
  mhf[,j] <- as.numeric(unlist(mhf[,j]))
}


mhf <- mhf[ , -which(names(mhf) %in% c("Holiday Name"))]
#mhf> subset(mhf, select=-c("flag_shrovetide.1","Holiday Name"))

setwd("../Process/Holiday/")

write.xlsx(mhf,"Magnit Holiday Flag 18,19,20.xlsx")