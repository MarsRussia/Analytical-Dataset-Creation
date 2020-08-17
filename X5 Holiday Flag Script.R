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

xhf<- read_excel("X5 Holiday Flag.xlsx")
xhf <- xhf[ -c(6) ]
xhf[,5][is.na(xhf[,5])] = -999

j<-6
for(i in 1:52){
  if(xhf[i,5]==-999){
    
  } else {
    xhf[,j]=""
    names(xhf)[j] <- xhf[i,5]
    j=j+1
  }
}

#buckets_df = subset(dfe, select = cols)
#cols = copy(colnames(buckets_df))
#dummy_df = fastDummies::dummy_cols(buckets_df)

p <- ncol(xhf)
for(k in 1:p)
  if (length(strsplit(names(xhf)[k],split = ",")[[1]]) >1){
    for ( i in 1:length(strsplit(names(xhf)[k],split = ",")[[1]])){
      xhf[,ncol(xhf)+1]=""
      names(xhf)[ncol(xhf)] <- strsplit(names(xhf)[k],split = ",")[[1]][i]
    }
    xhf <- xhf[ -c(k) ]
  }



#indx <- gsub(".*, ", "", names(xhf))

#for(i in 1:24){
#  names(xhf)[i] <- indx[i]
#}

for(j in 6:ncol(xhf)){
  for(i in 1:nrow(xhf)){
    a <- grepl(names(xhf)[j], xhf[i,5])
    xhf[i,j] <- 0
    xhf[i,j][a] <- 1
  }
  xhf[,j] <- as.numeric(unlist(xhf[,j]))
}

xhf <- xhf[ -c(5) ]

setwd("../Process/Holiday/")

write.xlsx(xhf,"X5 Holiday Flag 18,19,20.xlsx")
