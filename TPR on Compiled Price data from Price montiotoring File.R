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
library(data.table)
library(stringr)
library(tidyr)
setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path)))

compiled_price_data = read_excel("../Data/Compiled Price Data.xlsx", sheet = "Final")

finl_df = data.table()
for( i in unique(compiled_price_data$Nielsen_SKU)){
  tm_df = compiled_price_data[compiled_price_data$Nielsen_SKU==i,]
  for(j in unique(tm_df$Retailer)){
    tdf = tm_df[tm_df$Retailer==j,]
    tdf=tdf[,1:5]
    tdf = tdf[order(tdf$Week),] 
    ####Filling Na and 0 with previous weeks between 2018week25 to 2020week20
    tdf$`Price (Price Monitoring Sheet)`[tdf$`Price (Price Monitoring Sheet)`==0]=NA
    tdf$tmp = as.numeric(gsub("_W", "", tdf$Week))
    tde = tdf[tdf$tmp>=201825 & tdf$tmp<=202020,]
    tdr = tde%>%tidyr::fill(`Price (Price Monitoring Sheet)`)
    td1 = tdf[tdf$tmp<201825,]
    td2 = copy(tdr)
    td3 = tdf[tdf$tmp>202020,]
    tm = rbind(td1,td2)
    tm = rbind(tm,td3)
    tdf =copy(tm)
    tdf$tmp<-NULL
    tdf = tdf[order(tdf$Week),]
    colnames(tdf) = c("Retailer","PPG","Nielsen_SKU","Week","aup")
    choco_mbp=copy(tdf)
    choco_mbp <- data.table(choco_mbp)
    choco_mbp[,paste("lag_price_",1:7,sep = "") := shift(aup,1:7),
              by = list(PPG)]
    
    choco_mbp[,max_price_prev := max(lag_price_1,
                                     lag_price_2,
                                     lag_price_3,
                                     lag_price_4,
                                     lag_price_5,
                                     lag_price_6,
                                     lag_price_7,na.rm = TRUE),by = list(Retailer,PPG,Nielsen_SKU,Week)]
    choco_mbp[,lag_price_1 := ifelse(abs((lag_price_1 - max_price_prev)/max_price_prev) <= 0.05,lag_price_1,NA)]
    choco_mbp[,lag_price_2 := ifelse(abs((lag_price_2 - max_price_prev)/max_price_prev) <= 0.05,lag_price_2,NA)]
    choco_mbp[,lag_price_3 := ifelse(abs((lag_price_3 - max_price_prev)/max_price_prev) <= 0.05,lag_price_3,NA)]
    choco_mbp[,lag_price_4 := ifelse(abs((lag_price_4 - max_price_prev)/max_price_prev) <= 0.05,lag_price_4,NA)]
    choco_mbp[,lag_price_5 := ifelse(abs((lag_price_5 - max_price_prev)/max_price_prev) <= 0.05,lag_price_5,NA)]
    choco_mbp[,lag_price_6 := ifelse(abs((lag_price_6 - max_price_prev)/max_price_prev) <= 0.05,lag_price_6,NA)]
    choco_mbp[,lag_price_7 := ifelse(abs((lag_price_7 - max_price_prev)/max_price_prev) <= 0.05,lag_price_7,NA)]
    choco_mbp[,median_baseprice := median(c(lag_price_1,
                                            lag_price_2,
                                            lag_price_3,
                                            lag_price_4,
                                            lag_price_5,
                                            lag_price_6,
                                            lag_price_7),na.rm = TRUE),by = list(Retailer,PPG,Nielsen_SKU,Week)]
    
    choco_mbp[,median_baseprice := ifelse((median_baseprice -
                                             aup)/median_baseprice <= 0.05,
                                          aup,median_baseprice)]
    
    
    choco_mbp[,TPR := ((median_baseprice - aup)/median_baseprice)]
    finl_df = rbind(finl_df,choco_mbp)
  }
}
setnames(finl_df,"aup","Price(Price Montioring File)")

write.xlsx(finl_df,"../Process/Price_Monitoring/TPR calculation on PRice from Price Monitoring file.xlsx")
