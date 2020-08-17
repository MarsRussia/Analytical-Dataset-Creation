rm(list = ls())
library(rstudioapi)
library(data.table)
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

#Manux brand x packsize x sku
Nielsen_data <- fread("Raw Data/(GUMS )Neilsen_Complete_data_long_format.csv")

#duplicated sku drop
Nielsen_data <-Nielsen_data[!duplicated(Nielsen_data[,c("Manufacturer","Brand","SKU","Week")])]
setnames(Nielsen_data,"Packsize","PC")

Nielsen_data <-Nielsen_data[Nielsen_data$SKU %like% "KG",]
print(dim(Nielsen_data[Nielsen_data$SKU %like% "KG",]))

#extracting packsie/weight
# in two form, 1: weight, 2: numberXweight
Nielsen_data$tmp <- gsub("KG",'#',Nielsen_data$SKU)
Nielsen_data$tmp <- gsub("X0",' 0',Nielsen_data$tmp)
Nielsen_data$tmp= (lapply(Nielsen_data$tmp, function(i) unlist(strsplit(i, split = '[#]'))[1]))
Nielsen_data$tmp1= (lapply(Nielsen_data$tmp, function(i) unlist(strsplit(i, split = ' '))))
Nielsen_data$PackSize=(lapply(Nielsen_data$tmp1,function(i) i[length(i)]))
Nielsen_data$PackSize =as.numeric(Nielsen_data$PackSize)*1000 #in Gram
print(unique(Nielsen_data$PackSize))
print(sum(is.na(Nielsen_data$PackSize)))

#colnames change
col_req=c("Manufacturer","Brand","PackSize","SKU","Week")
col_1000=c(colnames(Nielsen_data)[colnames(Nielsen_data) %like% "1000"])
col_dist=c("1:Numeric Distribution (w)","1:Weighted Distribution (w)",
           "2:Numeric Distribution (w)","2:Weighted Distribution (w)",
           "3:Numeric Distribution (w)","3:Weighted Distribution (w)")
col_req=c(col_req,col_1000,col_dist)

Nielsen_data[,col_1000]=Nielsen_data[,col_1000,with=F]*1000

Nielsen_data=Nielsen_data[,col_req,with=F]
print(colnames(Nielsen_data))
colnames(Nielsen_data) <- gsub("in 1000", "", colnames(Nielsen_data))


colnames(Nielsen_data) <- gsub("1:", "Total_", colnames(Nielsen_data))
colnames(Nielsen_data) <- gsub("2:", "Non_Promo_", colnames(Nielsen_data))
colnames(Nielsen_data) <- gsub("3:", "Promo_", colnames(Nielsen_data))


setnames(Nielsen_data,c("Total_Volume ()","Total_Value ( RUR)","Total_Units ( PACKS)","Non_Promo_Volume ()"),
         c("Total_Volume","Total_Value","Total_Units","Non_Promo_Volume"))
setnames(Nielsen_data,c("Non_Promo_Value ( RUR)","Non_Promo_Units ( PACKS)","Promo_Volume ()","Promo_Value ( RUR)",
                        "Promo_Units ( PACKS)"),c("Non_Promo_Value","Non_Promo_Units","Promo_Volume","Promo_Value",
                                                  "Promo_Units"))

write.xlsx(Nielsen_data, file = "../Output/Temp/Nielsen_SKU_Data_GUM.xlsx")
