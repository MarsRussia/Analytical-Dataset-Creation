rm(list = ls())
library(readxl)
library(dplyr)
library(writexl)
library(lubridate)

#Setting Directory
cdir<-(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(cdir)

df <- read_excel("../Data/Combined Promo Sheet v6.xlsx",sheet="Final")


df$`PPG Name` <- ifelse(df$`PPG Name` == "OTC", "ORBIT OTC",df$`PPG Name`) 
df$`PPG Name` <- ifelse(df$`PPG Name` == "XXL", "ORBIT XXL",df$`PPG Name`) 



weeks <- data.frame(read_excel("../Data/MappingFile/Week Mapping.xlsx",sheet="Mapping"))
weeks <- weeks[weeks$Year >2017,]
# weeks$Week.Start.date <-  as.Date(as.character(as.POSIXct(weeks$Week.Start.date)))

##Functions
week_promo <- function(df){
  
  data<-weeks
  data$`Date start promo`<-as.Date("2022-01-01")
  data$`Date end promo`<-as.Date("2022-01-01")
  #data$`Discount NRV %` <- 0
  data$Promo_days<-0
  data$`PPG Name` <- "Initialize"
  data$Retailer <- "Intialize"
  
  for(i in 1:nrow(df)){
    # i=1
    d_frame<-df[i,]
    st<-(d_frame$`Date start promo`)
    end<-(d_frame$`Date end promo`)
    ind<-which((data$Week.Start.date>=st) & (data$Week.Start.date<=end ))
    ind1<-c(ind[1]-1,ind)
    j<-1
    for(d in ind1)
    {
      if(j==1){
        data[d,"Promo_days"]<- data[d,"Promo_days"]+7-(st-(data[d,1]))
        data[d,"Date start promo"]<-(d_frame$`Date start promo`)
        data[d,"Date end promo"]<-(d_frame$`Date end promo`)
        #data[d,8] <- d_frame$`Discount NRV %`
      }
      else if(j==length(ind1)){
        data[d,"Promo_days"]<- data[d,"Promo_days"]+end-(data[d,1])+1
        data[d,"Date start promo"]<-(d_frame$`Date start promo`)
        data[d,"Date end promo"]<-(d_frame$`Date end promo`)
        #data[d,8] <- d_frame$`Discount NRV %`
        
      }
      else{
        data[d,8]<-7
        data[d,"Date start promo"]<-(d_frame$`Date start promo`)
        data[d,"Date end promo"]<-(d_frame$`Date end promo`)
        #data[d,8] <- d_frame$`Discount NRV %`
        
      }
      j<-j+1
      data$`PPG Name` <- d_frame$`PPG Name`
      data$Retailer <- d_frame$Retailer
    }
  }
  # names(data)[names(data) == "sun"] <- "Date"
  fin <-data
  return(fin)
}

d_fin <- data.frame()
retailer <- unique(df$Retailer)
# i=retailer[1]
for(i in retailer){
  ab <- df[df$Retailer==i,]
  sku <- unique(ab$`PPG Name`)
  # j=sku[1]
  for(j in sku){
    df1 <- df[(df$Retailer == i) & (df$`PPG Name` ==j),]
    # df=df1
    fin <- week_promo(df1)
    d_fin <- rbind(d_fin,fin)
  }
}

d_fin$`Date start promo` <- ifelse(d_fin$`Date start promo` == as.Date("2022-01-01"),NA,as.character(d_fin$`Date start promo`))
d_fin$`Date end promo` <- ifelse(d_fin$`Date end promo` == as.Date("2022-01-01"),NA,as.character(d_fin$`Date end promo`))
# df$`Date start promo` <- as.character(df$`Date start promo`)

df <- data.frame(read_excel("../Data/Combined Promo Sheet v6.xlsx",sheet="Final"))

df$Date.start.promo <- as.character(ymd(df$Date.start.promo))
df$Date.end.promo <- as.character(ymd(df$Date.end.promo))

df <- df %>% rename("Date start promo" = "Date.start.promo",
                    "Date end promo" = "Date.end.promo","PPG Name"="PPG.Name")

df$`PPG Name` <- ifelse(df$`PPG Name` == "OTC", "ORBIT OTC",df$`PPG Name`) 
df$`PPG Name` <- ifelse(df$`PPG Name` == "XXL", "ORBIT XXL",df$`PPG Name`) 

df <- df[,-2]
d_fin <- d_fin %>% left_join(df,by=c("PPG Name","Retailer","Date start promo","Date end promo"))
d_fin$Week.Start.date <- as.POSIXct(d_fin$Week.Start.date, origin='1970-01-01')


gum <- d_fin[(d_fin$`PPG Name` == "ORBIT OTC") | (d_fin$`PPG Name` == "ORBIT XXL"),]
choco <- d_fin[!((d_fin$`PPG Name` == "ORBIT OTC") | (d_fin$`PPG Name` == "ORBIT XXL")),]


write_xlsx(gum,"../Process/Promo/Weekly_Promo_Gum_Data(PPG_Level).xlsx")
write_xlsx(choco,"../Process/Promo/Weekly_Promo_Choco_Data(PPG_Level).xlsx")
write_xlsx(d_fin,"../Process/Promo/Weekly_Promo_Days_Data(PPG_Level).xlsx")
