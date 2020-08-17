rm(list = ls())
library(readxl)
library(writexl)
library(dplyr)



#Setting Directory
cdir<-(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(cdir)
# 
# #Data Reading
# df1 <- read_excel("SKU_PPG_Mapping.xlsx")
# df1 <- df1[df1$YEAR ==2019,]
# display <- read_excel("Final_Display_2019_Data.xlsx")
# 
# df1 <- display %>% left_join(df1,by=c("YEAR","MWC Chain","Plan level SRM/SKU"))
# 
# df2 <- df1[!is.na(df1$Promo_Group),]
# write_xlsx(df2,"Display_Output.xlsx")

df <- read_excel("../Data/Display_Output.xlsx",sheet="Sheet3")

weeks <- data.frame(read_excel("../Data/MappingFile/Week Mapping.xlsx",sheet="Mapping"))
weeks <- weeks[weeks$Year ==2019,]
weeks$Week.Start.date <-  as.Date(as.character(as.POSIXct(weeks$Week.Start.date)))
# a <- weeks[1,1]-st
##Functions
week_promo <- function(df){
  
  data<-weeks
  data$`Date start promo`<-as.Date("2022-01-01")
  data$`Date end promo`<-as.Date("2022-01-01")
  data$BAHS <- 0
  data$AHS <-0
  data$miniBAHS <-0
  data$miniAHS <-0
  data$Promo_Group <- "Initialize"
  data$`MWC Chain` <- "Intialize"
  
  for(i in 1:nrow(df)){
    # i=1
    d_frame<-df[i,]
    st<-as.Date(d_frame$`Date start promo`)
    end<-as.Date(d_frame$`Date end promo`)
    ind<-which((data$Week.Start.date>=st) & (data$Week.Start.date<=end ))
    ind1<-c(ind[1]-1,ind)
    j<-1
    for(d in ind1)
    {
  
        data[d,6]<-as.Date(d_frame$`Date start promo`)
        data[d,7]<-as.Date(d_frame$`Date end promo`)
        data[d,8] <- d_frame$BAHS
        data[d,9] <- d_frame$AHS
        data[d,10] <- d_frame$miniBAHS
        data[d,11] <- d_frame$miniAHS
    
      data$Promo_Group <- d_frame$Promo_Group
      data$`MWC Chain` <- d_frame$`MWC Chain`
    }
  }
  # names(data)[names(data) == "sun"] <- "Date"
  fin <-data
  return(fin)
}

d_fin <- data.frame()
retailer <- unique(df$`MWC Chain`)
# i=retailer[1]
for(i in retailer){
  ab <- df[df$`MWC Chain`==i,]
  sku <- unique(ab$Promo_Group)
  # j=sku[1]
  for(j in sku){
    df1 <- df[(df$`MWC Chain` == i) & (df$Promo_Group ==j),]
    fin <- week_promo(df1)
    d_fin <- rbind(d_fin,fin)
  }
}

d_fin$`Date start promo` <- ifelse(d_fin$`Date start promo` == as.Date("2022-01-01"),NA,as.character(d_fin$`Date start promo`))
d_fin$`Date end promo` <- ifelse(d_fin$`Date end promo` == as.Date("2022-01-01"),NA,as.character(d_fin$`Date end promo`))

write_xlsx(d_fin,"../Process/HypercomAndDisplay/Disply_Output_v1.xlsx")
