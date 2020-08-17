rm(list = ls())
library(readxl)
library(dplyr)
library(writexl)
library(lubridate)

#Setting Directory

cdir<-(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(cdir)

df <- read_excel("../Process/HypercomAndDisplay/Extracted Data GUM for PPG from Hypercom_ver0.2.xlsx")
a <- df[df$`% Directory distribution` =="??????.",]  #PET
a <- a[!is.na(a$`Promo Groups`),]
a <- a[!(is.na(a$`Discount depth`)),]
a$`Distribution Promo%` <- as.double(a$`Distribution Promo%`)
a$`% Directory distribution` <- as.double(a$`% Directory distribution`)
a <- a %>% group_by(`Promo Groups`,Seller,`start date`,`expiration date`) %>% summarise(`sum_% Directory distribution`=unique(`% Directory distribution`),
                                                                             `Avg_% Directory distribution`=unique(`% Directory distribution`),
                                                                             `max_% Directory distribution`=unique(`% Directory distribution`),
                                                                             Count= n(),
                                                                             `sum_Distribution Promo%` = unique(`Distribution Promo%`),
                                                                             `Avg_Distribution Promo%` = unique(`Distribution Promo%`),
                                                                             `Max_Distribution Promo%` = unique(`Distribution Promo%`),
                                                                             `Avg_Price without stock for goods`=mean(`Price without stock for goods`),
                                                                             `Avg_Promo Price` = mean(`Promo Price`),
                                                                             `Avg_Price per unit` = mean(`Price per unit`),
                                                                             `Avg_Virtual price` = mean(`Virtual price`),
                                                                             `Avg_Discount depth` = mean(`Discount depth`)
                                                                              )

b <-  df[!(df$`% Directory distribution` =="§²§¦§¤."),]
b <- b[!(is.na(b$`Discount depth`)),]

b$`Distribution Promo%` <- as.double(b$`Distribution Promo%`)
b$`% Directory distribution` <- as.double(b$`% Directory distribution`)
b <- b %>% group_by(`Promo Groups`,Seller,`start date`,`expiration date`) %>% summarise(`sum_% Directory distribution`=sum(`% Directory distribution`),
                                                                             `Avg_% Directory distribution`=mean(`% Directory distribution`),
                                                                             `max_% Directory distribution`=max(`% Directory distribution`),
                                                                             Count= n(),
                                                                             `sum_Distribution Promo%` = sum(`Distribution Promo%`),
                                                                             `Avg_Distribution Promo%` = mean(`Distribution Promo%`),
                                                                             `Max_Distribution Promo%` = max(`Distribution Promo%`),
                                                                             `Avg_Price without stock for goods`=mean(`Price without stock for goods`),
                                                                             `Avg_Promo Price` = mean(`Promo Price`),
                                                                             `Avg_Price per unit` = mean(`Price per unit`),
                                                                             `Avg_Virtual price` = mean(`Virtual price`),
                                                                             `Avg_Discount depth` = mean(`Discount depth`)
                                                                                )
dat <- rbind(a,b)
write_xlsx(dat,"../Process/HypercomAndDisplay/Rolledup_Gum_v1.xlsx")#Change NA to PET. in excel manually also check if any overlaps present ,if so remove that.


############Weakly Data#####

df <- (read_excel("../Process/HypercomAndDisplay/Rolledup_Gum_v1.xlsx"))
df$`start date` <- as.Date(df$`start date`)
df$Seller <- ifelse(df$Seller == "§®§Ñ§Ô§ß§Ú§ä - §¥§Ú§ã§Ü§Ñ§å§ß§ä§Ö§â", "Magnit","X5") #Magnit Discounters

weeks <- data.frame(read_excel("../Data/MappingFile/Week Mapping.xlsx",sheet="Mapping"))
weeks <- weeks[weeks$Year >=2017,]
weeks$Week.Start.date <-  as.Date(as.character(as.POSIXct(weeks$Week.Start.date)))
# a <- weeks[1,1]-st
##Functions
week_promo <- function(df){
  
  data<-weeks
  data$`start date` <-as.Date("2022-01-01")
  data$`expiration date`<-as.Date("2022-01-01")
  data$promo_days<-0
  data$`Promo Groups` <- "Initialize"
  data$Seller <- "Intialize"
  
  for(i in 1:nrow(df)){
    # i=1
    d_frame<-df[i,]
    st<-as.Date(d_frame$`start date`)
    end<-as.Date(d_frame$`expiration date`)
    ind<-which((data$Week.Start.date>=st) & (data$Week.Start.date<=end ))
    ind1<-c(ind[1]-1,ind)
    j<-1
    for(d in ind1)
    {
      if(j==1){
        data[d,8]<-data[d,8]+7-(st-(data[d,1]))
        data[d,6]<-as.Date(d_frame$`start date`)
        data[d,7]<-as.Date(d_frame$`expiration date`)
        data[d,10] <- d_frame$Seller
        data[d,9] <- d_frame$`Promo Groups`
      }
      else if(j==length(ind1)){
        data[d,8]<-data[d,8]+end-(data[d,1])+1
        data[d,6]<-as.Date(d_frame$`start date`)
        data[d,7]<-as.Date(d_frame$`expiration date`)
        data[d,10] <- d_frame$Seller
        data[d,9] <- d_frame$`Promo Groups`
      }
      else{
        data[d,8]<-7
        data[d,6]<-as.Date(d_frame$`start date`)
        data[d,7]<-as.Date(d_frame$`expiration date`)
        data[d,10] <- d_frame$Seller
        data[d,9] <- d_frame$`Promo Groups`
      }
      j<-j+1
      data$`Promo Groups` <- d_frame$`Promo Groups`
      data$Seller <- d_frame$Seller
    }
  }
   names(data)[names(data) == "Week.Start.date"] <- "Date"
  fin <-data
  return(fin)
}

d_fin <- data.frame()
retailer <- unique(df$Seller)
# i=retailer[1]
for(i in retailer){
  ab <- df[df$Seller==i,]
  sku <- unique(ab$`Promo Groups`)
  # j=sku[1]
  for(j in sku){
    df1 <- df[(df$Seller == i) & (df$`Promo Groups` ==j),]
    fin <- week_promo(df1)
    d_fin <- rbind(d_fin,fin)
  }
}

d_fin$`start date`<- ifelse(d_fin$`start date` == as.Date("2022-01-01"),NA,as.character(d_fin$`start date`))
d_fin$`expiration date
` <- ifelse(d_fin$`expiration date` == as.Date("2022-01-01"),NA,as.character(d_fin$`expiration date`))


df <- data.frame(read_excel("../Process/HypercomAndDisplay/Rolledup_Gum_v1.xlsx"))
df$start.date <- as.character(ymd(df$start.date))
df$expiration.date <- as.character(ymd(df$expiration.date))
df$Seller <- ifelse(df$Seller == "§®§Ñ§Ô§ß§Ú§ä - §¥§Ú§ã§Ü§Ñ§å§ß§ä§Ö§â", "Magnit","X5") #Magnit Discounters

df <- df %>% rename("start date" = "start.date",
                    "expiration date" = "expiration.date","Promo Groups"="Promo.Groups")


final <- d_fin %>% left_join(df,by=c("start date","expiration date","Promo Groups","Seller"))
write_xlsx(final,"../Process/HypercomAndDisplay/Weekly_Promo_hypercom_Gum_Data(PPG_Level).xlsx")

