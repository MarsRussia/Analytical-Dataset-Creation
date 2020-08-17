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

Nielsen_data <- fread("Raw Data/(GUMS )Neilsen_Complete_data_long_format.csv")
#Creating Date (Merge Date week month mapping)
Week_Mapping <- read_excel("MappingFile/Week Mapping.xlsx",sheet = "Mapping")
Nielsen_data$Year = as.numeric(substr(Nielsen_data$Week,3,7))
Nielsen_data$Week_no = as.numeric(substr(Nielsen_data$Week,8,9))
Nielsen_data$Week_no = paste0("W",Nielsen_data$Week_no)

Nielsen_data$Nielsen_Sales <- Nielsen_data$`1:Value (in 1000 RUR)`*1000
Nielsen_data$Nielsen_Units <- Nielsen_data$`1:Units (in 1000 PACKS)`*1000
Nielsen_data$Price_per_Unit <- Nielsen_data$Nielsen_Sales/Nielsen_data$Nielsen_Units

#duplicated sku drop
Nielsen_data <-Nielsen_data[!duplicated(Nielsen_data[,c("Manufacturer","Brand","SKU","Week")])]


Nielsen_data <- merge(Nielsen_data,Week_Mapping,by.x=c("Year","Week_no"),by.y = c("Year","Week"),all.x = T)
Nielsen_data$Date <-as.Date(Nielsen_data$`Week Start date`)

Nielsen_data_raw <- copy(Nielsen_data)
Nielsen_data <- Nielsen_data[!is.na(Nielsen_data$SKU),]

#drop missing sku data
setnames(Nielsen_data,"Packsize","PC")
Nielsen_data <- Nielsen_data[!is.na(Nielsen_data$`1:Units (in 1000 PACKS)`)]







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



Nielsen_data1 <- Nielsen_data[,c("Manufacturer","Brand","SKU","PC","PackSize")]
Nielsen_data1 <-unique(Nielsen_data1)

#SKU-PPG mapping file
sku_ppg <-fread("MappingFIle/SKU_PPG_Mapping.csv")
Nielsen_data1 <-merge(Nielsen_data1,sku_ppg,by.x = "SKU",by.y = "Nielsen_SKU",all.x = T)
Nielsen_data1$PPG_Part<-NULL

########SKU-weight which are in range of PPG's Weight +- delta######
weight_delta=0.15
PPG_list=c("ORBIT OTC","ORBIT XXL")#unique(na.omit(Nielsen_data1$PPG))#

SKU_competitor <- data.frame()
for(i in PPG_list)
{
  print(i)
  other_ppg_data <- Nielsen_data1[!Nielsen_data1$PPG %in% i,]
  other_ppg_data$Weight_in_range=0
  colnames(other_ppg_data)=paste("Competitor",colnames(other_ppg_data),sep="_")
  
  ppg_data <- Nielsen_data1[Nielsen_data1$PPG==i,]
  ppg_weight_list <- unique(ppg_data$PackSize)
  
  tmp_competitor <-data.frame()
  for(j in ppg_weight_list)
  {
    #updates info
    idx=which(other_ppg_data$Competitor_Weight_in_range==0)
    other_ppg_data[idx,'Competitor_Weight_in_range']=as.numeric(abs((j-other_ppg_data[idx,'Competitor_PackSize'])/j)<=weight_delta)
    
  }
  
  other_ppg_data$PackSize <- list(ppg_weight_list)
  tmp_competitor=copy(other_ppg_data)
  
  tmp_competitor$PPG <- i
  SKU_competitor=rbind(SKU_competitor,tmp_competitor)
}

SKU_competitor <- SKU_competitor[!duplicated(SKU_competitor[,c("PPG","Competitor_SKU")])]#unique(SKU_competitor)
col_order <- c("PPG","PackSize",setdiff(colnames(SKU_competitor),c("PPG","PackSize")))
SKU_competitor <-SKU_competitor[,col_order,with=F]



#####Competitor Price data at SKU level#######
SKU_competitor_price <- merge(SKU_competitor,Nielsen_data_raw[,c("Date","Week","Year","Manufacturer","Brand","SKU","Nielsen_Sales","Nielsen_Units","Price_per_Unit")],
                              by.x=c("Competitor_Manufacturer","Competitor_Brand","Competitor_SKU"),
                              by.y=c("Manufacturer","Brand","SKU"),all.x=T)


#inf
SKU_competitor_price$Price_per_Unit[is.infinite(SKU_competitor_price$Price_per_Unit)]=NA




#own ppg data
Nielsen_SKU_data = merge(Nielsen_data_raw,sku_ppg,by.x = "SKU",by.y = "Nielsen_SKU",all.x = T)
#Nielsen_SKU_data =Nielsen_SKU_data[Nielsen_SKU_data$PPG %in% PPG_list,]
Nielsen_ppg_data = Nielsen_SKU_data %>%group_by(Year,Date,PPG) %>% summarise(Nielsen_Units=sum(Nielsen_Units,na.rm = T),
                                                                             Nielsen_Sales=sum(Nielsen_Sales,na.rm=T))

#SKU % share 2018-2019
Nielsen_SKU_data <- Nielsen_SKU_data[(Nielsen_SKU_data$Year>2017)& Nielsen_SKU_data$Year<=2019,]
Nielsen_SKU_data1 <- Nielsen_SKU_data %>% group_by(SKU,Year) %>% summarise(SKU_Units=sum(Nielsen_Units,na.rm = T),
                                                                           SKU_Sales=sum(Nielsen_Sales,na.rm=T))

Nielsen_SKU_data1['SKU_Price']=Nielsen_SKU_data1$SKU_Sales/Nielsen_SKU_data1$SKU_Units
Nielsen_SKU_data1$SKU_Price[is.infinite(Nielsen_SKU_data1$SKU_Price)]=NA

Nielsen_SKU_data1 <- Nielsen_SKU_data1 %>% group_by(Year) %>% mutate(SKU_Sum_sales = sum(SKU_Sales,na.rm = T),SKU_Sales_Share = SKU_Sales/SKU_Sum_sales)
Nielsen_SKU_Sales <- copy(Nielsen_SKU_data1)
setDT(Nielsen_SKU_data1)
Nielsen_SKU_data2=reshape(Nielsen_SKU_data1[,c('SKU','Year','SKU_Price','SKU_Sales_Share')], idvar = "SKU", timevar = "Year", direction = "wide")



#PPG Share
Nielsen_ppg_data =Nielsen_ppg_data[!is.na(Nielsen_ppg_data$PPG),]
Nielsen_ppg_data <- Nielsen_ppg_data[(Nielsen_ppg_data$Year>2017) & (Nielsen_ppg_data$Year<=2019),]

Nielsen_ppg_data['PPG_Price']=Nielsen_ppg_data$Nielsen_Sales/Nielsen_ppg_data$Nielsen_Units
Nielsen_ppg_data$PPG_Price[is.infinite(Nielsen_ppg_data$PPG_Price)]=NA

Nielsen_ppg_data1 <- Nielsen_ppg_data %>% group_by(PPG,Year) %>% summarise(PPG_Units=sum(Nielsen_Units,na.rm = T),
                                                                           PPG_Sales=sum(Nielsen_Sales,na.rm=T),
                                                                           PPG_min_price=min(PPG_Price,na.rm=T),
                                                                           PPG_max_price=max(PPG_Price,na.rm=T))


Nielsen_ppg_data1$PPG_min_price[is.infinite(Nielsen_ppg_data1$PPG_min_price)]=NA
Nielsen_ppg_data1$PPG_max_price[is.infinite(Nielsen_ppg_data1$PPG_max_price)]=NA


Nielsen_ppg_data1 <- Nielsen_ppg_data1 %>% group_by(Year) %>% mutate(PPG_Sum_sales = sum(PPG_Sales,na.rm = T),
                                                                     PPG_Sales_Share = PPG_Sales/PPG_Sum_sales)
Nielsen_PPG_Sales <- copy(Nielsen_ppg_data1)
setDT(Nielsen_ppg_data1)

Nielsen_ppg_data2=reshape(Nielsen_ppg_data1[,c('PPG','Year','PPG_min_price','PPG_max_price','PPG_Sales_Share')], idvar = "PPG", timevar = "Year", direction = "wide")



SKU_competitor_price <-SKU_competitor_price[(SKU_competitor_price$Year >2017)&(SKU_competitor_price$Year<=2019),]




############Correlation calculation############
SKU_competitor$Corr=NA
SKU_competitor$NA_prct=NA
for(i in 1:dim(SKU_competitor)[1])
{
  ppg=SKU_competitor$PPG[i]
  sku= SKU_competitor$Competitor_SKU[i]
  ppg_units=Nielsen_ppg_data[Nielsen_ppg_data$PPG==ppg,c("Date","Nielsen_Units")]
  sku_price=SKU_competitor_price[(SKU_competitor_price$PPG==ppg) &
                                   (SKU_competitor_price$Competitor_SKU==sku),c("Date","Price_per_Unit")]
  Unit_price <-merge(ppg_units,sku_price,by="Date",all.x=T)
  
  correlation=cor(Unit_price$Nielsen_Units,Unit_price$Price_per_Unit,use="pairwise.complete.obs")
  SKU_competitor$Corr[i]=correlation
  SKU_competitor$NA_prct[i]=sum(is.na(Unit_price$Price_per_Unit))/dim(Unit_price)[1]*100
  #if(sum(is.na(sku_price))/length(sku_price) >=)
  if(dim(ppg_units)[1]!=dim(sku_price)[1])
  {print(sku)
    print(ppg)}
}



#Merge PPG Price and sales Shares
SKU_competitor1=merge(SKU_competitor,Nielsen_ppg_data2,by='PPG',all.x = T)

#Merge SKU Price and sales Shares
SKU_competitor2=merge(SKU_competitor1,Nielsen_SKU_data2,by.x='Competitor_SKU',by.y="SKU",all.x = T)


#reorder
col_order <- c("PPG","PackSize",setdiff(colnames(SKU_competitor2),c("PPG","PackSize")))
SKU_competitor2 <-SKU_competitor2[,col_order,with=F]


#SKU price is in range of PPG price
#min 15% ppg < sku <= max 15% ppg
SKU_competitor2$'SKU_Price_in_range.2018'=ifelse(((SKU_competitor2$PPG_min_price.2018)*0.85 <= SKU_competitor2$SKU_Price.2018) & (SKU_competitor2$SKU_Price.2018
                                                                                                                                  < SKU_competitor2$PPG_max_price.2018),1,0)

SKU_competitor2$'SKU_Price_in_range.2019'=ifelse(((SKU_competitor2$PPG_min_price.2019)*0.85 <= SKU_competitor2$SKU_Price.2019) & (SKU_competitor2$SKU_Price.2019
                                                                                                                                  < SKU_competitor2$PPG_max_price.2019),1,0)

setorderv(SKU_competitor2,c('PPG','Competitor_Manufacturer',"Competitor_Brand","Competitor_SKU"))
a=colnames(SKU_competitor2)[colnames(SKU_competitor2) %like% '2018']
b=colnames(SKU_competitor2)[colnames(SKU_competitor2) %like% '2019']

col_req=c(setdiff(colnames(SKU_competitor2),c(a,b)),'SKU_Sales_Share.2018','SKU_Sales_Share.2019',
          'SKU_Price_in_range.2018','SKU_Price_in_range.2019')
SKU_competitor3=SKU_competitor2[,col_req,with=F]

#get price info of all SKUs
SKU_competitor_price <- merge(SKU_competitor3[,c('PPG','Competitor_SKU')],
                              Nielsen_data_raw[,c('SKU','Date','Nielsen_Sales','Nielsen_Units','Price_per_Unit')],by.x='Competitor_SKU',
                              by.y = 'SKU',all.x=T)

setorderv(SKU_competitor_price,c('PPG','Competitor_SKU','Date'))


list_of_datasets <- list("SKU_competitor_final" = SKU_competitor3, "PPG_data" =Nielsen_PPG_Sales,"SKU_data"=Nielsen_SKU_Sales,"SKU_competitor_data" = SKU_competitor2,
                         "SKU_comp_price_data"=SKU_competitor_price)
write.xlsx(list_of_datasets, file = "../Process/Competitor/Nielsen_Competitors_GUM_Data_all.xlsx")
#write.xlsx(SKU_competitor, file = "../Output/Competitor/Nielsen_Competitors_based_on_Corr_Choco_Data.xlsx")


