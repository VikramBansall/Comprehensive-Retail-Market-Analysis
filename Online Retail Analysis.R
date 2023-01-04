library(arules)
library(arulesViz)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(knitr)
library(readxl)
library(plyr)
library(RColorBrewer)
#
#data<-read.csv("Retail.csv")
data<-read_excel("C:/Users/Vikra/OneDrive/Documents/Retaill.xlsx")
data
View(data)
head(data)
#
data<-data[complete.cases(data),]
data %>% mutate(Description = as.factor(Description))
data%>% mutate(Country = as.factor(Country))
data$Date <- as.Date(data$InvoiceDate)
TransTime<- format(data$InvoiceDate,"%H:%M:%S")
InvoiceNo <- as.numeric(as.character(data$InvoiceNo))
cbind(data,InvoiceNo)
cbind(data,TransTime)
View(data)
glimpse(data)
#
transactiondata<-ddply(data,c("InvoiceNo","Date"),
                       function(df1)paste(df1$Description,collapse = ","))
transactiondata
transactiondata$InvoiceNo<-NULL
transactiondata$Date<-NULL
colnames(transactiondata)<-c("items")
#
write.csv(transactiondata,"C:/Users/Vikra/OneDrive/Documents/market_basket_transactions.csv", quote = FALSE, row.names = FALSE)
#data1<-read.csv("market_basket_transactions.csv")
#View(data1)
tr <- read.transactions('C:/Users/Vikra/OneDrive/Documents/market_basket_transactions.csv', format = 'basket', sep=',')
tr
summary(tr)

itemFrequencyPlot(tr,topN=30,type="absolute",col=brewer.pal(8,'Pastel2'),main="Absolute Item Frequency Plot")
#
associationrules<-apriori(tr,parameter = list(supp=0.01,conf=0.5,maxlen=5))
inspect(associationrules[1:10])


#
subrules<-associationrules[quality(associationrules)$confidence>0.4]
plot(subrules)
plot(subrules,method="two-key plot")
plot(subrules,method="paracoord")
plot(subrules,method = "grouped")