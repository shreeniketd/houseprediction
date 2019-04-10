####################################################################################
#House Price Prediction in Kind County, USA
#Developed by: Shakya Munghate and Shreeniket Deshmukh
####################################################################################
#Convert Scientific Values into Numeric
options(scipen = 999)

#Setting Directory
setwd("C:/Personal/Intermediate Analytics/Final Project/House Price Prediction")

#Calling all the libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
library("summarytools")
library(plotly)
library(leaflet)
library(caTools)
library(randomForest)
library(corrplot)

hp <- read.csv("kc_house_data.csv")
head(hp)
summary(hp)
dim(hp)
str(hp)
View(hp)

#Converting date iinto yyyy-mm-dd format
hp$date <-gsub("\\T000000","",hp$date)
hp$date<-as.Date(hp$date,"%Y%m%d")
head(hp)

#Checking for NA
hp[!complete.cases(hp),] # The data does not have missing value

#Dimension
dim(hp)

#Creating bins of size
hp$size_bins <- cut(hp$sqft_living,c(0,1000,1500,2000,2500,15000),
                    labels = c("Small","Medium Small","Medium","Medium Large","Large"))
head(hp)

#Subseting per bedrooms
two_bed <- subset(hp,bedrooms==2)
three_bed <- subset(hp,bedrooms==3)
four_bed <- subset(hp,bedrooms==4)
five_bed <- subset(hp,bedrooms==5)

#######################################################################################################################
#Descriptive Statistics
#######################################################################################################################
summary(hp)
hp_summary<-summarytools::descr(hp)
hp_summary
write.csv(hp_summary,file="Smmary.csv")
#2 bedrooms
hp2_summary<-summarytools::descr(two_bed)
hp2_summary

#3 bedrooms
hp3_summary<-summarytools::descr(three_bed)
hp3_summary

#4 bedrooms
hp4_summary<-summarytools::descr(four_bed)
hp4_summary

#5 bedrooms
hp5_summary<-summarytools::descr(five_bed)
hp5_summary

########################################################################################
# Exploratory Data Analysis
########################################################################################
par(mfrow=c(2,1))
# Year Built vs Count
YrCnt <- hp %>% group_by(yr_built) %>% summarise(House_Count=n())
YrCnt <- as.data.frame(YrCnt)

plot(x=YrCnt$yr_built,y=YrCnt$House_Count,type = "S", col="blue", 
     xlab="Year Built", ylab="No. of Houses Built",
     main="Count of houses Vs Year Built")
abline(h=mean(YrCnt$House_Count))

#Count of houses per bedrooms

t<-table(hp$bedrooms)
barplot(t, main = "Count of houses per No. of Bedrooms", 
        xlab="No. of Bedrooms", ylab="Count",col="blue",ylim = c(0,12000))


par(mfrow=c(1,1))

#Removing outliers
hp$outlier_flag<-ifelse(hp$bedrooms<12,0,1)
hp<-subset(hp,outlier_flag==0)

#Pricing Vs Bedrooms
hp_mean<-hp %>% group_by(bedrooms) %>% summarise(h_mean=mean(price))
hp_mean<-as.data.frame(hp_mean)
ggplot(hp_mean,aes(x=bedrooms,y=h_mean)) +geom_bar(stat="identity",fill="blue") +
  labs(x = 'Bedrooms',  y = 'Mean Price', title = 'Bedrooms and Mean Price')  + 
  theme(plot.title = element_text(hjust = 0.5))

#Checking the statistical relationship of Pricing and Bedrooms
hp$log_price<-log10(hp$price)
head(hp)
gg_p<-ggplot(hp,aes(x=bedrooms,y=log_price,col=bedrooms)) + geom_point(position="identity",alpha=0.5,size=hp$log_price)
gg_p+geom_smooth(method = "lm",se=F) + labs(x="Bedrooms",y="Log(Price)",title="Bedroom Vs Price") + theme(plot.title = element_text(hjust = 0.5))


ggscatter(hp,x="bedrooms",y="log_price", add = "reg.line", add.params = list(color="black"),
          conf.int=T, 
          cor.coef = T, 
          cor.method = "pearson", 
          xlab="Bedrooms", ylab="Log(Price)",
          title="Bedrooms Vs Price", col="blue") +theme(plot.title = element_text(hjust=0.5))

#Correlation Test Bedrooms vs Price
corr_BvP <- cor.test(hp$price,hp$bedrooms,method="pearson")
corr_BvP

#Pearson's product-moment correlation
#data:  hp$price and hp$bedrooms
#t = 48.866, df = 21610, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.3033885 0.3274002
#sample estimates:
#  cor 
#0.3154449 

#Price Trend for 2,3,4 bedroom apartment as per floor no.
par(mfrow=c(2,2))

# 2 bedrooms
two_p<- two_bed %>% group_by(floors) %>% summarise(two_mean=mean(price))
two_p<-as.data.frame(two_p)
plot(two_p$floors,two_p$two_mean,type="l", main="Floor and 2 Bedroom Pricing",xlab = "Floor",ylab = "Mean Price")

#3 bedrooms
three_p <- three_bed %>% group_by(floors) %>% summarise(three_mean=mean(price))
three_p <- as.data.frame(three_p)
plot(three_p$floors,three_p$three_mean,type="l", main="Floor and 3 Bedroom Pricing",xlab = "Floor",ylab = "Mean Price")

#4 bedrooms
four_p <- four_bed %>% group_by(floors) %>% summarise(four_mean=mean(price))
four_p <- as.data.frame(four_p)
plot(four_p$floors,four_p$four_mean,type="l", main="Floor and 4 Bedroom Pricing",xlab = "Floor",ylab = "Mean Price")

#5 bedrooms
five_p <- five_bed %>% group_by(floors) %>% summarise(four_mean=mean(price))
five_p <- as.data.frame(five_p)
plot(five_p$floors,five_p$five_mean,type="l", main="Floor and 5 Bedroom Pricing",xlab = "Floor",ylab = "Mean Price")

par(mfrow=c(1,2))

#Plotting histogram to check the distribution
hist(hp$log_price,col="blue" ,main="Histogram: Price",xlab = "Log(Price)")
plot_ly(alpha=0.5)%>% add_histogram(x=~hp$price)%>%
  layout(xaxis=list(title="Price"),yaxis=list(title="Frequency"),title="Histogram: Price")
plot_ly(alpha=0.5)%>% add_histogram(x=~hp$log_price)%>%
  layout(xaxis=list(title="Log(Price)"),yaxis=list(title="Frequency"),title="Histogram: Log(Price)")

#Checking the Normality of the distribution
qqnorm(hp$price)
qqline(hp$price)

par(mfrow=c(1,1))

#Area vs Price

gg_s<-ggplot(hp,aes(x=size_bins,y=log_price,col=bedrooms)) + geom_point(position="identity",alpha=0.5,size=hp$log_price)
gg_s+geom_smooth(method = "lm",se=F) + labs(x="Area",y="Log(Price)",title="Area Vs Price") + 
  theme(plot.title = element_text(hjust = 0.5))

ggscatter(hp,x="sqft_living",y="log_price", add = "reg.line", add.params = list(color="black"),
          conf.int=T, 
          cor.coef = T, 
          cor.method = "pearson", 
          xlab="Area", ylab="Log(Price)",
          title="Area Vs Price", col="blue") +theme(plot.title = element_text(hjust=0.5))

#Correlation Test Area vs Price

corr_AvP <- cor.test(hp$price,hp$sqft_living,method="pearson")
corr_AvP

#Pearson's product-moment correlation

#data:  hp$price and hp$sqft_living
#t = 144.92, df = 21610, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.6952215 0.7087452
#sample estimates:
#     cor 
#0.7020466
#We can conclude that Area and Price are significantly correlated because 
#p-value is less than 5% with correlation coeff=0.7


#Pricing and waterfront
hp$waterview<-ifelse(hp$waterfront==1,"Yes","No")

gg_w<-ggplot(hp,aes(x=waterview,y=log_price,fill=waterview)) + geom_boxplot() + labs(x="Water Facing",y="Log(Price)",title="Water Facing Vs Price") + theme(plot.title = element_text(hjust = 0.5))
gg_w

# Year Built Vs Price
# 2 Bedrooms
YrPr2 <- subset(hp,bedrooms==2) %>% group_by(yr_built) %>% summarise(House_Price2=mean(price))
YrPr2 <- as.data.frame(YrPr2)
head(YrPr2)

# 3 Bedrooms
YrPr3 <- subset(hp,bedrooms==3) %>% group_by(yr_built) %>% summarise(House_Price3=mean(price))
YrPr3 <- as.data.frame(YrPr3)
head(YrPr3)

# 4 Bedrooms
YrPr4 <- subset(hp,bedrooms==4) %>% group_by(yr_built) %>% summarise(House_Price4=mean(price))
YrPr4 <- as.data.frame(YrPr4)
head(YrPr4)

# 5 Bedrooms
YrPr5 <- subset(hp,bedrooms==5) %>% group_by(yr_built) %>% summarise(House_Price5=mean(price))
YrPr5 <- as.data.frame(YrPr5)
head(YrPr5)

par(mfrow=c(2,2))

plot(x=YrPr2$yr_built,y=YrPr2$House_Price2 ,type = "p", col="blue", 
     xlab="Year Built", ylab="Mean Price",
     main="Average Price of 2 Bedroom Houses Vs Year Built")
abline(h=mean(YrPr2$House_Price2),col="red")
  
plot(x=YrPr3$yr_built,y=YrPr3$House_Price3 ,type = "p", col="red", 
     xlab="Year Built", ylab="Mean Price",
     main="Average Price of 3 Bedroom Houses Vs Year Built")
abline(h=mean(YrPr3$House_Price3,col="blue"))  

plot(x=YrPr4$yr_built,y=YrPr4$House_Price4 ,type = "p", col="brown", 
     xlab="Year Built", ylab="Mean Price",
     main="Average Price of 4 Bedroom Houses Vs Year Built")
abline(h=mean(YrPr4$House_Price4),col="black")

plot(x=YrPr5$yr_built,y=YrPr5$House_Price5 ,type = "p", col="black", 
     xlab="Year Built", ylab="Mean Price",
     main="Average Price of 5 Bedroom Houses Vs Year Built")
abline(h=mean(YrPr5$House_Price5),col="brown")

par(mfrow=c(1,1))

ggscatter(hp,x="yr_built",y="price", add = "reg.line", add.params = list(color="black"),
          conf.int=T, 
          cor.coef = T, 
          cor.method = "pearson", 
          xlab="Year Built", ylab="price",
          title="Year Built Vs Price", col="blue") +theme(plot.title = element_text(hjust=0.5))

corr_YvP <- cor.test(hp$price,hp$yr_built,method="pearson")
corr_YvP

###################################################################
#Mapping this data on Maps
###################################################################
#Price
hp$price_bins <- cut(hp$price,c(0,300000,500000,700000,1200000,1500000,8000000),
                     labels = c("Cheap","Mid-Range","Mid-High","High-Range","Expensive","Platinum"))


med_long = median(hp$long,na.rm = TRUE)
med_lat = median(hp$lat,na.rm = TRUE)


col_price <- colorFactor(c("yellow","orange","green","blue","red","white"),hp$price_bins)

leaflet(hp) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~long, lat = ~lat, 
             col = ~col_price(price_bins))  %>%
   setView(lng=med_long, lat=med_lat,zoom = 11) %>%
    addLegend("topright", pal = col_price, values = ~price_bins,
            title = "Mapping House Price",
            opacity = 1.5)

#Size
col_size <- colorFactor(c("yellow","orange","green","blue","red","white"),hp$size_bins)

leaflet(hp) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~long, lat = ~lat, 
             col = ~col_size(size_bins))  %>%
  setView(lng=med_long, lat=med_lat,zoom = 11) %>%
  addLegend("topright", pal = col_size, values = ~size_bins,
            title = "Mapping House Size",
            opacity = 1)

##########################################################################################
#Corr Plot
##########################################################################################
hp_c<-hp[,3:21]
hp_corr<-cor(hp_c)
corrplot(hp_corr,method="circle",order = "hclust", addrect = 2)

##########################################################################################
#Regression Model
#for this we should first split the data into training and testing
##########################################################################################

set.seed(1)
split_hp <- sample.split(hp,SplitRatio = 0.70)
hp_train <- subset(hp[,c(1:21)],split_hp ==TRUE)
hp_test <- subset(hp[,c(1:21)],split_hp == FALSE)
str(hp_train)
#Linear Model
#Training the model

lm_hp <- lm(price~.,data = hp_train)
summary(lm_hp)
#plot(lm_hp$model)

#Predict
pred_hp <- predict(lm_hp,newdata = hp_test)

#RMSE
RMSE = function(m, o){ 
  sqrt(mean((m - o)^2))
}

RMSE_lm <-RMSE(pred_hp,hp_test$price)
RMSE_lm
#cbind(pred_hp,hp_test$price)

plot(hp_test$price,pred_hp,xlab = "Actual",ylab = "Predicted",main="Actuals vs Predicted")
abline(0,1)

#Below Columns show a strong statistical relationsip with the price
#################
"bedrooms
bathrooms
sqft_living
sqft_lot
floors
waterfrontYes
condition
sqft_above
yr_built 
zipcode
lat
long
sqft_living15
sqft_lot15"
#################

#Random Forest

rf_hp <- randomForest(price~.,data=hp_train, importance=T)
summary(rf_hp)

predrf_hp <- predict(rf_hp,newdata = hp_test)

#Importance
varImpPlot(rf_hp, main="Importance")

#RMSE
RMSE = function(m, o){ 
  sqrt(mean((m - o)^2))
}

RMSE_rf <-RMSE(predrf_hp,hp_test$price)
RMSE_rf
RMSE_lm

plot(hp_test$price,predrf_hp,xlab = "Actual",ylab = "Predicted")

abline(0,1)

############
#Lasso
############
library(caret)
library(glmnet)
trainX<-hp_train[,1:21]
lasso1<-train(price~.,trainX,method="glmnet",
             tuneGrid=expand.grid(alpha=1,lambda=seq(1,1000,length=100)))
plot(lasso1)
summary(lasso1)
plot(lasso1$finalModel,xvar = 'lambda',label=T)
plot(lasso1$finalModel,xvar = 'dev',label=T) #60% of variability is explained by 6 variables
plot(varImp(lasso1,scale=F),main="Variable Importance")

lasso_pred_hp <- predict(lasso1,newdata = hp_test)

#RMSE
RMSE = function(m, o){ 
  sqrt(mean((m - o)^2))
}

RMSE_lasso <-RMSE(lasso_pred_hp,hp_test$price)
RMSE_lasso 
RMSE_lm
RMSE_rf

plot(hp_test$price,lasso_pred_hp,xlab = "Actual",ylab = "Predicted")

abline(0,1)

