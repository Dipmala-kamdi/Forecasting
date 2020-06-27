library(readr)
setwd("D:\\excelr_DS\\R code\\Forecasting")

library(readxl)

#c_cola <- read.table(file.choose())
c_cola <- read_excel(file.choose())
View(c_cola) # Seasonality 12 months 
windows()
plot(c_cola$Sales,type="o")


# Creating dummies for Quarters 
####

#yq <- as.yearqtr(c_cola$Quarter, format = "Q%q_%y")
#View(yq)

xq<- c("Q1","Q2","Q3","Q4")
View(xq)
X<- data.frame(outer(rep(xq,len=42 ), xq,"==") + 0 )
View(X)

colnames(X)<-  xq
View(X)

price_data<-cbind(c_cola,X)
View(price_data)

price_data["t"]<- 1:42
View(price_data)

price_data["log_sale"]<-log(price_data["Sales"])
price_data["t_square"]<-price_data["t"]*price_data["t"]
attach(price_data)
View(price_data)

train<-price_data[1:38,]
View(train)

test<-price_data[39:42,]
View(test)

#####   LINEAR MODEL    #####

linear_model<-lm(Sales~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear # 591.5533

#####   Exponential   #####

expo_model<-lm(log_sale~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 466.248

#####   Quadratic   #####

Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 475.562

#####   Additive Seasonality    #####

sea_add_model<-lm(Sales~Q1+Q2+Q3,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 1860.024

#####   Additive Seasonality with Linear    #####

Add_sea_Linear_model<-lm(Sales~t+Q1+Q2+Q3,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 464.9829

#####   Additive Seasonality with Quadratic   #####

Add_sea_Quad_model<-lm(Sales~t+t_square+Q1+Q2+Q3,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 301.738

#####   Multiplicative Seasonality    #####

multi_sea_model<-lm(log_sale~Q1+Q2+Q3,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 1963.39

#####   Multiplicative Seasonality Linear trend   #####

multi_add_sea_model<-lm(log_sale~t+Q1+Q2+Q3,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 225.5244

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

#####   Multiplicative Seasonality Linear trend has least RMSE value

new_model <- lm(log_sale~t+Q1+Q2+Q3,data = price_data)

resid <- residuals(new_model)
resid
resid[1:10]
windows()
acf(resid,lag.max = 10)

k <- arima(resid, order=c(1,0,0))
str(k)

View(data.frame(res=resid,newresid=k$residuals))
windows()
acf(k$residuals,lag.max = 15)
pred_res<- predict(arima(k$residuals,order=c(1,0,0)),n.ahead = 4)
str(pred_res)
pred_res$pred
acf(k$residuals)
write_excel_csv(price_data,file="CocaCola_Sales_Rawdata.xlsx",col.names = F,row.names = F)

