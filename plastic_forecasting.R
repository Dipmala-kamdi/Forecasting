library(readr)
setwd("D:\\excelr_DS\\R code\\Forecasting")

# read the Amtrack data
plastic<-read.csv(file.choose()) 
View(plastic) #  12 months 
plot(plastic$Sales,type="o")

# Creating dummies for 12 months
X<- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )
View(X)

# Assigning month names
colnames(X)<-month.abb  
View(X)
plastic_data<-cbind(plastic,X)
View(plastic_data)

plastic_data["t"]<- 1:60
View(plastic_data)
plastic_data["log_sales"]<-log(plastic_data["Sales"])
plastic_data["t_square"]<-plastic_data["t"]*plastic_data["t"]
attach(plastic_data)
View(plastic_data)

train<-plastic_data[1:48,]
View(train)

test<-plastic_data[48:60,]
View(test)

########################### LINEAR MODEL    #####

linear_model<-lm(Sales~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear # 254.680


######################### Exponential   #####

expo_model<-lm(log_sales~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 261.856

######################### Quadratic   #####

Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 290.828


######################### Additive Seasonality    #####

sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 235.176

######################## Additive Seasonality with Linear   #####

Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 131.903

######################## Additive Seasonality with Quadratic    #####

Add_sea_Quad_model<-lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 209.753

######################## Multiplicative Seasonality   #####

multi_sea_model<-lm(log_sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 239.875

######################## Multiplicative Seasonality Linear trend    #####

multi_add_sea_model<-lm(log_sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 156.866

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multiplicative Seasonality Linear trend has least RMSE value

new_model <- lm(log_sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = plastic_data)

resid <- residuals(new_model)
resid
resid[1:10]

acf(resid,lag.max = 10)

k <- arima(resid, order=c(1,0,0))
str(k)

View(data.frame(res=resid,newresid=k$residuals))

acf(k$residuals,lag.max = 15)
pred_res<- predict(arima(k$residuals,order=c(1,0,0)),n.ahead = 12)
str(pred_res)
pred_res$pred
acf(k$residuals)
write.csv(plastic_data,file="PlasticSales.csv",col.names = F,row.names = F)
