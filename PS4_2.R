#加载包
library(tidyr) 
library(dplyr) 
library(ggplot2)
library(forecast)

#1.构建时间序列
winddata<- read.csv(file = "2281305.csv", header = T,encoding = "UTF-8")
wind_tb <- as_tibble(winddata)
tem_mean<-wind_tb %>% 
  mutate(tem = ifelse(substr(TMP,7,7) == "1" & substr(TMP,1,5) != '+9999',
                      as.numeric(substr(TMP,1,5))*0.1,NA)) %>%
  mutate(monthly = paste(substr(DATE,1,4),substr(DATE,6,7),sep = "")) %>% 
  filter(monthly >= 201001 & monthly <= 202008) %>%
  group_by(monthly) %>% 
  summarise(monthly_mean=mean(tem,na.rm = T)) %>% 
  pull()
tem_ts <- ts(tem_mean, start=c(2010,1),end = c(2020,08) ,frequency=12)
plot(tem_ts, type="l")
str(tem_ts)

#2.分解时间序列
tem_components <- decompose(tem_ts)
plot(tem_components)
ds<-tem_components$random
Box.test(ds,type='Ljung',lag=log(length(ds)))
#得到p值为1.175e-05
#p值小于0.05，说明接受为白噪声

#3.建模
tem_ts_d1<-diff(tem_ts)
plot(tem_ts_d1)
acf(tem_ts)
pacf(tem_ts)
model <- auto.arima(tem_ts)
summary(model)

#4.预测
forecast_2months <- forecast(model, 2)
plot(forecast_2months)

#2020年9月
forecast_2months$mean[1]
#80%置信区间
forecast_2months$lower[1,1]
forecast_2months$upper[1,1]
#95%置信区间
forecast_2months$lower[1,2]
forecast_2months$upper[1,2]

#2020年10月
forecast_2months$mean[2]
#80%置信区间
forecast_2months$lower[2,1]
forecast_2months$upper[2,1]
#95%置信区间
forecast_2months$lower[2,2]
forecast_2months$upper[2,2]

#获取9月数据真值，10月数据没有
tem_mean_sep<-wind_tb %>% 
  mutate(tem = ifelse(substr(TMP,7,7) == "1" & substr(TMP,1,5) != '+9999',
                      as.numeric(substr(TMP,1,5))*0.1,NA)) %>%
  mutate(monthly = paste(substr(DATE,1,4),substr(DATE,6,7),sep = "")) %>% 
  filter(monthly == 202009 ) %>%
  group_by(monthly) %>% 
  summarise(monthly_mean=mean(tem,na.rm = T)) %>% 
  pull()

Relative_bias <- (tem_mean_sep-forecast_2months$mean[1])/tem_mean_sep
Relative_bias_per <- Relative_bias*100
print(paste("Relative bias:",Relative_bias_per,"%",sep = ""))
#Relative bias = 1.4%
#预测效果挺好，偏差很小
