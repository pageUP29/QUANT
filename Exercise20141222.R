# Q:调用A股某个板块或者沪深300的
#所有股票的分钟数据，并保存

library(WindR)
w.start()

#Step1:Get all the stockcode.
#000300.SH

w_300<-w.wset('IndexConstituent','date=20141219;windcode=000300.SH')
HS300<-w_300$Data[,3:5]
HS300_code<-head(HS300[,1])

#Step2:Get minute data from WIND

w_wsi_data<-w.wsi(HS300_code,"close","2015-01-05 09:00:00","2015-01-08 22:48:52")
#Step3:Save data into harddisk

write.table(HS300_data$Data,file="data/HS300.csv",sep=",")

HS300.data<-HS300_data$Data
