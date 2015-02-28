
#Part1 调用相关包、初始化参数及获取数据

#0.调用相关包
library(dplyr)
library(WindR)
library(quantmod)
library(xts)
library(lubridate)

#1.初始化参数

options(digits=6)
w.start()#创建Wind连接
fields<-c('close')#设置获取字段，目前只考虑一个字段
begintime<-"2015-01-05 09:00:00"#设置开始时间
begindate<-as.Date(begintime)
endtime<-"2015-01-08 16:00:00"#设置结束时间
enddate<-as.Date(endtime)
dayrange<-(w.tdays("20150105","20150108"))$Data$DATETIME
#dayrange<-c("2015-01-05","2015-01-06","2015-01-07","2015-01-08")


#提取沪深300指数的股票代码
wsi.codes<-w.wset('IndexConstituent','windcode=000300.SH;field=wind_code')
wsi.code<-wsi.codes$Data[,2]
wsi.codes<-wsi.code[1:30]#测试用数据，同时最大不能超过30支股票
IF.codes<-"IF.CFE"#沪深300期货代码
CGB.codes<-"CGB3M.WI"#3个月国债代码

#2.循环读取沪深300个股分钟数据
read.wsi<-function(wsi.codes,fields,begintime,endtime){
   
        while (1)#检查报错
        {
            data<-w.wsi(wsi.codes,fields,begintime,endtime)
            
            if(w_wsi_data$ErrorCode==0) 
            {
                break
            }else{
                print(paste('error in getting code =',code,'errorcode=',w_wsi_data$ErrorCode))
            }
        }
  
    return(data$Data[,-2])
 }
HS300data<-read.wsi(wsi.codes,fields,begintime,endtime)
#3.读取提取沪深300股指期货分钟收益率数据
IF.CFE<-w.wsi(IF.codes,fields,begintime,endtime)
CFEdata<-IF.CFE$Data
#4.提取3个月国债日收益率数据并转换为分钟收益率
CGB3M<-w.wsd(CGB.codes,fields,begindate,enddate,"Fill=Previous")
CGB3M$Data$ret.CGB3M<-log(1+CGB3M$Data$CLOSE/100)/240
CGB3Mdata<-CGB3M$Data
#5.保存所有从服务器上读取的数据，避免重复读取

#Part2 计算收益率，为回归分析准备数据

#1.计算Hs300股票的分钟收益率，并将期货、国债收益率合并在一个数据集。
#  对股票代码和交易日期进行循环遍历

cal.ret<-function(wsi.codes,dayrange,HS300data,CFEdata,CGB3Mdata){
    data.ret<-c()
    ld<-length(dayrange)
    for(code in wsi.codes){
        for (i in 1:ld){
        data.S<-HS300data[HS300data$windcode==code & as.Date(HS300data$DATETIME)== dayrange[i],]    
        data.HS<-CFEdata[as.Date(CFEdata$DATETIME)== dayrange[i],]    
        data.CGB<-CGB3Mdata[as.Date(CGB3Mdata$DATETIME)== dayrange[i],]    
        #此处为回归合并data.S,data.HS,data.CGB
        data<-merge(data.S,data.HS,by="DATETIME")
        data<-merge(data.CGB,transform(data,DATETIME=as.Date(DATETIME)),,by='DATETIME')
        names(data)[c(1,5,6)]<-c("date","close.S","close.CFE")
        #ret<- Return.calculate(data,"log")，quantmod中的方法
        data$ret.S<-c(NA,diff(log(data$close.S)))
        data$ret.CFE<-c(NA,diff(log(data$close.CFE)))
        data<-data[,c(1,4,3,7,8)]
        if(length(data.ret)==0)
        {
            data.ret<-data
        }else{
            data.ret<-rbind(data.ret,data)#叠加
        }
        }
    }
return(data.ret)
}
data.ret<-cal.ret(wsi.codes,dayrange,HS300data,CFEdata,CGB3Mdata)

#遍历股票代码和交易日期，计算beta系数及隔天的epsiron。
cal.epsiron<-function(data.ret,wsi.codes,dayrange){
    data.epsiron<-c()
    beta<-c()
    epsiron<-c()
    ld<-length(dayrange)
    for(code in wsi.codes){
        for (i in 1:ld){
          data<-data.ret[data.ret$windcode==code & data.ret$date== dayrange[i],]    
          data<-data[complete.cases(data),] 
          #由于停牌等数据缺失，排除包含NA,NaN的记录
          #需要判断回归自变量、因变量是否全为NaN，回归obs不小于30
          if(length(data[,1])>=30){
          lm.S<-lm(data$ret.S~I(data$ret.CFE-data$ret.CGB3M),na.action=na.exclude)
          beta[i]<-as.numeric(lm.S$coefficients[2]) 
          #print(beta[i])
          }
          else{
              beta[i]<-NA   
              #print(beta[i])      
          }
          if(i==1 || is.na(beta[i-1]) || is.nan(beta[i-1]) || is.null(beta[i-1])){
              epsiron[i]<-NA
          }
          else{
              epsiron[i]<-mean(data$ret.S-beta[i-1]*(data$ret.CFE-data$ret.CGB3M),na.rm=TRUE)   
          }
          if(length(data.epsiron)==0){
              data.epsiron<-data.frame(code=code,date=dayrange[i],beta=beta[i],epsiron=epsiron[i])
          }
          else{
              data.epsiron<-rbind(data.epsiron,data.frame(code=code,date=dayrange[i],beta=beta[i],epsiron=epsiron[i]))
          }
          
         #cat(code,date,beta[i],epsiron[i])
        }
    }
return(data.epsiron)   
}
data.epsiron<-cal.epsiron(data.ret,wsi.codes,dayrange)
