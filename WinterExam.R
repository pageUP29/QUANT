
#Part1 ������ذ�����ʼ����������ȡ����

#0.������ذ�
library(dplyr)
library(WindR)
library(quantmod)
library(xts)
library(lubridate)

#1.��ʼ������

options(digits=6)
w.start()#����Wind����
fields<-c('close')#���û�ȡ�ֶΣ�Ŀǰֻ����һ���ֶ�
begintime<-"2015-01-05 09:00:00"#���ÿ�ʼʱ��
begindate<-as.Date(begintime)
endtime<-"2015-01-08 16:00:00"#���ý���ʱ��
enddate<-as.Date(endtime)
dayrange<-(w.tdays("20150105","20150108"))$Data$DATETIME
#dayrange<-c("2015-01-05","2015-01-06","2015-01-07","2015-01-08")


#��ȡ����300ָ���Ĺ�Ʊ����
wsi.codes<-w.wset('IndexConstituent','windcode=000300.SH;field=wind_code')
wsi.code<-wsi.codes$Data[,2]
wsi.codes<-wsi.code[1:30]#���������ݣ�ͬʱ����ܳ���30֧��Ʊ
IF.codes<-"IF.CFE"#����300�ڻ�����
CGB.codes<-"CGB3M.WI"#3���¹�ծ����

#2.ѭ����ȡ����300���ɷ�������
read.wsi<-function(wsi.codes,fields,begintime,endtime){
   
        while (1)#��鱨��
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
#3.��ȡ��ȡ����300��ָ�ڻ���������������
IF.CFE<-w.wsi(IF.codes,fields,begintime,endtime)
CFEdata<-IF.CFE$Data
#4.��ȡ3���¹�ծ�����������ݲ�ת��Ϊ����������
CGB3M<-w.wsd(CGB.codes,fields,begindate,enddate,"Fill=Previous")
CGB3M$Data$ret.CGB3M<-log(1+CGB3M$Data$CLOSE/100)/240
CGB3Mdata<-CGB3M$Data
#5.�������дӷ������϶�ȡ�����ݣ������ظ���ȡ

#Part2 ���������ʣ�Ϊ�ع����׼������

#1.����Hs300��Ʊ�ķ��������ʣ������ڻ�����ծ�����ʺϲ���һ�����ݼ���
#  �Թ�Ʊ����ͽ������ڽ���ѭ������

cal.ret<-function(wsi.codes,dayrange,HS300data,CFEdata,CGB3Mdata){
    data.ret<-c()
    ld<-length(dayrange)
    for(code in wsi.codes){
        for (i in 1:ld){
        data.S<-HS300data[HS300data$windcode==code & as.Date(HS300data$DATETIME)== dayrange[i],]    
        data.HS<-CFEdata[as.Date(CFEdata$DATETIME)== dayrange[i],]    
        data.CGB<-CGB3Mdata[as.Date(CGB3Mdata$DATETIME)== dayrange[i],]    
        #�˴�Ϊ�ع�ϲ�data.S,data.HS,data.CGB
        data<-merge(data.S,data.HS,by="DATETIME")
        data<-merge(data.CGB,transform(data,DATETIME=as.Date(DATETIME)),,by='DATETIME')
        names(data)[c(1,5,6)]<-c("date","close.S","close.CFE")
        #ret<- Return.calculate(data,"log")��quantmod�еķ���
        data$ret.S<-c(NA,diff(log(data$close.S)))
        data$ret.CFE<-c(NA,diff(log(data$close.CFE)))
        data<-data[,c(1,4,3,7,8)]
        if(length(data.ret)==0)
        {
            data.ret<-data
        }else{
            data.ret<-rbind(data.ret,data)#����
        }
        }
    }
return(data.ret)
}
data.ret<-cal.ret(wsi.codes,dayrange,HS300data,CFEdata,CGB3Mdata)

#������Ʊ����ͽ������ڣ�����betaϵ���������epsiron��
cal.epsiron<-function(data.ret,wsi.codes,dayrange){
    data.epsiron<-c()
    beta<-c()
    epsiron<-c()
    ld<-length(dayrange)
    for(code in wsi.codes){
        for (i in 1:ld){
          data<-data.ret[data.ret$windcode==code & data.ret$date== dayrange[i],]    
          data<-data[complete.cases(data),] 
          #����ͣ�Ƶ�����ȱʧ���ų�����NA,NaN�ļ�¼
          #��Ҫ�жϻع��Ա�����������Ƿ�ȫΪNaN���ع�obs��С��30
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