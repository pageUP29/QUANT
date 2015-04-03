#  ����R��Macd����300���̼۽��׻ز�Demo(GUI����)
#  by fantuanxiaot
#  ##################################################################
#  ##################################################################
#  ���õ�ǰ�Ĺ���Ŀ¼
setwd("D:/MyDriversRoad/R_files12")
#  ��ȡ����
library(RODBC)
excel_conn<-odbcConnectExcel('MACD_HS300.xls')
HS300.Data<-sqlFetch(excel_conn,'Sheet1')
#  �ر�DB����
close(excel_conn)
#  �鿴����
head(HS300.Data,30)
View(HS300.Data)
#  ����Ϊ����ODBC����R���Ժ�Excel������I/O���裬�������ݶ�ȡ
#  ##################################################################
#  ׼����Ҫ�ĺ���MACD
macd<-function(Stock,short_period,
               long_period,dea_period) {
  #  Stock�ǹ�Ʊ�ļ۸�
  #  short_period�Ƕ���
  #  long_period�ǳ���
  #  ����dea_period
  #  short_period<-5
  #  long_period<-25
  #  dea_period<-9
  length_stock<-length(Stock)
  EMA_short<-rep(0,length_stock)
  EMA_long<-rep(0,length_stock)
  stock_diff<-rep(0,length_stock)
  stock_dea<-rep(0,length_stock)
  stock_macd<-rep(0,length_stock)
  #  �������ָ���ʼֵ
  EMA_short[1]<-Stock[1]
  EMA_long[1]<-Stock[1]
  stock_diff[1]<-0
  stock_dea[1]<-0
  stock_macd[1]<-0
  #  �������ָ��
  for (t in 2:length_stock) {
    EMA_short[t]<-Stock[t]*2/(short_period+1)+
      EMA_short[t-1]*(short_period-1)/(short_period+1)
    EMA_long[t]<-Stock[t]*2/(long_period+1)+
      EMA_long[t-1]*(long_period-1)/(long_period+1)
    stock_diff[t]<-EMA_short[t]-EMA_long[t]
    stock_dea[t]<-stock_diff[t]*2/(dea_period+1)+
      stock_dea[t-1]*(dea_period-1)/(dea_period+1)
    stock_macd[t]<-2*(stock_diff[t]-stock_dea[t])
  }
  return(stock_macd)
}
#  ##################################################################
#  ��ȡ�ز��ָ��:��500������
BackTestSample<-HS300.Data[,"��"]
BackTestSample<-BackTestSample[c((length(BackTestSample)-499):length(BackTestSample))]
#  ##################################################################
#  ���س����ʵĺ���
Retreat_Ratio<-function(stock_return1) {
  N<-length(stock_return1)
  RetraceRatio<-rep(0,N)
  for (i in 2:N) {
    C<-max(stock_return1[1:i])
    if (C==stock_return1[i]) {
      RetraceRatio[i]<-0
    } else {
      RetraceRatio[i]<-(stock_return1[i]-C)/C
    }
  }
  return(RetraceRatio)
}
#  ���س����ʵĺ���
#  ##################################################################
#  ##################################################################
#  �����ǻز�ĺ���
MACD_BackTest<-function(short_period,long_period,dea_period)
{
  Stock_macd<-macd(BackTestSample,short_period,long_period,
                   dea_period)
  Stock<-BackTestSample
  stock_return<-NULL
  #  ��ʼ�ʽ���Ϊ10000
  n<-length(BackTestSample)
  dynamic_money<-c(10000,rep(0,n-1))
  cash_money<-c(10000,rep(0,n-1))
  static_money<-c(10000,rep(0,n-1))
  margin_ratio<-0.08
  cost_ratio<-0.003
  in_price<-NULL
  out_price<-NULL
  one_return<-NULL
  market_pos<-0
  cost_in<-NULL
  cost_out<-NULL
  margin_call_money<-rep(0,n)
  margin_put_money<-rep(0,n)
  #  ####���׵Ŀ�ʼ####
  #  #############ע��dynamic_money#############
  #  #############ע��market_pos�ı�����#############
  for (i in 2:n) {
    #  ####���ж���ֵ����####
    #  ####���ж���ֵ����####
    if (market_pos==0) {
      if (Stock_macd[i-1]<0 && Stock_macd[i]>=0) {
        in_price<-Stock[i]
        cost_in<-in_price*cost_ratio
        market_pos<-1
        dynamic_money[i]<-dynamic_money[i-1]-cost_in
        static_money[i]<-static_money[i-1]
        cash_money[i]<-cash_money[i-1]-cost_in-
          in_price*margin_ratio-in_price
        margin_call_money[i]<-in_price*margin_ratio
      } else if (Stock_macd[i-1]>0 && Stock_macd[i]<=0) {
        in_price<-Stock[i]
        cost_in<-in_price*cost_ratio
        market_pos<--1
        dynamic_money[i]<-dynamic_money[i-1]-cost_in
        static_money[i]<-static_money[i-1]
        cash_money[i]<-cash_money[i-1]-cost_in-
          in_price*margin_ratio+in_price  
        margin_put_money[i]<-in_price*margin_ratio
      } else {
        dynamic_money[i]<-dynamic_money[i-1]
        cash_money[i]<-cash_money[i-1]
        static_money[i]<-static_money[i-1]
        market_pos<-0
      }
    } else if (market_pos==1) {
      #  ####���ж�ƽ�ֵ����####
      #  ####���ж�ƽ�ֵ����####
      if (Stock_macd[i-1]>0 && Stock_macd[i]<=0) {
        out_price<-Stock[i]
        cost_out<-out_price*cost_ratio
        market_pos<-0
        dynamic_money[i]<-dynamic_money[i-1]+(out_price-Stock[i-1])-
          cost_out
        one_return<-out_price-in_price-cost_in-cost_out
        stock_return<-c(stock_return,one_return)
        static_money[i]<-static_money[i-1]+one_return
        cash_money[i]<-cash_money[i-1]-cost_out+margin_call_money[i-1]+
          out_price
        #  ####�ٿ�ͷ���####
        if (i!=n) {
          market_pos<--1
          in_price<-Stock[i]
          cost_in<-in_price*cost_ratio
          dynamic_money[i]<-dynamic_money[i]-cost_in
          static_money[i]<-static_money[i]
          cash_money[i]<-cash_money[i]-cost_in+in_price-
            in_price*margin_ratio
          margin_put_money[i]<-in_price*margin_ratio
        }
      } else {
        market_pos<-1
        dynamic_money[i]<-dynamic_money[i-1]+Stock[i]-Stock[i-1]
        static_money[i]<-static_money[i-1]
        cash_money[i]<-cash_money[i-1]+
          margin_ratio*(Stock[i]-Stock[i-1])
        margin_call_money[i]<-margin_call_money[i-1]-
          margin_ratio*(Stock[i]-Stock[i-1])
      }
    } else if (market_pos==-1) {
      if (Stock_macd[i-1]<0 && Stock_macd[i]>=0) {
        out_price<-Stock[i]
        cost_out<-out_price*cost_ratio
        market_pos<-0
        dynamic_money[i]<-dynamic_money[i-1]-out_price+Stock[i-1]-
          cost_out
        one_return<-in_price-out_price-cost_out-cost_in
        stock_return<-c(stock_return,one_return)
        static_money[i]<-static_money[i-1]+one_return
        cash_money[i]<-cash_money[i-1]-cost_out+
          margin_put_money[i-1]-out_price
        #  ####�ٶ�ͷ���####
        if (i!=n) {
          market_pos<-1
          in_price<-Stock[i]
          cost_in<-in_price*cost_ratio
          dynamic_money[i]<-dynamic_money[i]-cost_in
          static_money[i]<-static_money[i]
          cash_money[i]<-cash_money[i]-
            cost_in-in_price*margin_ratio-in_price
          margin_call_money[i]<-in_price*margin_ratio
        }
      } else {
        market_pos<--1
        dynamic_money[i]<-dynamic_money[i-1]-Stock[i]+Stock[i-1]
        static_money[i]<-static_money[i-1]
        cash_money[i]<-cash_money[i-1]+
          margin_ratio*(Stock[i-1]-Stock[i])
        margin_put_money[i]<-margin_put_money[i-1]-
          margin_ratio*(Stock[i-1]-Stock[i])
      }
    }
    if (i==n) {
      if (market_pos==1) {
        out_price<-Stock[i]
        market_pos<-0
        cost_out<-out_price*margin_ratio
        dynamic_money[i]<-dynamic_money[i-1]+out_price-Stock[i-1]-
          cost_out
        one_return<-out_price-in_price-cost_out-cost_in
        stock_return<-c(stock_return,one_return)
        static_money[i]<-static_money[i-1]+one_return
        cash_money[i]<-cash_money[i-1]-cost_out+out_price+
          margin_call_money[i-1]
      }
      if (market_pos==-1) {
        out_price<-Stock[i]
        market_pos<-0
        cost_out<-out_price*margin_ratio
        dynamic_money[i]<-dynamic_money[i-1]-out_price+Stock[i-1]-
          cost_out
        one_return<-in_price-out_price-cost_out-cost_in
        stock_return<-c(stock_return,one_return)
        static_money[i]<-static_money[i-1]+one_return
        cash_money[i]<-cash_money[i-1]-cost_out-out_price+
          margin_put_money[i-1]
      }
    }
  }
  #  �����ز�������ֵ
  Answer<-list()
  #  ��̬�ʽ�����
  Answer$Dynamic<-dynamic_money
  #  ��̬�ʽ�����
  Answer$Static<-static_money
  Answer$Cash<-cash_money
  return(Answer)
}
#  ############################һ�����MACD_BackTest(5,25,9)#################
#  ##################################################################
#  ############################������Macd�ز��GUI����#####################
#  ������Ҫ��GUI��Package
library(gWidgetsRGtk2)
#  �������趨
short_period_value<-c(3,4,5,6,7,8)
long_period_value<-c(20,21,22,23,24,25,26,27,28,29,30)
dea_period_value<-c(9,10,11,12,13,14,15)
#  R�����µ�GUI����
Window<-gwindow("����HS300��MACD�ز�GUI")
MACD.Window<-ggroup(cont=Window,horizontal=TRUE)
MACD.ParameterSet.Window<-ggroup(cont=MACD.Window,horizontal=FALSE)
MACD.Gframe.short<-gframe("short_period��ѡ��",cont=MACD.ParameterSet.Window)
MACD.Gframe.long<-gframe("long_period��ѡ��",cont=MACD.ParameterSet.Window)
MACD.Gframe.dea<-gframe("dea_period��ѡ��",cont=MACD.ParameterSet.Window)
MACD.Trading<-gframe("ѡ��ز⽻��",cont=MACD.ParameterSet.Window)
MACD.Trading.Exit<-gframe("�˳��ز�",cont=MACD.ParameterSet.Window)
#  R�����µ�GUI����
short.list<-gdroplist(short_period_value,cont=MACD.Gframe.short)
long.list<-gdroplist(long_period_value,cont=MACD.Gframe.long)
dea.list<-gdroplist(dea_period_value,cont=MACD.Gframe.dea)
Trading.Exit.Button<-gbutton("�˳�����",cont=MACD.Trading.Exit,handler=function(h,...) dispose(Window))
#  R�����µ�GUI����
#  TradingFun�Ĺ���
TradingFun<-function(h,...)
{
  #  �ȵõ����õĲ�����ѡ��
  short_period<-as.numeric(svalue(short.list))
  long_period<-as.numeric(svalue(long.list))
  dea_period<-as.numeric(svalue(dea.list))
  MACD.BackTest.Result<-MACD_BackTest(short_period,
                                      long_period,dea_period)
  #  �õ��س�����
  BackTest.RetreatRatio<-
    Retreat_Ratio(MACD.BackTest.Result$Dynamic)
  #  ��ͼ(��һ��ͼ)
  Length<-length(BackTest.RetreatRatio)
  par(mfrow=c(1,2),family='serif')
  c1<-c(c(0:Length),c(Length:0))
  c2<-c(c(0,BackTest.RetreatRatio),rep(0,Length+1))
  plot(c1,c2,type='n',xlab='Time',
       ylab='Retreat Ratio')
  polygon(c1,c2,col='green',border='blue')
  title('���ز����')
  #  ��ͼ(�ڶ���ͼ)
  plot(MACD.BackTest.Result$Dynamic,col='red',
       type='s',lwd=2,ylim=c(0,12000))
  lines(MACD.BackTest.Result$Static,col='blue',type='s',lwd=2)
  lines(MACD.BackTest.Result$Cash,col='gold',type='s',lwd=2)
  lines(c(1:length(MACD.BackTest.Result$Cash)),
        rep(10000,length(MACD.BackTest.Result$Cash)),
        col='black',type='c',lwd=2)
  legend(100,8000,lty=c(1,1,1),
         legend=c("��̬�ʽ�����","��̬�ʽ�����","�ֽ�����"),
         col=c("red","blue","gold"),hori=FALSE,bty="o")
  
}
Trading.Button<-gbutton("ȷ������",cont=MACD.Trading,handler=TradingFun)
#  ���Ա߻�ͼ
MACD.Plot<-ggroup(cont=MACD.Window,expand=TRUE)
add(MACD.Plot,ggraphics())
#  ##################################################################
#  ##################################################################