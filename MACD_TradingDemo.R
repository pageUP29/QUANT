#  基于R的Macd沪深300收盘价交易回测Demo(GUI制作)
#  by fantuanxiaot
#  ##################################################################
#  ##################################################################
#  设置当前的工作目录
setwd("D:/MyDriversRoad/R_files12")
#  读取数据
library(RODBC)
excel_conn<-odbcConnectExcel('MACD_HS300.xls')
HS300.Data<-sqlFetch(excel_conn,'Sheet1')
#  关闭DB连接
close(excel_conn)
#  查看数据
head(HS300.Data,30)
View(HS300.Data)
#  以上为基于ODBC连接R语言和Excel的数据I/O步骤，并将数据读取
#  ##################################################################
#  准备重要的函数MACD
macd<-function(Stock,short_period,
               long_period,dea_period) {
  #  Stock是股票的价格
  #  short_period是短期
  #  long_period是长期
  #  还有dea_period
  #  short_period<-5
  #  long_period<-25
  #  dea_period<-9
  length_stock<-length(Stock)
  EMA_short<-rep(0,length_stock)
  EMA_long<-rep(0,length_stock)
  stock_diff<-rep(0,length_stock)
  stock_dea<-rep(0,length_stock)
  stock_macd<-rep(0,length_stock)
  #  计算各类指标初始值
  EMA_short[1]<-Stock[1]
  EMA_long[1]<-Stock[1]
  stock_diff[1]<-0
  stock_dea[1]<-0
  stock_macd[1]<-0
  #  计算各类指标
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
#  获取回测的指标:共500个序列
BackTestSample<-HS300.Data[,"收"]
BackTestSample<-BackTestSample[c((length(BackTestSample)-499):length(BackTestSample))]
#  ##################################################################
#  最大回撤比率的函数
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
#  最大回撤比率的函数
#  ##################################################################
#  ##################################################################
#  以下是回测的函数
MACD_BackTest<-function(short_period,long_period,dea_period)
{
  Stock_macd<-macd(BackTestSample,short_period,long_period,
                   dea_period)
  Stock<-BackTestSample
  stock_return<-NULL
  #  初始资金设为10000
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
  #  ####交易的开始####
  #  #############注意dynamic_money#############
  #  #############注意market_pos的编程情况#############
  for (i in 2:n) {
    #  ####先判断入仓的情况####
    #  ####先判断入仓的情况####
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
      #  ####再判断平仓的情况####
      #  ####再判断平仓的情况####
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
        #  ####再空头入仓####
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
        #  ####再多头入仓####
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
  #  构建回测结果返回值
  Answer<-list()
  #  动态资金曲线
  Answer$Dynamic<-dynamic_money
  #  静态资金曲线
  Answer$Static<-static_money
  Answer$Cash<-cash_money
  return(Answer)
}
#  ############################一个检测MACD_BackTest(5,25,9)#################
#  ##################################################################
#  ############################以下是Macd回测的GUI制作#####################
#  载入需要做GUI的Package
library(gWidgetsRGtk2)
#  参数的设定
short_period_value<-c(3,4,5,6,7,8)
long_period_value<-c(20,21,22,23,24,25,26,27,28,29,30)
dea_period_value<-c(9,10,11,12,13,14,15)
#  R环境下的GUI建立
Window<-gwindow("基于HS300的MACD回测GUI")
MACD.Window<-ggroup(cont=Window,horizontal=TRUE)
MACD.ParameterSet.Window<-ggroup(cont=MACD.Window,horizontal=FALSE)
MACD.Gframe.short<-gframe("short_period的选择",cont=MACD.ParameterSet.Window)
MACD.Gframe.long<-gframe("long_period的选择",cont=MACD.ParameterSet.Window)
MACD.Gframe.dea<-gframe("dea_period的选择",cont=MACD.ParameterSet.Window)
MACD.Trading<-gframe("选择回测交易",cont=MACD.ParameterSet.Window)
MACD.Trading.Exit<-gframe("退出回测",cont=MACD.ParameterSet.Window)
#  R环境下的GUI建立
short.list<-gdroplist(short_period_value,cont=MACD.Gframe.short)
long.list<-gdroplist(long_period_value,cont=MACD.Gframe.long)
dea.list<-gdroplist(dea_period_value,cont=MACD.Gframe.dea)
Trading.Exit.Button<-gbutton("退出交易",cont=MACD.Trading.Exit,handler=function(h,...) dispose(Window))
#  R环境下的GUI建立
#  TradingFun的构建
TradingFun<-function(h,...)
{
  #  先得到设置的参数的选择
  short_period<-as.numeric(svalue(short.list))
  long_period<-as.numeric(svalue(long.list))
  dea_period<-as.numeric(svalue(dea.list))
  MACD.BackTest.Result<-MACD_BackTest(short_period,
                                      long_period,dea_period)
  #  得到回撤比率
  BackTest.RetreatRatio<-
    Retreat_Ratio(MACD.BackTest.Result$Dynamic)
  #  作图(第一幅图)
  Length<-length(BackTest.RetreatRatio)
  par(mfrow=c(1,2),family='serif')
  c1<-c(c(0:Length),c(Length:0))
  c2<-c(c(0,BackTest.RetreatRatio),rep(0,Length+1))
  plot(c1,c2,type='n',xlab='Time',
       ylab='Retreat Ratio')
  polygon(c1,c2,col='green',border='blue')
  title('最大回测比率')
  #  作图(第二幅图)
  plot(MACD.BackTest.Result$Dynamic,col='red',
       type='s',lwd=2,ylim=c(0,12000))
  lines(MACD.BackTest.Result$Static,col='blue',type='s',lwd=2)
  lines(MACD.BackTest.Result$Cash,col='gold',type='s',lwd=2)
  lines(c(1:length(MACD.BackTest.Result$Cash)),
        rep(10000,length(MACD.BackTest.Result$Cash)),
        col='black',type='c',lwd=2)
  legend(100,8000,lty=c(1,1,1),
         legend=c("动态资金曲线","静态资金曲线","现金曲线"),
         col=c("red","blue","gold"),hori=FALSE,bty="o")
  
}
Trading.Button<-gbutton("确定交易",cont=MACD.Trading,handler=TradingFun)
#  在旁边画图
MACD.Plot<-ggroup(cont=MACD.Window,expand=TRUE)
add(MACD.Plot,ggraphics())
#  ##################################################################
#  ##################################################################