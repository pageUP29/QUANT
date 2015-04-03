#MACD简单回测例子

#加载包,打开控件
library(WindR)
w.start()
w_wsd_data<-w.wsd("600000.SH","close,MACD","2015-03-04","2015-04-03","MACD_L=26;MACD_S=12;MACD_N=9;MACD_IO=1;Fill=Previous")$Data
str(w_wsd_data)
#用户输入部分 
begindate<-'20150301'#输入交易开始时间
enddate<-'20150331' #输入交易终止时间
Code<-'000001.SZ'
strategy<-'Simple MACD Strategy'#输入回测策略名

#创建策略名
out<-w.bktstart(strategy,begindate,enddate,'InitialFund=100000') 
bktid <- out$Data$BktId#策略Id


#创建交易信号
#如果MACD由负转正，买入；由正转负，卖出

T_data<-w.wsd(Code,"close,MACD",begindate,enddate,"MACD_L=26;MACD_S=12;MACD_N=9;MACD_IO=1;Fill=Previous")$Data

n<-nrow(T_data)-1
for (i in 1:n){
  if (T_data[i,3]<0 & T_data[i+1,3]>0){
    #如果MACD由负转正，买入 
    #查资金
    q_c<-w.bktquery('Capital',T_data[i+1,1])
    volume<-floor(q_c$Data$AvailableFund/(T_data[i+1,2]*100))*100
    b_out<-w.bktorder(paste(T_data[i+1,1],' 14:59:00'),Code,'buy',volume)
  }  
}


#查仓位,最后一天清仓
p_out<-w.bktquery('position',T_data[n+1,1])
p_code<-p_out$Data$SecurityCode
p_volume<-p_out$Data$SecurityVolume
s_out<-w.bktorder(paste(T_data[n+1,1],' 14:59:00'),p_code,'sell',p_volume)

bkt_end<-w.bktend( )#结束回测
w.bktsummary(bkt_end$Data[,1],'Trade')#查看回测摘要

w.bktdelete(BKTID,Options)
w.bktstrategy()
w.bktstrategy(type=0)#查询现有策略状态
w.bktstatus()

w.bktdelete()

#课后练习
#任选5只股票，下载这些股票历史交易数据（日），
#计算DIFF,DEA,MACD，并绘图，标记出所有的金叉和死叉

