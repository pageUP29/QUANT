
#Training 
#Import Multi Files

#Method one: Using loop
setwd("D:/data")  
fileName <- dir()  
N=length(fileName)  

datalist <- vector("list", N) # ����һ���ձ�  

for(i in 1:N){  
    datalist[[i]]=read.table(fileName[i],header=TRUE)  
}  

#Method one: Using Vector
setwd("D:/data")  
fileName <- dir()  

read.file <- function(File){  
    read.table(File,header=TRUE)  
} # �����ȡ���ݵĺ���  

datalist <- lapply(fileName,read.file)  
tapply()
by(df,factor,fun)
dplyr
group
#Use system.time() to calcuate the time cost.