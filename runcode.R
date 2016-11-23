source("idt-function.R")

data1=iris[,1:4]

data3=rbind(data1, data1, data1)

doIdt(data3,maxdepth=5, minnum=45)
#plottree(iris[,1:4],maxdepth=5, minnum=45)
