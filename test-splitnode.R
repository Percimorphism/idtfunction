source("idt-function.R")

feat=iris[,1:4]
feat=as.data.frame(feat)

result1=splitnode(feat)
print(result1)

result11=splitnode(result1[[1]])
print(result11)

result111=splitnode(result11[[1]])
print(result111)

result1111=splitnode(result111[[1]])
print(result1111)



result12=splitnode(result1[[2]])
print(result12)

result121=splitnode(result12[[1]])
print(result121)

result1211=splitnode(result121[[1]])
print(result1211)
