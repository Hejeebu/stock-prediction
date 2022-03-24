library(neuralnet)
data=read.csv('C:/Users/Srirama/Downloads/GME.csv')
names(data)
attach(data)
str(data)
scaleddata<-scale(data)
normalize<-function(x){
  return((x-min(x))/max(x)-min(x))
}

maxmindata = as.data.frame(lapply(data,normalize
                                  ))
trainset<-maxmindata[1:150,]

testset<-maxmindata[151:200,]
names(data)

library(neuralnet)
nn<-neuralnet(Close~Open+High+Low+Adj.Close+Volume,data=trainset,hidden = c(2,1),linear.output = T,threshold = 0.01)
nn$result.matrix
plot(nn)

nn$result.matrix
temp_test<-subset(testset,select=c("Open","High","Low","Adj.Close","Volume"))
head(temp_test)
nn.results<-compute(nn,temp_test)
results<-data.frame(actual=testset$Close,predict=nn.results$net.result)

results

df <- data.frame(
  expand.grid(dividend = c("1", "0,", "2"), fcfps = 0.55:5),
  = round(runif(9, 1000, 20000), 0)
)
countries