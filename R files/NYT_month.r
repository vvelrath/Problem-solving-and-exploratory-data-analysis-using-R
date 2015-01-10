days <- sample(1:31, 6, replace=F)
j=1
for(i in days)
{
data1 <- read.csv(url(paste("http://stat.columbia.edu/~rachel/datasets/nyt",i,".csv",sep="")))
data1$agecat <-cut(data1$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
data1$day=j
if(j==1) data<-data1
else 
{
data2<-rbind(data,data1)
data<-data2
}
head(data)
j=j+1
}