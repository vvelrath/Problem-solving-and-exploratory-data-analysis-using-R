data1 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt1.csv"))
# categorize
head(data1)
data1$agecat <-cut(data1$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
# view
summary(data1)
# brackets
install.packages("doBy")
library("doBy")
siterange <- function(x){c(length(x), min(x), mean(x), max(x), var(x))}
summaryBy(Age~agecat, data =data1, FUN=siterange)
# so only signed in users have ages and genders
summaryBy(Gender+Signed_In+Impressions+Clicks~agecat,
data =data1)
# plot
install.packages("ggplot2")
library(ggplot2)
d<-ggplot(data1, aes(x=Impressions, fill=agecat))
d<-d+geom_histogram(binwidth=1)
d
d<-ggplot(data1, aes(x=agecat, y=Impressions, fill=agecat))
d<-d+geom_boxplot()
d
# create click thru rate
# we don't care about clicks if there are no impressions
# if there are clicks with no imps my assumptions about
# this data are wrong
data1$hasimps <-cut(data1$Impressions,c(-Inf,0,Inf))
summaryBy(Clicks~hasimps, data =data1, FUN=siterange)
d<-ggplot(subset(data1, Impressions>0), aes(x=Clicks/Impressions,colour=agecat))
d<-d + geom_density()
d
d<-ggplot(subset(data1, Clicks>0), aes(x=Clicks/Impressions,colour=agecat)) 
d<-d+ geom_density()
d
d<-ggplot(subset(data1, Clicks>0), aes(x=agecat, y=Clicks,fill=agecat))
d<-d + geom_boxplot()
d
d<-ggplot(subset(data1, Clicks>0), aes(x=Clicks, colour=agecat))
d<-d+ geom_density()
d
#Males vs. Females
data1.males<-data1[data1$Gender=="1",]
data1.females<-data1[data1$Gender=="0",]
ggplot(subset(data1.males, Clicks>0), aes(x=Clicks/Impressions,colour=agecat))+geom_density()
ggplot(subset(data1.females, Clicks>0), aes(x=Clicks/Impressions,colour=agecat))+geom_density()
#18 Year old Male vs. 18 Year old Female
data1.females<-data1.females[data1.females$Age<18,]
data1.males<-data1.males[data1.males$Age<18,]
ggplot(subset(data1.males, Clicks>0), aes(x=Clicks/Impressions,colour=agecat))+geom_density()
ggplot(subset(data1.females, Clicks>0), aes(x=Clicks/Impressions,colour=agecat))+geom_density()
#Logged In Users versus Not Logged In Users
data1.signedin<-data1[data1$Signed_In==1,]
data1.notsignedin<-data1[data1$Signed_In==0,]
ggplot(subset(data1.signedin, Clicks>0), aes(x=Clicks/Impressions,colour=agecat))+geom_density()
ggplot(subset(data1.notsignedin, Clicks>0), aes(x=Clicks/Impressions,colour=agecat))+geom_density()
# create categories
data1$scode[data1$Impressions==0] <- "NoImps"
data1$scode[data1$Impressions >0] <- "Imps"
data1$scode[data1$Clicks >0] <- "Clicks"
# Convert the column to a factor
data1$scode <- factor(data1$scode)
head(data1)
#look at levels
clen <- function(x){c(length(x))}
etable<-summaryBy(Impressions~scode+Gender+agecat,data = data1, FUN=clen)
summary(etable)