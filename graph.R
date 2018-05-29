file_test = read.csv("C:/Users/ankit/Desktop/UTD/ML/project/final/Blood-Donations-Prediction-master/data/test.csv",header=TRUE, stringsAsFactors=FALSE, sep=",")
test_df1 = data.frame(file_test)
e <- density(test_df1$Months.since.Last.Donation) 

files = read.csv("C:/Users/ankit/Desktop/UTD/ML/project/final/Blood-Donations-Prediction-master/data/train.csv",header=TRUE, stringsAsFactors=FALSE, sep=",")
dframe = data.frame(files)
dframe$Months.since.Last.Donation=sqrt(dframe$Months.since.Last.Donation)
d <- density(dframe$Months.since.Last.Donation)
plot(d)


library(lmtest)
library(lm.beta)
library(caTools)
library(car)
library(pscl)
library(ROCR)
names(file_test) <- c('ID','x1','x2','x3','x4','y')
attach(file_test)
cor(file_test[,-1])
par(mfrow = c(2,2))
plot(y~x1+x2+x3+x4,col='blue')
plot(y~jitter(x1)+jitter(x2)+jitter(x3)+jitter(x4),col='blue')




require(ggplot2)
require(reshape2)
df<-read.csv("C:/Users/ankit/Desktop/UTD/ML/project/final/Blood-Donations-Prediction-master/data/train.csv",header=TRUE)
df<-subset(df,select=-(X))
res <- cor(df)
round(res, 2)
library(corrplot)
corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)