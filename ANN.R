library(caret)
library(pROC)
#Path of the dataset
train_file = read.csv("C:/Users/ankit/Desktop/UTD/ML/project/train.csv",header=TRUE, stringsAsFactors=FALSE, sep=",")
test_file = read.csv("C:/Users/ankit/Desktop/UTD/ML/project/test.csv",header=TRUE, stringsAsFactors=FALSE, sep=",")
#Loading the training and test data and then Pre-Processing
train_frame = data.frame(train_file)
train_frame =subset(train_frame, select=-c(1))
colnames(train_frame) = c("msld", "nd","tvd","msfd","label")
train_frame$msfd=sqrt(train_frame$msfd-train_frame$msld)
train_frame$label = factor(ifelse(train_frame$label==0, "Zero", "One"))
test_frame = data.frame(test_file)
test_frame =subset(test_frame, select=-c(1))
colnames(test_frame) = c("msld", "nd","tvd","msfd")
test_frame$msfd=sqrt(test_frame$msfd-test_frame$msld)
#estimator Function to find the probablity
probablity_estimator = function(data,lev = NULL, model = NULL)
{
  confusion_matrix = confusionMatrix(data$pred,data$obs)
  return(c(confusion_matrix$overall[1],confusion_matrix$byClass[c(1,5,6,7)]))
}
#training
training= trainControl(method="cv", number=50, classProbs = TRUE ,summaryFunction =probablity_estimator)
#Algorithm with different attributes
ann = expand.grid(nhid = c(2,3,4,5,6,7) ,actfun=c("purelin","sin","tansig","radbas"))
ann_model= train(label~ ., data=train_frame, trControl=training, method="elm",tuneGrid=ann , metric="Accuracy")
print(ann_model)
plot(ann_model)
#creating file for submissions
output = predict(object=ann_model, test_frame, type='prob')
head(output$One)
submit=data.frame(test_frame$X,output$One)
colnames(submit) = c('', 'Made Donation in March 2007')
write.csv(submit, 'submit.csv', row.names = F)

