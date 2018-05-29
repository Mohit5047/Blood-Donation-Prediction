rm(list = ls())
library(caret)
#log loss function
logloss = function(a, p)
{
  return(-1/length(a)*(sum((a*log(p)+(1-a)*log(1-p)))))
}
#Reading the training and test data file
total_data = read.csv('/Users/mohit/Documents/Sem 2/ML/Project/transfusion.data')
test_data = read.csv('/Users/mohit/Documents/Sem 2/ML/Project/test.csv')
#transfer data to frame
colnames(total_data) = c('msld', 'nd', 'vd', 'msfd', 'y')
total_data$y = as.factor(total_data$y)
total_data$vd = NULL
test_data.sr_no = test_data$X
test_data$X = NULL
colnames(test_data) = c('msld', 'nd', 'vd', 'msfd','y')
#Pre-Processing
total_data$r = sqrt(total_data$msfd)
test_data$r = sqrt(test_data$msfd)
#splitting into train and validation
target = total_data$y; total_data$y = NULL;
id = sample(1:nrow(total_data), nrow(total_data)*0.7)
train = total_data[id, ]
train.y = target[id]
valid = total_data[-id, ]
valid.y = target[-id]
#Algorithm
fit.glm = glm(train.y ~ ., data = train, family = binomial(link = 'logit'))
quasibinomial(link = "logit")
#validation
valid.pred = predict(fit.glm, valid, type = 'response')
confusionMatrix(round(valid.pred, 0), valid.y)
logloss((as.numeric(valid.y)-1), valid.pred)
print(fit.glm)
plot(fit.glm)
#Write to file for submission
test_data.pred = predict(fit.glm, test_data, type = 'response')
test_data = read.csv('/Users/mohit/Documents/Sem 2/ML/Project/test.csv')
submit = data.frame(test_data$X, test_data.pred)
colnames(submit) = c('', 'Made Donation in March 2007')
write.csv(submit, '/Users/mohit/Documents/Sem 2/ML/Project/blood_submit.csv', row.names = F)