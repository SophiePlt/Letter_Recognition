
library(dplyr)
library(ggplot2)
library(caTools) # splits
library(rpart) # CART
library(rpart.plot) # CART plotting
library(GGally)
library(ROCR)
library(MASS)
library(caret)
library(randomForest)
library(gbm)


Letters <- read.csv("Letters.csv")
Letters$isB = as.factor(Letters$letter=="B")
set.seed(456)
train.ids = sample(nrow(Letters), 0.65*nrow(Letters))
Letters.train = Letters[train.ids,]
Letters.test = Letters[-train.ids,]

##################### Question A  ##########

#Baseline accuracy
table(Letters$isB)
2350/(2350+766)

###Logistic regression model

logregmod <- glm(isB ~ . -letter, data=Letters.train, family="binomial")
summary(logregmod)

#We predict whether the letter is "B" or not
predTest = predict(logregmod, newdata=Letters.test, type="response")
summary(predTest)

#We use the threshold p=0.5 and we obtain a confusion matrix
table(Letters.test$isB, predTest > 0.5) 
#We calculate the accuracy using the values of the confusion matrix
(791+238)/(791+27+35+238)
# We plot ROC curve and calculate AUC for our logistic regression model
rocr.pred <- prediction(predTest, Letters.test$isB)
logPerformance <- performance(rocr.pred, "tpr", "fpr")
plot(logPerformance, colorize = TRUE)
abline(0, 1)
as.numeric(performance(rocr.pred, "auc")@y.values)

AUROC<- performance(rocr.pred, 'auc')
AUROC <- AUROC@y.values
AUROC 

###CART model

# method = specify classification method, "rpart" for CART
# tuneGrid = gives the sequence of parameters to try, 
#             in this case, we try cp = 0 through cp=0.1 in increments of .002
# trControl = here using 10-fold cross validation
# metric = "Accuracy" for classification accuracy, "RMSE" or "Rsquared" or for regression


set.seed(456)
train.cart <- train(isB ~.-letter,
                    data = Letters.train,
                    method = "rpart",
                    tuneGrid = data.frame(cp = seq(0, .04, by=.002)),
                    trControl = trainControl(method = "cv", number=10),
                    metric = "Accuracy")

# look at the cross validation results, stored as a data-frame
train.cart$results 
train.cart

# plot the results
ggplot(train.cart$results, aes(x=cp, y=Accuracy)) + geom_point(size=3) +
  xlab("Complexity Parameter (cp)") + geom_line()

# Extract the best model and make predictions
train.cart$bestTune
mod.cart = train.cart$finalModel
prp(mod.cart, digits=3)

#we predict on the test set
pred.cart = predict(mod.cart, newdata=Letters.test, type="class")
table(Letters.test$isB, pred.cart)


###Random Forests

set.seed(456)
mod.rf <- randomForest(isB ~ .-letter, data = Letters.train, mtry = 5, nodesize = 5, ntree = 500)
summary(mod.rf)

pred.rf <- predict(mod.rf, newdata = Letters.test) 
table(Letters.test$isB, pred.rf)
importance(mod.rf)


#################Question B #############

#Baseline accuracy of the original dataset
table(Letters$letter)
(803)/(789+766+803+758)

##LDA model 
LdaModel <- lda(letter ~.-isB, data=Letters.train)
summary(LdaModel)
predTestLDA <- predict(LdaModel, newdata=Letters.test) 
table(Letters.test$letter, predTestLDA$class)
(249+235+272+247)/nrow(Letters.test)

### CART model

set.seed(456)
train.cart <- train(letter ~.-isB,
                    data = Letters.train,
                    method = "rpart",
                    tuneGrid = data.frame(cp = seq(0, .03, by=.002)),
                    trControl = trainControl(method = "cv", number=10),
                    metric = "Accuracy")

# look at the cross validation results, stored as a data-frame
train.cart$results 
train.cart

# plot the results
ggplot(train.cart$results, aes(x=cp, y=Accuracy)) + geom_point(size=3) +
  xlab("Complexity Parameter (cp)") + geom_line()

# Extract the best model and make predictions
train.cart$bestTune
mod.cart = train.cart$finalModel
prp(mod.cart, digits=3)

#we predict on the test set
pred.cart = predict(mod.cart, newdata=Letters.test, type="class")
table(Letters.test$letter, pred.cart)
(256+242+268+238)/nrow(Letters.test)


### Bagging : we use random forest when m=p
set.seed(456)
mod.bagging <- randomForest(letter~ .-isB, data = Letters.train, mtry = 18, nodesize = 5, ntree = 500)
summary(mod.bagging)

pred.bagging <- predict(mod.bagging, newdata = Letters.test) 
table(Letters.test$letter, pred.bagging)
importance(mod.bagging)

(260+259+278+257)/nrow(Letters.test)


###Random forest using cross validation to select mtry

train.rf <- train(letter ~.-isB,
                  data = Letters.train,
                  method = "rf",
                  tuneGrid = data.frame(mtry=1:17),
                  trControl = trainControl(method="cv", number=5, verboseIter = TRUE),
                  metric = "Accuracy")

train.rf$results
train.rf
best.rf <- train.rf$finalModel
pred.best.rf <- predict(best.rf, newdata = Letters.test) 

ggplot(train.rf$results, aes(x = mtry, y = Accuracy)) + geom_point(size = 3) + 
  ylab("Accuracy") + theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18))

table(Letters.test$letter, pred.best.rf)
importance(mod.pred.best.rf)
(263+262+280+263)/nrow(Letters.test)

### Boosting model


mod.boost <- gbm(letter ~ .-isB,
                 data = Letters.train,
                 distribution = "multinomial",
                 n.trees = 3300,
                 shrinkage = 0.1,
                 interaction.depth = 10)
summary(mod.boost)

pred.boost <- predict(mod.boost, newdata = Letters.test, n.trees=3300, type="response")
pred = apply(pred.boost, 1, which.max)

pred = factor(pred, levels = c(1,2,3,4), labels = c("A", "B", "P", "R"))

table(Letters.test$letter, pred)
(262+261+282+266)/nrow(Letters.test) 
