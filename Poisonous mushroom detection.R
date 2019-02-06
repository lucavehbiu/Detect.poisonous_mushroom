require(tidyverse)
require(ggplot2)
require(caret)
require(modelr)
require(parallel)

setwd("/Users/Luca/Desktop/")
data <- read.csv("data_train2.csv", sep = ",", header = T)
DF <- data


#convert everything into factors
DF[sapply(DF, is.character)] <- lapply(DF[sapply(DF, is.character)], 
                                       as.factor)

#remove the missing values variables
DF <- DF %>% select(-c(veil.type, X, odor, stalk.root))


#train model
set.seed(1616)
trctrl <- trainControl(method = "repeatedcv", number = 2)
a <- createDataPartition(DF$class, list = F, p = 0.74)
train <- DF[a,]
test <- DF[-a , ]


#random forest
rf <- train(class ~., data = train %>% select(population, class, 
                                              gill.color,
                                              cap.color, 
                                              stalk.color.below.ring, stalk.color.above.ring,
                                              stalk.shape, gill.size, gill.spacing, cap.shape), 
            method = "rf",
            trControl = trctrl,
            allowParallel = T) #20 seconds runtime




pred <- predict(rf, newdata = test )
confusionMatrix(pred, test$class)

#Add the predictions from random forest to the train set
train <- add_predictions(rf, train)



#logistic regression
model <- glm(pred ~. , data = train %>% select(pred, 
                                                gill.color, stalk.color.above.ring, 
                                                stalk.color.below.ring, 
                                                population, gill.size, ring.number
                                                ), family ="binomial")

#express predictions as probabilities
pred_glm <- plogis(predict(model, newdata = test))
test$pred.prob <- pred_glm

#minimize the chances to get edibles that are posionous
test$pred_glm <- ifelse(test$pred.prob > 0.95, "p", "e") %>% as.factor()

#results
confusionMatrix(test$class, test$pred_glm)




















