df <- read.csv('bank_note_data.csv')
head(df)

any(is.na(df))

library(caTools)
set.seed(101)

split <- sample.split(df$Class, SplitRatio = 0.7)

train <- subset(df, split == T)
test <- subset(df, split == F)

library(neuralnet)

nn <- neuralnet(Class ~. - Class, data = train, hidden = c(5,3), linear.output=F)

predicted.nn.values <- compute(nn, test[1:4])

head(predicted.nn.values$net.result)

predictions <- sapply(predicted.nn.values$net.result, round)
head(predictions)

table(predictions, test$Class)

library(randomForest)

df$Class <- factor(df$Class)
set.seed(101)

split <- sample.split(df$Class, SplitRatio = 0.7)

train <- subset(df, split == T)
test <- subset(df, split == F)

rf.model <- randomForest(Class ~., data=train)
rf.pred <- predict(rf.model, test)
table(rf.pred, test$Class)


