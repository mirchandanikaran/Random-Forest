# Case- Random forest

setwd("D:/Datasets") 
getwd()

# Step 1: Data 

mydata = read.csv("Cardiotocographic.csv", header=T)
head(mydata)
str(mydata)
mydata$NSP = as.factor(mydata$NSP)
str(mydata)
head(mydata)
names(mydata)
# quit()

###################################################
#step 2: Data Partition
# Divide the data into two parts: Train data 
# and test data
# pd is partition data

set.seed(1234)
pd = sample(2, nrow(mydata), replace=TRUE, 
           prob=c(0.7, 0.3))
pd
train = mydata [pd ==1,]
test = mydata [pd ==2,]
length(train$NSP)
length(test$NSP)

##################################################
# step 3: Random Forest

library(randomForest)

set.seed(222)
rf = randomForest(NSP ~ ., data=train)
print(rf)

rf$call
rf$mtry
rf$type
rf$predicted
table(rf$predicted)
rf$err.rate
rf$confusion

rf$importance # Provides the most imp variable
plot(rf)
plot(rf$importance)
rf$ntree
rf$forest
rf$y # Classification of each category
rf$terms
###################################################


# step 4: Prediction and confusion matrix
library(caret)

p1= predict (rf, train)
head(p1)
head (train$NSP)

tail(p1)
tail(train$NSP)

confusionMatrix(p1, train$NSP)

# Step 5: Prediction with test data

p2= predict(rf, test)
confusionMatrix (p2, test$NSP)


# Step 6

plot(rf)
?plot(rf)


# Step 7
names(train)
t = tuneRF(train[,-22], train[,22],
          stepFactor = 0.5,
          plot=TRUE,
          ntreeTry =300, #no. of trees
          trace= TRUE,
          improve = 0.05)

rf1 = randomForest(NSP ~ ., data=train,
                  ntree= 300,
                  mtry=8, # how many variables we are considering
                  importance=TRUE
                  )
rf
rf1
plot(rf1)


hist(treesize(rf1),
     main="No of Nodes for the Trees",
     col = rainbow(7))
#####################################################
# Variable Importance

varImpPlot(rf1)

varImpPlot(rf1,
           sort = T,
           n.var=5,
           main="Top 5 important variables")

importance(rf1)
varUsed(rf)
names(mydata)

####################################################
# Partial Dependence plot
partialPlot(rf1, train, ASTV, "1") # for 1st category
partialPlot(rf1, train, ASTV, "2") # for 2nd category
partialPlot(rf1, train, ASTV, "3") # for 3rd category

partialPlot(rf1, train, ALTV, "1")
partialPlot(rf1, train, ALTV, "2")
partialPlot(rf1, train, ALTV, "3")

# Extract Single Tree
getTree(rf, 1, labelVar = TRUE)

# Multi Dimensional Scaling Plot of Proximity Matrix
MDSplot (rf, train$NSP)

