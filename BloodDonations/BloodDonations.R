setwd("~/Data Science/DataDriven/BloodDonations")
Train = read.csv("train.csv", stringsAsFactors=FALSE)
Test =  read.csv("test.csv", stringsAsFactors=FALSE)

str(Train)

Train$Average.Donation.Period =  (Train$Months.since.First.Donation / Train$Number.of.Donations) 
Train$First.Last.ratio = (Train$Months.since.First.Donation / Train$Months.since.Last.Donation) 

 
Test$Average.Donation.Period =  (Test$Months.since.First.Donation / Test$Number.of.Donations) 
Test$First.Last.ratio = (Test$Months.since.First.Donation / Test$Months.since.Last.Donation) 

is.na(Train$First.Last.ratio) <- do.call(cbind,lapply(Train$First.Last.ratio, is.infinite))
is.na(Test$First.Last.ratio) <- do.call(cbind,lapply(Test$First.Last.ratio, is.infinite))

#Train$Last.First.ratio = NULL 

str(Train)
str(Test)

#Simple multivarate Log regresstion
glmfit = glm(Made.Donation.in.March.2007 ~. - X - Total.Volume.Donated..c.c.., data=Train, family = binomial)
PredTestglm = predict(glmfit, newdata=Test, type="response")


# Regression Trees 
install.packages("rpart")
library(rpart)

Tree = rpart(Made.Donation.in.March.2007 ~. - X - Total.Volume.Donated..c.c.., data = Train, method="class", minbucket=25)


#Random Forest 
install.packages("randomForest")
library(randomForest)

# Build random forest model
Forest = randomForest(Made.Donation.in.March.2007 ~. - X - Total.Volume.Donated..c.c.., data = Train, ntree=200, nodesize=25 )

PredictForest = predict(Forest, newdata = Test)
table(Test$Made.Donation.in.March.2007, PredictForest)

#(40+74)/(40+37+19+74)

#regression trees boosting gbm 
install.packages("gbm")
library(gbm)
gbmfit = gbm(Made.Donation.in.March.2007 ~. - X - Total.Volume.Donated..c.c.., data=Train, n.trees=500,interaction.depth=2, distribution="bernoulli",shrinkage=0.005,cv.folds = 5 )
PredTest = predict(glmfit, newdata=Test, n.trees=500, type="response")


#XGBoost
install.packages("RTools")
library(RTools)
install.packages("devtools")
devtools::install_github('dmlc/xgboost', subdir='R-package')
install.packages('xgboost')
require(xgboost)

param = list(subsample = .5)
watchlist = list(logloss)
dTrain = xgb.DMatrix(Train)
xgbfit = xgb.train(Made.Donation.in.March.2007 ~. - X - Total.Volume.Donated..c.c.., param,  Dtrain , objective = "binary:logistic" , watchlist, verbose=1)
PredTestglm = predict(glmfit, newdata=Test, type="response")


# Submission File
my_predictions = data.frame( Test$X, "Made Donation in March 2007" = PredTest)

submission_format = read.csv("BloodDonationSubmissionFormat.csv", check.names=FALSE)

colnames(my_predictions) = colnames(submission_format)

write.csv(my_predictions, "my_predictions_5glm.csv", row.names=FALSE)