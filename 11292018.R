# Part 1 - Load data
naStrings = c("NA","na",
            "","0"," ","N/A","n/a","n.a")
data = read.delim("~/Desktop/orange_small_train.data", na.strings=naStrings)
numInputFeatures = ncol(data)
numSamples = nrow(data)

# Load labels

#appetency = read.table("~/Documents/DSA 6000/DSA Project/orange_small_train_appetency.labels.txt")
churn = read.table("~/Documents/DSA 6000/DSA Project/orange_small_train_churn.labels.txt")
#upsell = read.table("~/Documents/DSA 6000/DSA Project/orange_small_train_upselling.labels.txt")

# Convert lavel data from integers into repsonse variable factors
#appetency = as.factor(appetency[,])
#churn = as.factor(churn[,])
#upsell = as.factor(upsell[,])

responseVars = c('appetency','churn','upsell')
featuresList = names(data)
featureClasses = sapply(data,class)
# Attach response to predictors
# data$appetency = appetency
# data$churn = churn
# data$upsell = upsell


data$sumNA = apply(data, 1, function(row) sum(is.na(row)))


#data

# Complete data set has been loaded in and data frame constructed

#---------

# Part 2 - Divide data set into training, test, and validation sets

set.seed(2018)
trainingIndices = sample.int(numSamples, size = 0.8 * numSamples, replace = FALSE)

train = data[trainingIndices,]
test = data[-trainingIndices,]

churnTrain = churn[trainingIndices,]
churnTest = churn[-trainingIndices,]

head(churnTrain)

validIndices = sample.int(trainingIndices, size = 0.25 * nrow(train), replace = FALSE)
valid = train[validIndices,]
train = train[-validIndices,]

churnValid = churnTrain[validIndices]
churnTrain = churnTrain[-validIndices]

numTrain = nrow(train)
numValid = nrow(valid)
numTest = nrow(test)

numChurnTrain = length(churnTrain)
numChurnTrain

# -----

# Part 3 - Data Cleaning

## Remove columns with excess NAs - to remove noise and improve accurancy of missing data imputation later on

featureNAs = sapply(train, function(col) sum(is.na(col)))
featureNAprop = featureNAs / numTrain

train = train[,featureNAprop < .2]

# We now have a complete data set that has removed any columns that have too many 
# missing values.  We can move forward with imputing missing values based on
# avaiable data

# Missing Value Imputation

possibleFeaturesList = setdiff(names(train),responseVars)
featureClasses = sapply(train,class)
possNumFeatures = possibleFeaturesList[featureClasses != 'factor']
possCatFeatures = possibleFeaturesList[featureClasses == 'factor']

## Numeric Feature Imputation
# Throw away outliers (more than 3sig from mu)

possCatFeatures # need to remove labels
possNumFeatures

numFeaturesMeans = sapply(train[,possNumFeatures], function(col) mean(col, na.rm = TRUE) )
numFeaturesMeans
library(data.table)
length(numFeaturesMeans)
head(train[,possNumFeatures])
train = as.data.table(train)

for (cols in possNumFeatures) {
  x = train[[cols]]
  isNa = is.na(x)
  if (sum(isNa) > 0) {
    train[, (cols) := as.numeric(x),with=FALSE]
    mu = numFeaturesMeans[cols]
    train[isNa, (cols) := mu,with=FALSE]
  }
}
sum(is.na(train$Var6))
train = as.data.frame(train)
sum(is.na(train[,possNumFeatures]))

# ---- 
# Clean Categorical Data

possCatFeatures
train = as.data.frame(train)

train[,possCatFeatures]

levelsOfCats = sapply(train[,possCatFeatures], function(col) length(levels(col)))
levelsOfCats

tooManyCats = (levelsOfCats > 500) 
notEnoughCats = levelsOfCats < 2

possCatFeatures = possCatFeatures[!tooManyCats]

possCatFeatures = possCatFeatures[!notEnoughCats]

possCatFeatures
possCatFeatures = possCatFeatures[!is.na(possCatFeatures)]
possCatFeatures

str(train[,possCatFeatures])

train = train[,c(possNumFeatures, possCatFeatures)]
train
sum(is.na(train[,possCatFeatures]))

# ---- BUILDING NUMERIC MODELS

trainNum = train[,possNumFeatures]

# Find which features only have 1 category

trainNum$churn = as.factor(churnTrain)

head(trainNum)

lm1.fits = glm(churn ~ ., data=trainNum, family = 'binomial')
summary(lm1.fits)

corrs = cor(trainNum[,-15])

install.packages("corrplot")
library(corrplot)
corrplot(corrs, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

summary(trainNum$Var73)
hist(trainNum$Var73)
# As Var73 goes up, amount of missing data in original data set goes 
# down. Perhaps Var73 has something to do with days since last interaction?

lm1.probs = predict(lm1.fits, type="response")
lm1.pred = rep("Not Churn", numTrain)

lm1.pred[lm1.probs > 0.5] = "Churn"
lm1.pred

table(lm1.pred, trainNum$churn)

plot(trainNum$churn)

 # ---- LOG MODEL 2

lm2.fits = glm(trainNum$churn ~ Var65 + Var73 + Var81 + Var113 + sumNA, family = "binomial")
summary(lm2.fits)

lm2.probs = predict(lm2.fits,type="response")
lm2.probs

lm2.pred = rep("Not Churn", numTrain)
lm2.pred[lm2.probs > 0.25] = "Churn"

table(lm2.pred,trainNum$churn)

lm2TPR = 9/2210
lm2TPR

lm2TNR = 27774/27790
lm2TNR

# REGRESSION DECISION TREE
library(tree)

tree2 = tree(log(Churn)~.,data=trainNum)
trainNum = as.data.frame(trainNum)

tree.trainNum = tree(trainNum$churn~.,trainNum)
summary(tree.trainNum)

Churn = ifelse(churnTrain == '1',"Yes","No")

trainSet = data.frame(trainNum[,-15],Churn)
trainSet
tree.trainNum = tree(Churn~.,trainSet)
summary(tree.trainNum)

plot(tree.trainNum)
text(tree.trainNum,pretty=0)

tree.trainNum


# RANDOM FOREST

library(randomForest)

bag.trainNum = randomForest(Churn~.,mtry=14,data=trainSet)
bag.trainNum

# GRADIENT BOOSTING


install.packages("gbm")
library(gbm)

trainSet.boost = gbm(Churn~.,data=trainSet,distribution="gaussian",n.trees = 500,interaction.depth = 2,shrinkage=.01)
summary(trainSet.boost)

# n.trees = seq(from=100,to=500,by=5)
# predMatrix = predict(trainSet.boost,test,n.trees=n.trees)
# dim(predMatrix)
# predMatrix
# test.error<-with(trainSet[,],apply( (predMatrix - trainSet$Churn)^2,2,mean))
# head(test.error)

yhat.boost = predict(trainNum.boost,newdata=valid,n.trees=500)

# lm3 = glm(Churn~Var113+Var73+Var65+sumNA,data=trainSet,family='binomial')
# 
# summary(lm3)
# 
# lm3.probs = predict(lm3,type="response")
# lm3.probs
# lm3.pred = rep("No Churn", numTrain)
# lm3.pred[lm3.probs > 0.25] = "Churn"
# lm3.pred
# 
# table(lm3.pred,Churn)




