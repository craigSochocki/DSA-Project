# Part 1 - Load data
naStrings = c("NA","na",
            "","0"," ","N/A","n/a","n.a")
data = read.delim("~/Desktop/orange_small_train.data", na.strings=naStrings)
numInputFeatures = ncol(data)
numSamples = nrow(data)

# Load labels

appetency = read.table("~/Documents/DSA 6000/DSA Project/orange_small_train_appetency.labels.txt")
churn = read.table("~/Documents/DSA 6000/DSA Project/orange_small_train_churn.labels.txt")
upsell = read.table("~/Documents/DSA 6000/DSA Project/orange_small_train_upselling.labels.txt")

# Convert lavel data from integers into repsonse variable factors
appetency = as.factor(appetency[,])
churn = as.factor(churn[,])
upsell = as.factor(upsell[,])

responseVars = c('appetency','churn','upsell')
featuresList = names(data)
featureClasses = sapply(data,class)
# Attach response to predictors
data$appetency = appetency
data$churn = churn
data$upsell = upsell


data$sumNA = apply(data, 1, function(row) sum(is.na(row)))


data

# Complete data set has been loaded in and data frame constructed

#---------

# Part 2 - Divide data set into training, test, and validation sets

set.seed(2018)
trainingIndices = sample.int(numSamples, size = 0.8 * numSamples, replace = FALSE)

train = data[trainingIndices,]
test = data[-trainingIndices,]


validIndices = sample.int(nrow(train), size = 0.25 * nrow(train), replace = FALSE)
valid = train[validIndices,]
train = train[-validIndices,]

numTrain = nrow(train)
numValid = nrow(valid)
numTest = nrow(test)

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

library(data.table)
length(numFeaturesMeans)
head(train[,possNumFeatures])
train = as.data.table(train)

for (cols in possNumFeatures) {
  x = train[[cols]]
  isNa = is.na(x)
  if (sum(isNa) > 0) {
    train[, cols := as.numeric(x),with=FALSE]
    mu = numFeaturesMeans[cols]
    train[isNa, cols := mu,with=FALSE]
  }
}

# ---- 
# Clean Categorical Data

possCatFeatures = setdiff(possCatFeatures,responseVars)

for (cat in possCatFeatures) {
  for(level in levels(cat))
    if (condition) {
      
    }
}
