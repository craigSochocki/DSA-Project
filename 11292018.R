# Load data

naStrings = c("NA","na","","0"," ","N/A","n/a","n.a")
data = read.delim("~/Documents/DSA 6000/DSA Project/orange_small_train.data")
numInputFeatures = ncol(data)
numSamples = nrow(data)

# Load labels
appetency = read.table("~/Documents/DSA 6000/DSA Project/orange_small_train_appetency.labels.txt")
churn = read.table("~/Documents/DSA 6000/DSA Project/orange_small_train_churn.labels.txt")
upsell = read.table("~/Documents/DSA 6000/DSA Project/orange_small_train_upselling.labels.txt")

featuresList = names(data)

# Attach response to predictors
data$appetency = appetency
data$churn = churn
data$upsell = upsell

# Divide training set into training, test, and validation sets

set.seed(2018)
trainingIndices = sample.int(numSamples, size = 0.8 * numSamples, replace = FALSE)

train = data[trainingIndices,]
test = data[-trainingIndices,]

validIndices = sample.int(length(train), size = 0.25 * length(train), replace = FALSE)
valid = train[validIndices,]
train = train[-validIndices,]

numTrain = nrow(train)
numValid = nrow(valid)
numTest = nrow(test)

