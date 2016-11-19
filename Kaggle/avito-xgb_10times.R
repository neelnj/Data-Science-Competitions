
# Required Libraries
library(data.table)
library(readr)
library(caret)
library(stringdist)

# Read in data
location <- fread("Location.csv")
itemPairsTest <- fread("ItemPairs_test.csv")
itemPairsTrain <- fread("ItemPairs_train.csv")
itemInfoTest <- read_csv("ItemInfo_test.csv")
itemInfoTrain <- read_csv("ItemInfo_train.csv")
itemInfoTest <- data.table(itemInfoTest)
itemInfoTrain <- data.table(itemInfoTrain)

setkey(location, locationID)
setkey(itemInfoTrain, itemID)
setkey(itemInfoTest, itemID)

# Drop unused factors
dropAndNumChar <- function(itemInfo){
  itemInfo[, ':=' (ncharTitle = nchar(title),
                   ncharDescription = nchar(description),
                   description = NULL,
                   images_array = NULL,
                   attrsJSON = NULL)]
}

dropAndNumChar(itemInfoTest)
dropAndNumChar(itemInfoTrain)


# Merge
mergeInfo <- function(itemPairs, itemInfo){
  # merge on itemID_1
  setkey(itemPairs, itemID_1)
  itemPairs <- itemInfo[itemPairs]
  setnames(itemPairs, names(itemInfo), paste0(names(itemInfo), "_1"))
  # merge on itemID_2
  setkey(itemPairs, itemID_2)
  itemPairs <- itemInfo[itemPairs]
  setnames(itemPairs, names(itemInfo), paste0(names(itemInfo), "_2"))
  # merge on locationID_1
  setkey(itemPairs, locationID_1)
  itemPairs <- location[itemPairs]
  setnames(itemPairs, names(location), paste0(names(location), "_1"))
  # merge on locationID_2
  setkey(itemPairs, locationID_2)
  itemPairs <- location[itemPairs]
  setnames(itemPairs, names(location), paste0(names(location), "_2"))
  return(itemPairs)
}

itemPairsTrain <- mergeInfo(itemPairsTrain, itemInfoTrain)
itemPairsTest <- mergeInfo(itemPairsTest, itemInfoTest)

rm(list=c("itemInfoTest", "itemInfoTrain", "location"))

for( j in 1:ncol(itemPairsTrain) ) {
  
  itemPairsTrain[is.na(itemPairsTrain[,j]), j] <- median(itemPairsTrain[,j], na.rm = TRUE)
}

for( j in 1:ncol(itemPairsTest) ) {
  
  itemPairsTest[is.na(itemPairsTest[,j]), j] <- median(itemPairsTest[,j], na.rm = TRUE)
}

# Create features
matchPair <- function(x, y){
  ifelse(is.na(x), ifelse(is.na(y), 3, 2), ifelse(is.na(y), 2, ifelse(x==y, 1, 4)))
}

createFeatures <- function(itemPairs){
  itemPairs[, ':=' (locationMatch = matchPair(locationID_1, locationID_2),
                    locationID_1 = NULL,
                    locationID_2 = NULL,
                    regionMatch = matchPair(regionID_1, regionID_2),
                    regionID_1 = NULL,
                    regionID_2 = NULL,
                    metroMatch = matchPair(metroID_1, metroID_2),
                    metroID_1 = NULL,
                    metroID_2 = NULL,
                    categoryID_1 = NULL,
                    categoryID_2 = NULL,
                    priceMatch = matchPair(price_1, price_2),
                    priceDiff = pmax(price_1/price_2, price_2/price_1),
                    priceMin = pmin(price_1, price_2, na.rm=TRUE),
                    priceMax = pmax(price_1, price_2, na.rm=TRUE),
                    price_1 = NULL,
                    price_2 = NULL,
                    titleStringDist = stringdist(title_1, title_2, method = "jw"),
                    titleStringDist2 = (stringdist(title_1, title_2, method = "lcs") / 
                        pmax(ncharTitle_1, ncharTitle_2, na.rm=TRUE)),
                    #titleStringDist3 = (stringdist(title_1, title_2, method = "osa") / 
                    #                      pmax(ncharTitle_1, ncharTitle_2, na.rm=TRUE)),
                    #titleStringDist4 = (stringdist(title_1, title_2, method = "lv") / 
                    #                      pmax(ncharTitle_1, ncharTitle_2, na.rm=TRUE)),
                    #titleStringDist5 = (stringdist(title_1, title_2, method = "dl") / 
                    #                      pmax(ncharTitle_1, ncharTitle_2, na.rm=TRUE)),
                    #titleStringDist6 = (stringdist(title_1, title_2, method = "hamming") / 
                    #                      pmax(ncharTitle_1, ncharTitle_2, na.rm=TRUE)),
                    #titleStringDist7 = (stringdist(title_1, title_2, method = "qgram") / 
                    #                      pmax(ncharTitle_1, ncharTitle_2, na.rm=TRUE)),
                    titleStringDist8 = (stringdist(title_1, title_2, method = "cosine") / 
                                          pmax(ncharTitle_1, ncharTitle_2, na.rm=TRUE)),
                    titleStringDist9 = (stringdist(title_1, title_2, method = "jaccard") / 
                                         pmax(ncharTitle_1, ncharTitle_2, na.rm=TRUE)),
                    #titleStringDist10 = (stringdist(title_1, title_2, method = "soundex") / 
                          #                pmax(ncharTitle_1, ncharTitle_2, na.rm=TRUE)),
                    title_1 = NULL,
                    title_2 = NULL,
                    titleCharDiff = pmax(ncharTitle_1/ncharTitle_2, ncharTitle_2/ncharTitle_1),
                    titleCharMin = pmin(ncharTitle_1, ncharTitle_2, na.rm=TRUE),
                    titleCharMax = pmax(ncharTitle_1, ncharTitle_2, na.rm=TRUE),
                    ncharTitle_1 = NULL,
                    ncharTitle_2 = NULL,
                    descriptionCharDiff = pmax(ncharDescription_1/ncharDescription_2, ncharDescription_2/ncharDescription_1),
                    descriptionCharMin = pmin(ncharDescription_1, ncharDescription_2, na.rm=TRUE),
                    descriptionCharMax = pmax(ncharDescription_1, ncharDescription_2, na.rm=TRUE),
                    ncharDescription_1 = NULL,
                    ncharDescription_2 = NULL,
                    distance = sqrt((lat_1-lat_2)^2+(lon_1-lon_2)^2),
                    lat_1 = NULL,
                    lat_2 = NULL,
                    lon_1 = NULL,
                    lon_2 = NULL,
                    itemID_1 = NULL,
                    itemID_2 = NULL,
                    allmatch= (matchPair(locationID_1, locationID_2)+matchPair(regionID_1, regionID_2)+ matchPair(metroID_1, metroID_2) ),
                    absprice=abs(price_1 - price_2)
                    )]
  
  itemPairs[, ':=' (priceDiff = ifelse(is.na(priceDiff), 0, priceDiff),
                    priceMin = ifelse(is.na(priceMin), 0, priceMin),
                    priceMax = ifelse(is.na(priceMax), 0, priceMax),
                    absprice = ifelse(is.na(absprice),0,absprice),
                    titleStringDist = ifelse(is.na(titleStringDist), 0, titleStringDist),
                    titleStringDist2 = ifelse(is.na(titleStringDist2) | titleStringDist2 == Inf, 0, titleStringDist2),
                    #titleStringDist3 = ifelse(is.na(titleStringDist3) | titleStringDist3 == Inf, 0, titleStringDist3),
                    #titleStringDist4 = ifelse(is.na(titleStringDist4) | titleStringDist4 == Inf, 0, titleStringDist4),
                    #titleStringDist5 = ifelse(is.na(titleStringDist5) | titleStringDist5 == Inf, 0, titleStringDist5),
                    #titleStringDist6 = ifelse(is.na(titleStringDist6) | titleStringDist6 == Inf, 0, titleStringDist6),
                   # titleStringDist7 = ifelse(is.na(titleStringDist7) | titleStringDist7 == Inf, 0, titleStringDist7),
                    titleStringDist8 = ifelse(is.na(titleStringDist8) | titleStringDist8 == Inf, 0, titleStringDist8),
                    titleStringDist9 = ifelse(is.na(titleStringDist9) | titleStringDist9 == Inf, 0, titleStringDist9)
                    #titleStringDist10 = ifelse(is.na(titleStringDist10) | titleStringDist10 == Inf, 0, titleStringDist10)
                    )]
}

createFeatures(itemPairsTest)
createFeatures(itemPairsTrain)

S1=which(itemPairsTrain$isDuplicate==1)
A=sample(S1,100000,replace=TRUE)
B=itemPairsTrain[A,]
itemPairsTrain=rbind(itemPairsTrain,B)

library(xgboost)

maxTrees <- 100
shrinkage <- 0.09
gamma <- 2
depth <- 2
minChildWeight <- 45
colSample <- 0.4
subSample <- 0.37
earlyStopRound <- 4

modelVars <- names(itemPairsTrain)[which(!(names(itemPairsTrain) %in% c("isDuplicate", "generationMethod", "foldId")))]

itemPairsTest <- data.frame(itemPairsTest)
itemPairsTrain <- data.frame(itemPairsTrain)
set.seed(1984)
#cat(dim(itemPairsTrain))
#itemPairsTrain <- itemPairsTrain[sample(nrow(itemPairsTrain), 500000), ]

# Matrix
dtrain <- xgb.DMatrix(as.matrix(itemPairsTrain[, modelVars]), label=itemPairsTrain$isDuplicate,missing=NaN)
dtest <- xgb.DMatrix(as.matrix(itemPairsTest[, modelVars]),missing=NaN)

# xgboost cross-validated

#xgbCV <- xgb.cv(params=list(max_depth=depth,
#                            eta=shrinkage,
#                            gamma=gamma,
#                            colsample_bytree=colSample,
#                            min_child_weight=minChildWeight,
#                            subsample=subSample,
#                            objective="binary:logistic"),
#                data=dtrain,
#                nrounds=maxTrees,
#                eval_metric ="auc",
#                nfold=3,
#                stratified=TRUE,
#                early.stop.round=earlyStopRound)

#numTrees <- min(which(xgbCV$test.auc.mean==max(xgbCV$test.auc.mean)))

tt=4
totaltest=vector("numeric",length=nrow(itemPairsTest))
for (ii in 1:tt)
{
  print(ii)
  set.seed(2016+ii)
xgbResult <- xgboost(params=list(max_depth=depth,
                                 eta=shrinkage,
                                 gamma=gamma,
                                 colsample_bytree=colSample,
                                 min_child_weight=minChildWeight),
                     data=dtrain,
                     nrounds=1.1*maxTrees,
                     objective="binary:logistic",
                     eval_metric="auc")

testPreds <- predict(xgbResult, dtest)
totaltest=totaltest+testPreds
}
finaltest=totaltest/tt

submission <- data.frame(id=itemPairsTest$id, probability=finaltest)
write.csv(submission, file="submission.csv",row.names=FALSE)
