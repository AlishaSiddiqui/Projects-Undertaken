library(tidyverse)
library(lubridate)
#`df` is `CPOD_Time` saved as `df<-as.data.frame(CPOD_Time)`

library('ggplot2')
library('ggthemes') 
library('scales')
library('dplyr') 
library('mice')
library('randomForest') 
library('data.table')
library('gridExtra')
library('corrplot') 
library('GGally')
library('e1071')
library('randomForest')



train <-read.csv('housing_train.csv', stringsAsFactors = F)
test  <-read.csv('housing_test.csv', stringsAsFactors = F)
test$SalePrice<-rep(NA,1459)
house<-bind_rows(train,test)
cat_var <- names(train)[which(sapply(train, is.character))]
cat_car <- c(cat_var, 'BedroomAbvGr', 'HalfBath', ' KitchenAbvGr','BsmtFullBath', 'BsmtHalfBath', 'MSSubClass')
numeric_var <- names(train)[which(sapply(train, is.numeric))]
train1_cat<-train[cat_var]
train1_num<-train[numeric_var]
correlations <- cor(na.omit(train1_num[,-1]))
row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
correlations<- correlations[row_indic ,row_indic ]
#corrplot(correlations, method="square")******
train <- train[train$GrLivArea<=4000,]
Missing_indices <- sapply(train,function(x) sum(is.na(x)))
Missing_Summary <- data.frame(index = names(train),Missing_Values=Missing_indices)
#Missing_Summary[Missing_Summary$Missing_Values > 0,]
test$SalePrice <- NA
train$isTrain <- 1
test$isTrain <- 0
house <- rbind(train,test)
house$MasVnrArea[which(is.na(house$MasVnrArea))] <- mean(house$MasVnrArea,na.rm=T)
## Alley
#Changing NA in Alley to None
house$Alley1 <- as.character(house$Alley)
house$Alley1[which(is.na(house$Alley))] <- "None"
#table(house$Alley1)
house$Alley <- as.factor(house$Alley1)
house <- subset(house,select = -Alley1)
## MasVnrType
#Changing NA in MasVnrType to None
house$MasVnrType1 <- as.character(house$MasVnrType)
house$MasVnrType1[which(is.na(house$MasVnrType))] <- "None"
house$MasVnrType <- as.factor(house$MasVnrType1)
house <- subset(house,select = -MasVnrType1)
#table(house$MasVnrType)


## LotFrontage

#Imputing missing Lot Frontage by the median


house$LotFrontage[which(is.na(house$LotFrontage))] <- median(house$LotFrontage,na.rm = T)


## FireplaceQu

#Changing NA in FireplaceQu to None


house$FireplaceQu1 <- as.character(house$FireplaceQu)
house$FireplaceQu1[which(is.na(house$FireplaceQu))] <- "None"
house$FireplaceQu <- as.factor(house$FireplaceQu1)
house <- subset(house,select = -FireplaceQu1)


## PoolQC

#Changing NA in PoolQC to None


house$PoolQC1 <- as.character(house$PoolQC)
house$PoolQC1[which(is.na(house$PoolQC))] <- "None"
house$PoolQC <- as.factor(house$PoolQC1)
house <- subset(house,select = -PoolQC1)


## Fence

#Changing NA in Fence to None


house$Fence1 <- as.character(house$Fence)
house$Fence1[which(is.na(house$Fence))] <- "None"
house$Fence <- as.factor(house$Fence1)
house <- subset(house,select = -Fence1)


## MiscFeature

#Changing NA in MiscFeature to None


house$MiscFeature1 <- as.character(house$MiscFeature)
house$MiscFeature1[which(is.na(house$MiscFeature))] <- "None"
house$MiscFeature <- as.factor(house$MiscFeature1)
house <- subset(house,select = -MiscFeature1)


## GarageType

#Changing NA in GarageType to None


house$GarageType1 <- as.character(house$GarageType)
house$GarageType1[which(is.na(house$GarageType))] <- "None"
house$GarageType <- as.factor(house$GarageType1)
house <- subset(house,select = -GarageType1)


## GarageYrBlt

#Changing NA in GarageYrBlt to None


house$GarageYrBlt[which(is.na(house$GarageYrBlt))] <- 0


## GarageFinish

#Changing NA in GarageFinish to None


house$GarageFinish1 <- as.character(house$GarageFinish)
house$GarageFinish1[which(is.na(house$GarageFinish))] <- "None"
house$GarageFinish <- as.factor(house$GarageFinish1)
house <- subset(house,select = -GarageFinish1)


## GarageQual

#Changing NA in GarageQual to None


house$GarageQual1 <- as.character(house$GarageQual)
house$GarageQual1[which(is.na(house$GarageQual))] <- "None"
house$GarageQual <- as.factor(house$GarageQual1)
house <- subset(house,select = -GarageQual1)


## GarageCond
#
#Changing NA in GarageCond to None


house$GarageCond1 <- as.character(house$GarageCond)
house$GarageCond1[which(is.na(house$GarageCond))] <- "None"
house$GarageCond <- as.factor(house$GarageCond1)
house <- subset(house,select = -GarageCond1)


## BsmtQual

#Changing NA in BsmtQual to None


house$BsmtQual1 <- as.character(house$BsmtQual)
house$BsmtQual1[which(is.na(house$BsmtQual))] <- "None"
house$BsmtQual <- as.factor(house$BsmtQual1)
house <- subset(house,select = -BsmtQual1)


## BsmtCond

#Changing NA in BsmtCond to None


house$BsmtCond1 <- as.character(house$BsmtCond)
house$BsmtCond1[which(is.na(house$BsmtCond))] <- "None"
house$BsmtCond <- as.factor(house$BsmtCond1)
house <- subset(house,select = -BsmtCond1)


## BsmtExposure

#Changing NA in BsmtExposure to None


house$BsmtExposure1 <- as.character(house$BsmtExposure)
house$BsmtExposure1[which(is.na(house$BsmtExposure))] <- "None"
house$BsmtExposure <- as.factor(house$BsmtExposure1)
house <- subset(house,select = -BsmtExposure1)


## BsmtFinType1

#Changing NA in BsmtFinType1 to None


house$BsmtFinType11 <- as.character(house$BsmtFinType1)
house$BsmtFinType11[which(is.na(house$BsmtFinType1))] <- "None"
house$BsmtFinType1 <- as.factor(house$BsmtFinType11)
house <- subset(house,select = -BsmtFinType11)


## BsmtFinType2

#Changing NA in BsmtFinType2 to None


house$BsmtFinType21 <- as.character(house$BsmtFinType2)
house$BsmtFinType21[which(is.na(house$BsmtFinType2))] <- "None"
house$BsmtFinType2 <- as.factor(house$BsmtFinType21)
house <- subset(house,select = -BsmtFinType21)


## Electrical

#Changing NA in Electrical to None


house$Electrical1 <- as.character(house$Electrical)
house$Electrical1[which(is.na(house$Electrical))] <- "None"
house$Electrical <- as.factor(house$Electrical1)
house <- subset(house,select = -Electrical1)


## Factorizing


house$MSZoning<- factor(house$MSZoning)
house$Street <- factor(house$Street)
house$LotShape <-factor(house$LotShape )
house$LandContour<-factor(house$LandContour)
house$Utilities<-factor(house$Utilities)
house$LotConfig<-factor(house$LotConfig)
house$LandSlope<-factor(house$LandSlope)
house$Neighborhood<-factor(house$Neighborhood)
house$Condition1<-factor(house$Condition1)
house$Condition2<-factor(house$Condition2)
house$BldgType<-factor(house$BldgType)
house$HouseStyle<-factor(house$HouseStyle)
house$RoofStyle<-factor(house$RoofStyle)
house$RoofMatl<-factor(house$RoofMatl)
house$Exterior1st<-factor(house$Exterior1st)
house$Exterior2nd<-factor(house$Exterior2nd)
house$ExterQual<-factor(house$ExterQual)
house$ExterCond<-factor(house$ExterCond)
house$Foundation<-factor(house$Foundation)
house$Heating<-factor(house$Heating)
house$HeatingQC<-factor(house$HeatingQC)
house$CentralAir<-factor(house$CentralAir)
house$KitchenQual<-factor(house$KitchenQual)
house$Functional<-factor(house$Functional)
house$PavedDrive<-factor(house$PavedDrive)
house$SaleType<-factor(house$SaleType)
house$SaleCondition<-factor(house$SaleCondition)
#str(house)


#Taking all the column classes in one variable so as to seperate factors from numerical variables.


Column_classes <- sapply(names(house),function(x){class(house[[x]])})
numeric_columns <-names(Column_classes[Column_classes != "factor"])


#Train and test dataset creation


train <- house[house$isTrain==1,]
test <- house[house$isTrain==0,]
smp_size <- floor(0.75 * nrow(train))

## setting the seed to make the partition reproducible

set.seed(123)
train_ind <- sample(seq_len(nrow(train)), size = smp_size)

train_new <- train[train_ind, ]
validate <- train[-train_ind, ]
train_new <- subset(train_new,select=-c(Id,isTrain))
validate <- subset(validate,select=-c(Id,isTrain))



## 7.....Build the model

#missmap(train_new)
library(randomForest)
house_model <- randomForest(SalePrice~.,
                            data = train_new)

## 9...Final Prediction
# Predict using the test set
prediction <- predict(house_model,test)
prediction[which(is.na(prediction))] <- mean(prediction,na.rm=T)

submit <- data.frame(Id=test$Id,SalePrice=prediction)


test_print <- filter(test,
                     test$GrLivArea <= 15000  & test$OverallQual <= 7 & test$YearBuilt <= 2019)

test_print <- arrange(test_print,Id)

rows <- c(test_print[1,]$Id,test_print[nrow(test_print),]$Id)

final_data <- submit %>%
  filter(submit$Id %in% rows)

cat("Predicted price range:", rows)
