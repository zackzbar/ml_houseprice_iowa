library(tidyverse)
library(plyr)
library(knitr)
library(corrplot)
library(caret)
library(gridExtra)

#Load train dataset
train <- read.csv('./train.csv', stringsAsFactors = FALSE)

#Get rid of ID column
train$Id <- NULL

#Make characters lowercase and trim the whitespace
#train = apply(train, 2, function(x) tolower(x))
#train = apply(train, 2, function(x) trimws(x))
#train = as.data.frame(train)

#Check which columns have missing data
missing <- which(colSums(is.na(train)) > 0)
sort(colSums(sapply(train[missing], is.na)), decreasing=TRUE)
   
#### Imputing missing values ##################################################

#PoolQC has 1453 NAs.  We can just assign 'None' to the NAs and encode the 
#variable since the values are ordinal. We can just make one vector to use for 
#multiple variables since many follow the same levels of quality
train$PoolQC[is.na(train$PoolQC)] <- 'None'
quality <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
train$PoolQC <- as.integer(revalue(train$PoolQC, quality))

#MiscFeature has 1406 NAs.  Since the values are not ordinal, we can convert
#them into factors.
train$MiscFeature[is.na(train$MiscFeature)] <- 'None'
train$MiscFeature <- as.factor(train$MiscFeature)

#Alley has 1369 NAs  Since the values are not ordinal, we
#can covert them into factors.
train$Alley[is.na(train$Alley)] <- 'None'
train$Alley <- as.factor(train$Alley)

#Fence has 1179 NAs.  Fence seemingly might be ordinal, but since nothing 
#defines a "best" fence, we will make it a factor.
train$Fence[is.na(train$Fence)] <- 'None'
train$Fence <- as.factor(train$Fence)

#FireplaceQu has 690 NAs.  The number of NAs matches the number of houses with 
#zero fireplaces, and the values are ordinal, so we can use the quality vector
train$FireplaceQu[is.na(train$FireplaceQu)] <- 'None'
train$FireplaceQu <- as.integer(revalue(train$FireplaceQu, quality))
table(train$Fireplaces)  #Check that FireplaceQu matches number of 0s

#LotFrontage has 259 NAs.  We can impute with the median lot frontage of the 
#most common neighborhood.
for (i in 1:nrow(train)){
  if(is.na(train$LotFrontage[i])){
    train$LotFrontage[i] <- as.integer(median(train$LotFrontage[train$Neighborhood==train$Neighborhood[i]], na.rm=TRUE)) 
  }
}

#GarageType has 81 NAs.  The values appear not to be ordinal, so we can convert
#into a factor.
train$GarageType[is.na(train$GarageType)] <- 'No Garage'
train$GarageType <- as.factor(train$GarageType)

#GarageYrBlt has 81 NAs.  We can replace the NAs here with values from YearBuilt.
train$GarageYrBlt[is.na(train$GarageYrBlt)] <- train$YearBuilt[is.na(train$GarageYrBlt)]


#GarageFinish has 81 NAs.  This variable is ordinal, so we can use a custom vector.
train$GarageFinish[is.na(train$GarageFinish)] <- 'None'
finish <- c('None' = 0, 'Unf' = 1, 'RFn' = 2, 'Fin' = 3)
train$GarageFinish <- as.integer(revalue(train$GarageFinish, finish))

#GarageQual has 81 NAs.  This variable is ordinal, so we can use the quality vector.
train$GarageQual[is.na(train$GarageQual)] <- 'None'
train$GarageQual <- as.integer(revalue(train$GarageQual, quality))

#GarageCond has 81 NAs.  This variable is ordinal, so we can use the quality vector.
train$GarageCond[is.na(train$GarageCond)] <- 'None'
train$GarageCond <- as.integer(revalue(train$GarageCond, quality))

#BsmtExposure has 38 NAs.  This variable is ordinal, so we can make a custom vector.
train$BsmtExposure[is.na(train$BsmtExposure)] <- 'None'
exposure <- c('None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)
train$BsmtExposure <- as.integer(revalue(train$BsmtExposure, exposure))

#BsmtFinType2 has 38 NAs.  This variable is ordinal, so we can use the fintype vector.
train$BsmtFinType2[is.na(train$BsmtFinType2)] <- 'None'
fintype <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
train$BsmtFinType2 <- as.integer(revalue(train$BsmtFinType2, fintype))

#BsmtQual has 37 NAs.  This value is ordinal and we can use the quality vector.
train$BsmtQual[is.na(train$BsmtQual)] <- 'None'
train$BsmtQual <- as.integer(revalue(train$BsmtQual, quality))

#BsmtCond has 37 NAs.  This value is ordinal and we can use the quality vector.
train$BsmtCond[is.na(train$BsmtCond)] <- 'None'
train$BsmtCond <- as.integer(revalue(train$BsmtCond, quality))

#BsmtFinType1 has 37 NAs. This variable is ordinal, so we can use the fintype vector.
train$BsmtFinType1[is.na(train$BsmtFinType1)] <- 'None'
train$BsmtFinType1 <- as.integer(revalue(train$BsmtFinType1, fintype))

#MasVnrType has 8 NAs.  Checking the medians of the values of this variable, it
#looks like there is a significant difference between BrkCmn and None vs. the 
#other types.  We can custom adjust the ordinality to reflect this.
train$MasVnrType[is.na(train$MasVnrType)] <- 'None'
masonry <- c('None' = 0, 'BrkCmn' = 0, 'BrkFace' = 1, 'Stone' = 2)
train$MasVnrType <- as.integer(revalue(train$MasVnrType, masonry))

#MasVnrArea has 8 NAs.  We can just make the NAs here 0.
train$MasVnrArea[is.na(train$MasVnrArea)] <- 0

#Electrical has 1 NA.  Since the values are not ordinal, we can impute with the
#mode, 'SBrkr'. 
train$Electrical[is.na(train$Electrical)] <- names(sort(-table(train$Electrical)))[1]
train$Electrical <- as.factor(train$Electrical)

### Factorize or encode remaining character values #############################

#Non-ordinal values 
train$Foundation <- as.factor(train$Foundation)
train$Heating <- as.factor(train$Heating)
train$RoofStyle <- as.factor(train$RoofStyle)
train$RoofMatl <- as.factor(train$RoofMatl)
train$LandContour <- as.factor(train$LandContour)
train$BldgType <- as.factor(train$BldgType)
train$HouseStyle <- as.factor(train$HouseStyle)
train$Neighborhood <- as.factor(train$Neighborhood)
train$Condition1 <- as.factor(train$Condition1)
train$Condition2 <- as.factor(train$Condition2)
train$MSZoning <- as.factor(train$MSZoning)
train$LotConfig <- as.factor(train$LotConfig)
train$Exterior1st <- as.factor(train$Exterior1st)
train$Exterior2nd <- as.factor(train$Exterior2nd)
train$SaleType <- as.factor(train$SaleType)
train$SaleCondition <- as.factor(train$SaleCondition)

#Ordinal values
train$HeatingQC <- as.integer(revalue(train$HeatingQC, quality))
train$CentralAir <- as.integer(revalue(train$CentralAir, c('N' = 0, 'Y' = 1)))
train$LandSlope <- as.integer(revalue(train$LandSlope, c('Sev' = 0, 'Mod' = 1, 'Gtl' = 2)))
train$Street <- as.integer(revalue(train$Street, c('Grvl' = 0, 'Pave' = 1)))
train$PavedDrive <- as.integer(revalue(train$PavedDrive, c('N' = 0, 'P' = 1, 'Y' = 2)))
train$LotShape <- as.integer(revalue(train$LotShape, c('IR3'=0, 'IR2'=1, 'IR1'=2, 'Reg'=3)))
train$ExterQual <- as.integer(revalue(train$ExterQual, quality))
train$ExterCond <- as.integer(revalue(train$ExterCond, quality))
train$KitchenQual <- as.integer(revalue(train$KitchenQual, quality))
train$Functional <- as.integer(revalue(train$Functional, c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)))

#Get rid of Utilities variable since it has all values except one in one column
train$Utilities <- NULL

#Three variables should arguably be categorical though they are numeric.

#YrSold describes the year in which the house was sold.  The range is 2006-2010.
#As we know, the economy entered a recession during this period, particularly 
#in late 2007 and much of 2008.  So we may expect that housing prices will be
#different in 2009 and 2010.  
library(tidyverse)
ggplot(train[!is.na(train$SalePrice),], aes(x=as.factor(YrSold), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 800000, by=25000)) +
   coord_cartesian(ylim = c(0, 200000)) 
  
train$YrSold <- as.factor(train$YrSold)

#MoSold is listed as numeric but we will convert to factor
train$MoSold <- as.factor(train$MoSold)


#MSSubClass is encoded as a numeric variable, but should be a factor
train$MSSubClass <- as.factor(train$MSSubClass)
train$MSSubClass<-revalue(train$MSSubClass, c('20'='1 story 1946+', '30'='1 story 1945-', 
                                          '40'='1 story unf attic', '45'='1,5 story unf', 
                                          '50'='1,5 story fin', '60'='2 story 1946+', 
                                          '70'='2 story 1945-', '75'='2,5 story all ages', 
                                          '80'='split/multi level', '85'='split foyer', 
                                          '90'='duplex all style/age', '120'='1 story PUD 1946+', 
                                          '150'='1,5 story PUD all', '160'='2 story PUD 1946+', 
                                          '180'='PUD multilevel', '190'='2 family conversion'))


### Exploratory Data Analysis ##################################################

#Check how many of each type of variable there are.  The total should be 80.
num_var <- which(sapply(train, is.numeric))
fact_var <- which(sapply(train, is.factor))
char_var <- which(sapply(train, is.character))

#Correlation plot
train_num_var <- train[, num_var]
cor_num_var <- cor(train_num_var, use="pairwise.complete.obs")

cor_sorted <- as.matrix(sort(cor_num_var[,'SalePrice'], decreasing = TRUE))
cor_high <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_num_var <- cor_num_var[cor_high, cor_high]
corrplot.mixed(cor_num_var, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)


### Feature Engineering ########################################################

#1. The bathroom variables appear to all not be super important.  We could maybe
#combine all of them into one to make a stronger predictor? 

train$total_bathrooms <- train$FullBath + (train$HalfBath*0.5) + 
  train$BsmtFullBath + (train$BsmtHalfBath*0.5)

#2. The porch variables seem related.  We could combine them into one single
#porch variable and see whether this has greater correlation with SalesPrice

train$total_porch <- train$OpenPorchSF + train$EnclosedPorch + 
  train$X3SsnPorch + train$ScreenPorch

#3. Total living space can be created to combine the square footage below and
#above ground.

train$total_sqr_footage <- train$GrLivArea + train$TotalBsmtSF

#4. 


### Data Preparation ##########################################################

#We can check the correlation matrix to decide if we want to drop any varaibles.
#If two variables have high correlation with each other, we can drop one and 
#keep the one that has the highest correlation with SalePrice.

drop_cols <- c('GarageArea', 'X1st')
train <- train[,!(names(train) %in% drop_cols)]

#We want to remove outliers.  The most obvious outliers are the two homes 
plot(train$GrLivArea, train$SalePrice)
