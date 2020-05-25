
df = read_csv(".\\processed_data\\train_full.csv")

test = head(df)

df = as.data.frame(df)

df = drop(df, X1,Id,1)

char_to_ordinary = function(x){
  
  x = tolower(ifelse(is.numeric(x),x,trimws(x)))
}

colSums(df[is.na(df)])

sapply(test,2,tolower)

char_to_ordinary('fa')

#++++++++++++++++++++++++

char_to_ordinary = function(x){
  ifelse(x == 'nothing',0,ifelse(x %in% c('po', 'unf','no'), 1,ifelse(x %in% c('lwq', 'fa','mn'), 2,ifelse(x %in% c('rec','ta','av'),3,
                          ifelse(x %in% c('blq','gd'),4,ifelse(x %in% c('ex','alq'),5,6))))))
}

d = 'FireplaceQu'
colnames = c('ExterQual','ExterCond', 'BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinType2', 'HeatingQC','KitchenQual',
  'FireplaceQu','GarageQual', 'GarageCond', 'PoolQC')

#apply((array), margin, ...)
for (col_name in colnames){
 test[,col_name] =sapply(test[,col_name],char_to_ordinary)
}

#==============================

year_to_ordinal = function(year){
  decade = -1
  if (year < 1920) {
    decade = 1
  } else if (year > 1999){
    decade = 10
  }else{
    decade = ((year %/% 10 * 10) - 1900) / 10
  }
  #return(decade)
  paste0('group_',as.character(decade))
}
  
sapply(df$YearBuilt,year_to_ordinal)


((1919 %/% 10 * 10) - 1900) / 10


test[,colnames]
str(df$OverallQual)

df[,'OverallQual']

#5- #Ex	Excellent - Exceptional Masonry Fireplace
#4- #Gd	Good - Masonry Fireplace in main level
#3- TA	Average - Prefabricated Fireplace in main living area or Masonry Fireplace in basement
#2- Fa	Fair - Prefabricated Fireplace in basement
#1- Po	Poor - Ben Franklin Stove
#0- NA	No Fireplace

#====
#6- GLQ	Good Living Quarters
#5- ALQ	Average Living Quarters
#4- BLQ	Below Average Living Quarters	
#3- Rec	Average Rec Room
#2- LwQ	Low Quality
#1- Unf	Unfinshed
#0- NA	No Basement/ nothing

s = 'char'
d = 9
