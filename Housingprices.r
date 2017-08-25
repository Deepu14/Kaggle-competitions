ls()
rm(list = ls())
setwd("/Users/Deepu/Desktop/Kaggle datasets/House Prices/House Prices 2/")
getwd()
train = read.csv(file = "train.csv",sep = ",",strip.white = TRUE)
test = read.csv(file = "test.csv", sep = ",", strip.white = TRUE)
str(train)
str(test)
test$SalePrice <- sample(34900:163000,1459)
combine_data <- rbind(train,test)
str(combine_data)

combine_data$MSSubClass <- as.factor(combine_data$MSSubClass)


#binning dates
combine_data$YearBuilt <- cut(combine_data$YearBuilt,breaks = seq(1870,2010,10),
                              labels = c("1870","1880","1890","1900",
                                         "1910","1920","1930","1940",
                                         "1950","1960","1970","1980","1990","2000"),
                              right = FALSE)
combine_data$YearBuilt <- as.numeric(combine_data$YearBuilt)
#combine_data$YearBuilt <- as.factor(combine_data$YearBuilt)
#table(combine_data$YearBuilt)

combine_data$YearRemodAdd <- cut(combine_data$YearRemodAdd,breaks = seq(1950,2020,10),
                                 labels = c("1950","1960","1970","1980","1990",
                                            "2000","2010+"), right = FALSE)
combine_data$YearRemodAdd <- as.numeric(combine_data$YearRemodAdd)
#combine_data$YearRemodAdd <- as.factor(combine_data$YearRemodAdd)
#table(combine_data$YearRemodAdd)


#since most of the observations in RoofMatl belongs to the category CompShg
#i will label other categories to "other"
library(plyr)
combine_data$RoofMatl <- revalue(combine_data$RoofMatl,c("ClyTile" = "other",
                                  "Membran" ="other","Metal"="other","Roll"="other",
                                  "Tar&Grv" = "other","WdShake" = "other",
                                  "WdShngl" = "other"))
table(combine_data$RoofMatl)


combine_data$Exterior1st <- revalue(combine_data$Exterior1st, c("AsphShn"= "other",
                                    "BrkComm"="other","CBlock"="other","ImStucc"="other",
                                    "Stone"="other","WdShing"="Wd Shng"))
table(combine_data$Exterior1st)

combine_data$Exterior2nd <- revalue(combine_data$Exterior2nd, c("AsphShn"= "other",
                                    "Brk Cmn"="other","CBlock"="other","ImStucc"="other",
                                     "Stone"="other","Other"="other"))
table(combine_data$Exterior2nd)

combine_data$MasVnrType <- revalue(combine_data$MasVnrType , c("BrkCmn"="Brk",
                                   "BrkFace"="Brk"))
table(combine_data$MasVnrType)

combine_data$ExterQual <- revalue(combine_data$ExterQual, c("Ex"="Gd","Fa"="TA"))
table(combine_data$ExterQual)

combine_data$ExterCond <- revalue(combine_data$ExterCond, c("Ex"="Gd","Fa"="TA"))
table(combine_data$ExterCond)

combine_data$Foundation <- revalue(combine_data$Foundation, c("Slab"="other",
                                   "Stone"="other","Wood"="other"))

combine_data$BsmtQual <- revalue(combine_data$BsmtQual, c("Ex"="Gd","Fa"="TA"))
table(combine_data$BsmtQual)

combine_data$BsmtCond <- revalue(combine_data$BsmtCond, c("Fa"="TA"))
table(combine_data$BsmtCond)

combine_data$Heating <- revalue(combine_data$Heating, c("Floor" = "other",
                                "Grav"="other","OthW"="other","Wall"="other",
                                "GasW"="Gas","GasA"="Gas"))

combine_data$HeatingQC <- revalue(combine_data$HeatingQC, c("Ex"="Gd","Fa"="TA"))
table(combine_data$HeatingQC)

combine_data$Electrical <- revalue(combine_data$Electrical,c("FuseA"="Fuse",
                                   "FuseF"="Fuse","FuseP"="Fuse","Mix"="SBrkr"))
table(combine_data$Electrical)

combine_data$KitchenQual <- revalue(combine_data$KitchenQual,c("Ex"="Gd","Fa"="TA"))
table(combine_data$KitchenQual)

combine_data$Functional <- revalue(combine_data$Functional,c("Maj1"="ded","Maj2"="ded","Min1"="ded",
                                                             "Min2"="ded","Mod"="ded",
                                                             "Sev"="ded"))
table(combine_data$Functional)

combine_data$FireplaceQu <- revalue(combine_data$FireplaceQu,c("Ex"="Gd","Fa"="TA"))
table(combine_data$FireplaceQu)

#GarageYr Blt has an outlier  2207
which(combine_data$GarageYrBlt == 2207)
combine_data$GarageYrBlt[2593] <- 2007

combine_data$GarageQual <- revalue(combine_data$GarageQual,c("Ex"="Gd","Fa"="TA"))
table(combine_data$GarageQual)

combine_data$GarageCond <- revalue(combine_data$GarageCond, c("Ex"="Gd","Fa"="TA"))
table(combine_data$GarageCond)

combine_data$Fence <- revalue(combine_data$Fence, c("GdPrv"="Prv","MnPrv"="Prv",
                                                    "GdWo"="Wood","MnWw"="Wood"))
table(combine_data$Fence)

combine_data$MoSold <- as.factor(combine_data$MoSold)
combine_data$YrSold <- as.factor(combine_data$YrSold)

combine_data$MSSubClass <- as.factor(combine_data$MSSubClass)
class(combine_data$MSSubClass)
table(combine_data$MSSubClass)
combine_data$MSSubClass <- revalue(combine_data$MSSubClass, c("30"="20",
                           "50"="45","70"="60","85"="80","150"="120",
                           "180"="160"))
combine_data$MSSubClass <- revalue(combine_data$MSSubClass, c("40"="20","75"="60"))

class(combine_data$MSZoning)
table(combine_data$MSZoning)

combine_data$LotShape <- revalue(combine_data$LotShape, c("IR3"="IR2"))
table(combine_data$LotShape)

table(combine_data$LotConfig)
combine_data$LotConfig <- revalue(combine_data$LotConfig, c("FR3"="FR2"))
table(combine_data$LotConfig)

table(combine_data$Condition1)
combine_data$Condition1 <- revalue(combine_data$Condition1, c("RRAe"="RR",
                           "RRAn"="RR","RRNe"="RR","RRNn"="RR","PosA"="Pos",
                           "PosN"="Pos"))
table(combine_data$Condition1)

table(combine_data$Condition2)
combine_data$Condition2 <- revalue(combine_data$Condition2, c("RRAe"="RR",
                                                              "RRAn"="RR","RRNn"="RR","PosA"="Pos",
                                                              "PosN"="Pos"))
table(combine_data$Condition2)

table(combine_data$BldgType)
combine_data$BldgType <- revalue(combine_data$BldgType, c("TwnhsE"="Twnhs"))
table(combine_data$BldgType)


table(combine_data$HouseStyle)
combine_data$HouseStyle <- revalue(combine_data$HouseStyle,c("1.5Fin"="1.5",
                                   "1.5Unf"="1.5","2.5Fin"="2Story",
                                   "2.5Unf"="2Story","SFoyer"="Split","SLvl"="Split"))
table(combine_data$HouseStyle)

###########Treating Missing Values
combine_data$MSZoning[which(is.na(combine_data$MSZoning))] <- "RL"
combine_data$Exterior1st[which(is.na(combine_data$Exterior1st))] <- "VinylSd"
combine_data$Exterior2nd[which(is.na(combine_data$Exterior2nd))] <- "VinylSd" 




#remove PoolQC, Fence, MiscFeature, MiscVal,Alley, FireplaceQu, Street, Utilities,Condition2


library(dplyr)
train1 <- combine_data[1:nrow(train),]
test1 <- combine_data[-(1:nrow(train)),]

train1 <- select(train1,-c(Alley, PoolQC, Fence, MiscFeature, MiscVal,
                          FireplaceQu,Street, Utilities, Condition2))
classes <- lapply((train1),FUN = function(x){is.numeric(x)})
numeric_cols <- names(classes[classes == "TRUE"])
categorical_cols <- names(classes[classes == "FALSE"])

train_numeric_data <- train1[numeric_cols]
train_cat_data <- train1[categorical_cols]
library(missForest)
imputed_numdata <- missForest(train_numeric_data)
train_numeric_data <- imputed_numdata$ximp
cor(train_numeric_data$SalePrice,train_numeric_data)
which(train_numeric_data$BsmtFinSF1 >2000)
which(train_numeric_data$GrLivArea >4000)
train_numeric_data<-train_numeric_data[-c(524,692,899,1183,1299),]
nrow(train_numeric_data)

test1 <- select(test1,-c(Alley, PoolQC, Fence, MiscFeature, MiscVal,
                         FireplaceQu,Street, Utilities, Condition2,SalePrice))
classes1 <- lapply((test1),FUN = function(x){is.numeric(x)})
num_cols <- names(classes1[classes1 == "TRUE"])
cat_cols <- names(classes1[classes1 == "FALSE"])
test_num_data <- test1[num_cols]
test_cat_data <- test1[cat_cols]
imputed_testdata <- missForest(test_num_data)
test_num_data <- imputed_testdata$ximp
#LotFrontage+LotArea+OverallQual+YearBuilt+YearRemodAdd+MasVnrArea+BsmtFinSF1+BsmtUnfSF+TotalBsmtSF+ 
#X1stFlrSF+X2ndFlrSF+GrLivArea+BsmtFullBath+FullBath+HalfBath+TotRmsAbvGrd+Fireplaces+GarageYrBlt+ 
#GarageCars+GarageArea+WoodDeckSF+OpenPorchSF 

linreg <- lm(SalePrice~ LotFrontage+LotArea+OverallQual+YearBuilt+YearRemodAdd+MasVnrArea+BsmtFinSF1+BsmtUnfSF+
               TotalBsmtSF+ X1stFlrSF+X2ndFlrSF+GrLivArea+BsmtFullBath+FullBath+HalfBath+TotRmsAbvGrd+Fireplaces+
               GarageYrBlt+GarageCars+GarageArea+WoodDeckSF+OpenPorchSF, train_numeric_data )
summary(linreg)

pred1 <- predict(linreg,test_num_data,type = "response")
pred1 <- as.data.frame(pred1)
pred1 <- data_frame(Id =test1$Id,SalePrice = pred1$pred1 )
pred1$SalePrice[which(pred1$SalePrice < 0)] <- min(train_numeric_data$SalePrice)
write.csv(pred1,"linreg.csv")
#this gave me an RMSE score of 0.2316

linreg1 <- lm(SalePrice~ LotFrontage+LotArea+OverallQual+YearBuilt+YearRemodAdd+MasVnrArea+BsmtFinSF1+
               TotalBsmtSF+ X1stFlrSF+X2ndFlrSF+GrLivArea+FullBath+Fireplaces+
               GarageArea+WoodDeckSF+OpenPorchSF, train_numeric_data )
summary(linreg1)

pred2 <- predict(linreg1,test_num_data,type = "response")
pred2 <- as.data.frame(pred2)
pred2 <- data_frame(Id =test1$Id,SalePrice = pred2$pred2 )
pred2$SalePrice[which(pred2$SalePrice < 0)] <- min(train_numeric_data$SalePrice)
write.csv(pred2,"linreg2.csv")
#RMSE score is 0.232
#this did not improve my RMSE score

#now i will also include categorical variables
#train data
str(train_cat_data)
colSums(is.na(train_cat_data))

table(train_cat_data$Electrical)
train_cat_data$Electrical[which(is.na(train_cat_data$Electrical))] <- "SBrkr"
library(missForest)
impute_catdata <- missForest(train_cat_data)
train_cat_data <- impute_catdata$ximp
colSums(is.na(train_cat_data))
train_cat_data<-train_cat_data[-c(524,692,899,1183,1299),]

train_complete_data <- cbind(train_numeric_data,train_cat_data)

#test data

str(test_cat_data)
colSums(is.na(test_cat_data))

table(test_cat_data$KitchenQual)
test_cat_data$KitchenQual[which(is.na(test_cat_data$KitchenQual))] <- "TA"

table(test_cat_data$Functional)
test_cat_data$Functional[which(is.na(test_cat_data$Functional))] <- "Typ"

table(test_cat_data$SaleType)
test_cat_data$SaleType[which(is.na(test_cat_data$SaleType))] <- "WD"

impute_testcat <- missForest(test_cat_data)
test_cat_data <- impute_testcat$ximp
colSums(is.na(test_cat_data))

test_complete_data <- cbind(test_num_data,test_cat_data)

####################################################
model1 <- lm(SalePrice~ LotFrontage+LotArea+OverallQual+YearBuilt+YearRemodAdd+MasVnrArea+BsmtFinSF1+
               TotalBsmtSF+GrLivArea+TotRmsAbvGrd+Fireplaces+
               GarageYrBlt+GarageCars+GarageArea+WoodDeckSF+OpenPorchSF +MSSubClass+MSZoning+LotShape+LandContour+
               LandSlope+Neighborhood+ExterQual+BsmtQual+BsmtExposure+GarageType+Condition1+RoofStyle+
               KitchenQual+Exterior1st+RoofMatl+SaleCondition, train_complete_data)
summary(model1)

model1_pred <- predict(model1,test_complete_data,type = "response")
model1_pred <- as.data.frame(model1_pred)
model1_pred <- data_frame(Id = test$Id,SalePrice=model1_pred$model1_pred)
model1_pred$SalePrice[which(model1_pred$SalePrice <0)] <- min(train_complete_data$SalePrice)
write.csv(model1_pred,"model1.csv",row.names = FALSE)
#Now my RMSE score is 0.169 which is far better than the previous score

#Now i will do log transform for the response variable 
library(agricolae)
skewness(train_numeric_data$SalePrice)
kurtosis(train_numeric_data$SalePrice)
d <- density(train_numeric_data$SalePrice)
plot(d)

dl <- density(log(train_numeric_data$SalePrice))
plot(dl)

model2 <- lm(log(SalePrice)~ LotFrontage+LotArea+OverallQual+YearBuilt+YearRemodAdd+MasVnrArea+BsmtFinSF1+
               TotalBsmtSF+GrLivArea+TotRmsAbvGrd+Fireplaces+
               GarageYrBlt+GarageCars+GarageArea+WoodDeckSF+OpenPorchSF +MSSubClass+MSZoning+LotShape+LandContour+
               LandSlope+Neighborhood+ExterQual+BsmtQual+BsmtExposure+GarageType+Condition1+RoofStyle+
               KitchenQual+Exterior1st+RoofMatl+SaleCondition, train_complete_data)
summary(model2)

model2_pred <- predict(model2,test_complete_data,type = "response")
model2_pred <- as.data.frame(model2_pred)
model2_pred <- data_frame(Id = test$Id,SalePrice=exp(model2_pred$model2_pred))
write.csv(model2_pred,"model2.csv",row.names = FALSE)
#This gave me an RMSE score of 0.14020(my best score)

#I will also check the distribution of significant variables and perform log transformations on them
#LotArea, BsmtFinSF1,TotalBsmtSF,GrLivArea, GarageArea

skewness(train_complete_data$LotArea)
kurtosis(train_complete_data$LotArea)
lotarea <- density(train_complete_data$LotArea)
plot(lotarea)

plot(density(log(train_complete_data$LotArea)))
skewness(log(train_complete_data$LotArea))
kurtosis(log(train_complete_data$LotArea))


plot(density(log(train_complete_data$GrLivArea)))
skewness(log(train_complete_data$GrLivArea))
kurtosis(train_complete_data$GrLivArea)


#now i will apply log transformations to LotArea and GrLivArea
model3 <- lm(log(SalePrice)~ LotFrontage+log(LotArea)+OverallQual+YearBuilt+YearRemodAdd+MasVnrArea+BsmtFinSF1+
               TotalBsmtSF+log(GrLivArea)+TotRmsAbvGrd+Fireplaces+
               GarageYrBlt+GarageCars+GarageArea+WoodDeckSF+OpenPorchSF +MSSubClass+MSZoning+LotShape+LandContour+
               LandSlope+Neighborhood+ExterQual+BsmtQual+BsmtExposure+GarageType+Condition1+RoofStyle+
               KitchenQual+Exterior1st+RoofMatl+SaleCondition, train_complete_data)
summary(model3)
model3_pred <- predict(model3,test_complete_data,type = "response")
model3_pred <- as.data.frame(model3_pred)
model3_pred <- data.frame(Id = test$Id,SalePrice = exp(model3_pred$model3_pred))
write.csv(model3_pred,"model3.csv",row.names = FALSE)
#this gave me RMSE score of 0.13904 which is a slight improvement over the previous score 0.14020

#Let's check if the model satisfies linear regression assumptions
plot(model3)
#Looks like the model satisfies linear regression assumptions


#Let's try LASSO Regression which penalizes the variables that have multicollinearity and performs feature selection
#LASSO- Least Absolute Shrinkage and Selection Operator. It performs both variable selection and regularization.
#It forces the sum of the absolute value of coefficients to be less than a fixed value which results in certain coefficients
#set to be zero effectively removing those variables from the model, making the model simpler and more intrepretable.
#The regularization parameter lambda governs the degree to which the coefficients are penalized.

library(glmnet)
#to select the best regularization parameter(lambda) we are using cross validation 
x_train <- model.matrix( ~LotFrontage+(LotArea)+OverallQual+YearBuilt+YearRemodAdd+MasVnrArea+BsmtFinSF1+
                           TotalBsmtSF+(GrLivArea)+TotRmsAbvGrd+Fireplaces+
                           GarageYrBlt+GarageCars+GarageArea+WoodDeckSF+OpenPorchSF +MSSubClass+MSZoning+LotShape+LandContour+
                           LandSlope+Neighborhood+ExterQual+BsmtQual+BsmtExposure+GarageType+Condition1+RoofStyle+
                           KitchenQual+Exterior1st+RoofMatl+SaleCondition-1,data = train_complete_data)
#model.matrix is used in order to have a matrix that transform categorical variables to dummy variables.
CV <- cv.glmnet(x_train,(train_complete_data$SalePrice),alpha = 1,nlambda = 1000,nfolds = 10)
#if alpha = 1, then it is Lasso Regression, if alpha = 0, it is Ridge Regression.
plot(CV)
#From the plot, the numbers on the top of x axis indicates the number of non-zero variables we have
lasso <- glmnet(x=x_train,y = train_complete_data$SalePrice,alpha = 1,
                lambda = CV$lambda.min)
lasso
lasso$beta[,1]
x_test <- model.matrix( ~LotFrontage+(LotArea)+OverallQual+YearBuilt+YearRemodAdd+MasVnrArea+BsmtFinSF1+
                         TotalBsmtSF+(GrLivArea)+TotRmsAbvGrd+Fireplaces+
                         GarageYrBlt+GarageCars+GarageArea+WoodDeckSF+OpenPorchSF +MSSubClass+MSZoning+LotShape+LandContour+
                         LandSlope+Neighborhood+ExterQual+BsmtQual+BsmtExposure+GarageType+Condition1+RoofStyle+
                         KitchenQual+Exterior1st+RoofMatl+SaleCondition-1,data =test_complete_data)
y_pred <- predict(lasso,x_test)
colnames(y_pred) <- "SalePrice"
lasso_pred <- as.data.frame(y_pred)
lasso_pred <- data.frame(Id = test$Id,SalePrice = lasso_pred$SalePrice)
write.csv(lasso_pred,"lassoreg.csv",row.names = FALSE)
#this gave me RMSE score of 0.15815 which is not better than my previous model


#Let's try XGBoost
#XGBoost works only with numeric vectors. So we need to perform One Hot Encoding

install.packages("xgboost")
library(xgboost)
library(caret)
xg_train <- model.matrix( ~ LotFrontage+log(LotArea)+OverallQual+YearBuilt+YearRemodAdd+MasVnrArea+BsmtFinSF1+
                           TotalBsmtSF+log(GrLivArea)+TotRmsAbvGrd+Fireplaces+
                           GarageYrBlt+GarageCars+GarageArea+WoodDeckSF+OpenPorchSF +MSSubClass+MSZoning+LotShape+LandContour+
                           LandSlope+Neighborhood+ExterQual+BsmtQual+BsmtExposure+GarageType+Condition1+RoofStyle+
                           KitchenQual+Exterior1st+RoofMatl+SaleCondition-1,data = train_complete_data)
xg_test <- model.matrix(~LotFrontage+log(LotArea)+OverallQual+YearBuilt+YearRemodAdd+MasVnrArea+BsmtFinSF1+
                          TotalBsmtSF+log(GrLivArea)+TotRmsAbvGrd+Fireplaces+
                          GarageYrBlt+GarageCars+GarageArea+WoodDeckSF+OpenPorchSF +MSSubClass+MSZoning+LotShape+LandContour+
                          LandSlope+Neighborhood+ExterQual+BsmtQual+BsmtExposure+GarageType+Condition1+RoofStyle+
                          KitchenQual+Exterior1st+RoofMatl+SaleCondition-1,data = test_complete_data)
xgb <- xgboost(data = xg_train,label = (train_complete_data$SalePrice),booster="gblinear",
               eta=0.1,max_depth=10,nrounds = 25,subsample=0.8,objective="reg:linear",
               alpha=1,gamma=2,colsample_bytree=0.5,
               seed=1,eval_metric="rmse")
xgb_pred <- predict(xgb,xg_test)
xgb_pred <- as.data.frame(xgb_pred)
xgb_pred <- data_frame(Id=test_complete_data$Id,SalePrice=xgb_pred$xgb_pred)
write.csv(xgb_pred,"xgboost.csv",row.names = FALSE)



xg_train2 <- model.matrix( ~ LotFrontage+log(LotArea)+OverallQual+YearBuilt+YearRemodAdd+MasVnrArea+BsmtFinSF1+
                             TotalBsmtSF+log(GrLivArea)+TotRmsAbvGrd+Fireplaces+
                             GarageYrBlt+GarageCars+GarageArea+WoodDeckSF+OpenPorchSF +MSSubClass+MSZoning+LotShape+LandContour+
                             LandSlope+Neighborhood+ExterQual+BsmtQual+BsmtExposure+GarageType+Condition1+RoofStyle+
                             KitchenQual+Exterior1st+RoofMatl+SaleCondition-1,data = train_complete_data)
xg_test2 <- model.matrix( ~ LotFrontage+log(LotArea)+OverallQual+YearBuilt+YearRemodAdd+MasVnrArea+BsmtFinSF1+
                            TotalBsmtSF+log(GrLivArea)+TotRmsAbvGrd+Fireplaces+
                            GarageYrBlt+GarageCars+GarageArea+WoodDeckSF+OpenPorchSF +MSSubClass+MSZoning+LotShape+LandContour+
                            LandSlope+Neighborhood+ExterQual+BsmtQual+BsmtExposure+GarageType+Condition1+RoofStyle+
                            KitchenQual+Exterior1st+RoofMatl+SaleCondition-1,data = test_complete_data)
xgb2 <- xgboost(data = xg_train2,label = (train_complete_data$SalePrice),booster="gbtree",
                eta=0.1,max_depth=10,nrounds = 200,subsample=0.8,objective="reg:linear",
                alpha=1,gamma=2,colsample_bytree=0.5,
                seed=1,eval_metric="rmse")
xgb2_pred <- predict(xgb2,xg_test2)
xgb2_pred <- as.data.frame(xgb2_pred)
xgb2_pred <- data_frame(Id=test_complete_data$Id,SalePrice=xgb2_pred$xgb2_pred)
write.csv(xgb2_pred,"xgboost2.csv",row.names = FALSE)
#this gave me RMSE score of 0.14844

library(caret)
names(getModelInfo())
modelLookup(model = "xgbLinear")
modelLookup(model = "xgbTree")
fitcontrol <- trainControl(method = "repeatedcv",repeats = 3)
grid <- expand.grid(nrounds=seq(50,200,50),
                    eta= seq(0.1,0.4,0.1),
                    lambda=c(0,1),
                    alpha=c(0,1))
model_xgb <- train(xg_train2,train_complete_data$SalePrice,method="xgbLinear",trControl = fitcontrol,tuneGrid=grid)
model_xgb_pred <- predict(model_xgb$finalModel,xg_test2)
model_xgb_pred <- as.data.frame(model_xgb_pred)
model_xgb_pred <- data_frame(Id=test_complete_data$Id, SalePrice=model_xgb_pred$model_xgb_pred)
write.csv(model_xgb_pred,"Model_xgb.csv",row.names = FALSE)
#this did not improve my model's accuracy

####Now i will use boruta library for feature selection
library(Boruta)
boruta <- Boruta(SalePrice ~.,data = train_complete_data,doTrace=2, holdHistory=TRUE)
boruta
boruta$finalDecision
final.boruta <- TentativeRoughFix(boruta)
getSelectedAttributes(final.boruta,withTentative = FALSE)
final.boruta.df <- attStats(final.boruta)
class(final.boruta.df)
plot(final.boruta)
selected_attributes <- rownames(final.boruta.df[final.boruta.df$decision == "Confirmed",])

xg_boruta_train <- model.matrix(~ LotFrontage + LotArea +OverallQual+OverallCond+YearBuilt+YearRemodAdd+MasVnrArea+  
                                BsmtFinSF1+BsmtUnfSF+TotalBsmtSF+X1stFlrSF+X2ndFlrSF+GrLivArea+BsmtFullBath+
                                FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+TotRmsAbvGrd+Fireplaces+GarageYrBlt+
                                GarageCars+GarageArea+WoodDeckSF+OpenPorchSF+ScreenPorch+MSSubClass+MSZoning+    
                                LotShape+LandContour+LandSlope+Neighborhood+BldgType+HouseStyle+RoofStyle+   
                                Exterior1st+Exterior2nd+MasVnrType+ExterQual+Foundation+BsmtQual+BsmtExposure+
                                BsmtFinType1+HeatingQC+CentralAir+KitchenQual+Functional+GarageType+GarageFinish+ 
                                PavedDrive+SaleCondition-1,data = train_complete_data)
xg_boruta_test <- model.matrix(~ LotFrontage + LotArea +OverallQual+OverallCond+YearBuilt+YearRemodAdd+MasVnrArea+  
                                 BsmtFinSF1+BsmtUnfSF+TotalBsmtSF+X1stFlrSF+X2ndFlrSF+GrLivArea+BsmtFullBath+
                                 FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+TotRmsAbvGrd+Fireplaces+GarageYrBlt+
                                 GarageCars+GarageArea+WoodDeckSF+OpenPorchSF+ScreenPorch+MSSubClass+MSZoning+    
                                 LotShape+LandContour+LandSlope+Neighborhood+BldgType+HouseStyle+RoofStyle+   
                                 Exterior1st+Exterior2nd+MasVnrType+ExterQual+Foundation+BsmtQual+BsmtExposure+
                                 BsmtFinType1+HeatingQC+CentralAir+KitchenQual+Functional+GarageType+GarageFinish+ 
                                 PavedDrive+SaleCondition-1,data = test_complete_data)
xgb3 <- xgboost(data = xg_boruta_train,label = (train_complete_data$SalePrice),booster="gbtree",
                eta=0.1,max_depth=10,nrounds = 200,subsample=0.8,objective="reg:linear",
                alpha=1,gamma=2,colsample_bytree=0.5,
                seed=1,eval_metric="rmse")
xgb3_pred <- predict(xgb3,xg_boruta_test)
xgb3_pred <- as.data.frame(xgb3_pred)
xgb3_pred <- data_frame(Id=test_complete_data$Id, SalePrice=xgb3_pred$xgb3_pred)
write.csv(xgb3_pred,"boruta_xgb.csv",row.names = FALSE)
#This gave me an RMSE score of 0.14503 which is not better than my best score


#Now i will use Bayesian Optimization for parameter tuning
library(rBayesianOptimization)
data <- select(train_complete_data,-c(Id, SalePrice))
label <- as.matrix(train_complete_data$SalePrice)
bayes_train <- list(data=as(as.matrix(data),"dgCMatrix"),label=label)
#bayes_train <- as.matrix(bayes_train)
dtrain <- xgb.DMatrix(bayes_train$data,label=bayes_train$label)
cv_folds <- KFold(target = bayes_train$label,nfolds = 10,stratified = TRUE,
                  seed=1)

xgb_cv_bayes <- function(eta,max.depth, min_child_weight, subsample,colsample_bytree){
cv <- xgb.cv(params = list(booster = "gbtree",
                           eta=eta,
                           max_depth = max.depth,
                           min_child_weight = min_child_weight,
                           subsample = subsample,
                           colsample_bytree = colsample_bytree,
                           lambda = 1, alpha = 0,
                           objective = "reg:linear",
                           eval_metric = "rmse"),
               data = dtrain, nround = 200,
               folds = cv_folds, prediction = TRUE, showsd = TRUE,
               early_stopping_rounds = 5, maximize = TRUE, verbose = 0)
  list(Score = -cv$evaluation_log[, min(test_rmse_mean)],
       Pred = cv$pred,
       cb.print.evaluation(period = 1))
}

bayes_result <- BayesianOptimization(xgb_cv_bayes,
                bounds = list(eta = seq(0.1,0.4,0.1)
                              , max.depth = seq(3L, 10L,1L)
                              , min_child_weight = c(3L, 10L)
                              , subsample = c(0.8, 1)
                              , colsample_bytree = c(0.5, 1))
                              , init_grid_dt = NULL, init_points = 10
                              , n_iter = 5, acq = "ucb",kappa = 3
                              , eps = 1.5,verbose = TRUE)

xgb_bayesian <- xgboost(data = xg_train2,label = (train_complete_data$SalePrice),booster="gbtree",
                eta=0.03,max_depth=10,nrounds = 2000,subsample=1,objective="reg:linear",
                lambda = 1, alpha = 0,colsample_bytree=1.0,min_child_weight=3.0,
                seed=1,eval_metric="rmse")
xgb_bayesian_pred <- predict(xgb_bayesian,xg_test2)
xgb_bayesian_pred<- as.data.frame(xgb_bayesian_pred)
xgb_bayesian_pred <- data_frame(Id=test_complete_data$Id, SalePrice=xgb_bayesian_pred$xgb_bayesian_pred)
write.csv(xgb_bayesian_pred,"xgb_bayesian_pred.csv",row.names = FALSE)


xgb4 <- xgboost(data = xg_train2,label = (train_complete_data$SalePrice),booster = 'gbtree',
                seed = 0,
                colsample_bytree = 0.5,
                subsample = 0.8,
                eta = 0.02, 
                objective = 'reg:linear',
                nrounds = 1000,
                max_depth = 12,
                alpha = 1,
                gamma = 2,
                min_child_weight = 1)
xgb4_pred <- predict(xgb4,xg_test2)
xgb4_pred <- as.data.frame(xgb4_pred)
xgb4_pred <- data_frame(Id=test_complete_data$Id, SalePrice = xgb4_pred$xgb4_pred)
write.csv(xgb4_pred,"xgb4.csv",row.names = FALSE)
#RMSE Score 0.14764
