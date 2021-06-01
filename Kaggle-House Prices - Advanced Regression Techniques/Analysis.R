library(readr)
train <- read_csv("D:/OneDrive/TTD-CHL/Rworking/R-working-project/Kaggle-House Prices - Advanced Regression Techniques/train.csv")
head(train)
train<-read.csv("D:/OneDrive/TTD-CHL/Rworking/R-working-project/Kaggle-House Prices - Advanced Regression Techniques/train.csv")
head(train)

inPackage<-function(packages){
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(install.packages == FALSE)){install.packaegs((packages[!installed_packages]))}
  invisible(lapply(packages, library, character.only = TRUE))
}
inpackage<-c("knitr","ggplot2","plyr","dplyr","corrplot","caret","gridExtra","Rmisc","scales","ggrepel","randomForest","psych","xgboost")

#---------Data Clean----------#
train <- read.csv("../Kaggle-House Prices - Advanced Regression Techniques/train.csv", stringsAsFactors = F)
test <- read.csv("../Kaggle-House Prices - Advanced Regression Techniques/test.csv", stringsAsFactors = F)
dim(train)
# 81 columns/variables with 1460 observation
train[,c(1:10,81)] %>% str()
test %>% str()

# 只留下10個變數跟1個觀測直
test_labels <- test$Id

test$Id <- NULL    #刪除用不到的變數 ID
train$Id <- NULL   #刪除用不到的變數 ID
test %>% str()

test$SalePrice <- NA #插入一欄 saleprice
colnames(test)

all <-rbind( train, test)  #兩筆資料對接(垂直跌再一起)
dim(all)

ggplot(data=all[!is.na(all$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

summary(all$SalePrice)

numericVars <- which( sapply(all, is.numeric) ) #用WHICH+salppy挑出屬於數字的變數
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')
all_numVar <- all[, numericVars]  #數字的變數名
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #所有變數做相關分析

#跟價格相關性從高排到低
cor_sorted <- as.matrix( sort( cor_numVar[,'SalePrice'], decreasing = TRUE)) 

#選出 r>0.5a abs為絕對值
CorHigh <- names( which( apply ( cor_sorted, 1, function(x) abs(x)>0.5)))
#apply用於矩陣，1是垂直方向,２是水平方向

cor_numVar <- cor_numVar[CorHigh, CorHigh]
#相關矩陣


corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")
ggplot(data=all[!is.na(all$SalePrice),], aes(x=factor(OverallQual), y=SalePrice))+
  geom_boxplot(col='blue') + labs(x='Overall Quality') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)


#"lt", "d" or "n",代表標籤位置，"lt" means left and top, "d" means diagonal. If "n", add no textlabel.






