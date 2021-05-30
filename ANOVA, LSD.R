# Package names
packages <- c("ggplot2","carData","lme4","lmerTest","magrittr" , "dplyr", "tidyr", "reshape2", "agricolae","car", "multcomp",  "rcompanion", "openxlsx", "ggpubr", "carData", "magrittr","corrplot", "FSA", "dslabs", "magrittr", "tidyverse")
upgrade(packageStatus())
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
   install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))

#----------function used in the analysis--------------#
LSDOUT <- function(x){
   Var <- row.names (x$groups) 
   newGroup <- data.frame ( Var  , x$groups[,1:2] ) 
   sort <- newGroup [ order ( newGroup$Var ), ] #按照變數名稱排序
   rownames(sort) <- c() #刪除ROWNAME
   plotdata <- data.frame (sort,"N"= x$means [,3],"SEM"<- x$means[,2]/sqrt(x$means[,3]) ,"SD"=x$means[,2],"CV"=x$means[,2]/x$means[,1] )
   names(plotdata) <- c( "Factor","mean", "Sign","n" , "SEM",  "SD" ,"CV")
   write.csv(plotdata,paste0("D:/LSD",format(Sys.time(), "-%Y%b%d-%H%M%S"),".csv"))
   return(plotdata)
}

dataSum<-function ( ob , var){
   se<-function(x){sd(x,na.rm = T)/sqrt(length(x))}
   out<-aggregate(ob, by = var, FUN= function(x) c("mean" = mean(x), "median" = median(x),"SD"=sd(x), "SE"=se(x),N=length(x),"CV"=cv <- sd(x) / mean(x)))
   write.csv(out,paste0("D:/summary",format(Sys.time(), "%d-%b-%Y %H%M%S"),".csv"))
   return(out)
}



#------------------------------------------------------------------------#
# input excel -> setting factor
edit ( sp1 <- read.csv ( "D:/bball.csv" , header = T , sep = "," ))
edit ( sp2 <- read.csv ( "D:/yield.csv" , header = T , sep = "," ))
edit ( sp3 <- read.csv ( "D:/yieldcom.csv" , header = T , sep = "," ))
edit ( sp4 <- read.csv ( "D:/PlantHeight.csv" , header = T , sep = "," ))
write.csv(sp1,paste0("D:/summary",format(Sys.time(),"%d-%b-%Y %H%M%S"),".csv"))

for( i in c(1:4) ){
   sp1[,i]<-as.factor(sp1[,i])
}

sp1[,8]<-as.factor(sp1[,8])
lmer( SL ~ Var+Bac+(1|Block)+Var*Bac*(1|Block) , data=sp1 )  %>% anova()
str(Anova)
#----用[grepl]或subset分類出不同曲集再進行分析---#
BB__a<-sp1[grepl("A",sp1$Block),]
BA<-subset(sp1,Block==c("A"))
BC<-subset
paste("fitbit", 1:12, ".jpg", sep = "")

for (i in c(1:4)) {
   sp2[,i]<-as.factor(sp2[,i])
   }
head(sp2)

comFula<-(Y~ year+season+rep+variety + 
             year:season + year/season/rep +
             year:variety +variety:season +
             year:season:variety
          )
comFula<-(Y~ (1|year)+(1|season)+(1|rep)+variety+
             (1|year)*(1|season)
             )
lmer(comFula,data=sp2) %>% Anova()


plot(aov1)

lm1=lm(
   Yld~Year+Site+ Year:Site +Year/Site/Rep+Tomatovar+Year:Tomatovar+Site:Tomatovar+Year:Site:Tomatovar
   )
for(i in as.integer(100*runif(10))
){
   x<-paste0("rawdata", i , ".jpg")
   print(x)
   }


#用aggregate取各組平均值、標準差、標準差(sd)、平均值的標準差(se)

dataSum(list(sp1$SL))

#前方觀測值以cbind(a,b)~變數公式寫法，或以list匡列所有觀測值與變數
#x<-aggregate(cbind(SL,FL,LP)~Var+Bac+Block,data=sp1,mean)
#y<-aggregate(cbind(SL,FL,LP)~Var+Bac+Block,data=sp1,sd,na.rm=T)
#z<-aggregate(cbind(SL,FL,LP)~Var+Bac+Block,data=sp1,se)
#name<-c("Var","Bac","Block","SL_mean","FL_mean","LP_mean","SL_sd","FL_sd"#,"LP_sd","SL_se","FL_se","LP_se")#
#out<-data.frame(x,y[,4:6],z[,4:6])
#names(out)<-name
#head(out)
#out %>% write.csv(paste0("D:/summary",format(Sys.time(), "%d-%b-%Y %H%M%S"),".csv"))


#變方均質性測試
#1. KS.test:檢定兩個分布是否來自於相同分布
#   如果輸入"pnorm"就是檢定該分布是否符合常態分佈(Ho: a=常態分布)
#   p<0.001 -> 該數不屬於常態分布
head(sp3)
plot(ggqqplot(sp2$產量))
plot(ggqqplot(sp3$實粒數))
ks.test(x=sp2$產量,"pnorm")
ks.test(x=sp2$小區,"pnorm")
ks.test(x=sp3$實粒重,"pnorm")

x<-ks.test(x=sp1$FL,"pnorm")
capture.output(x)

#2. qqplot，ggqqplot或qqnorm皆可
ggqqplot(sp2$產量)

qqnorm(sp1$SL)

#3.  	Shapiro-Wilk normality test，P<0.05顯著則不符常態
shapiro.test(sp1$LP)

#4.  Levene'test
#檢定兩組數植的變異數是否相同
leveneTest(SL~Var,sp1)


##相關分析(可略過)
#method=spearman 是無母數統計版本的皮爾森相關係數
#因為皮爾森相關係數需要兩個變數符合常態分佈
#如果符合常態，可以把後面method刪除
x=sp1$SL
y=sp1$FL
cor(x,y)
cor.test(x,y,method='spearman')
summary(cor.test(x,y,method='spearman'))


x<-lm(產量~品種+年+季+重複,data=sp2)
summary(aov(產量~品種+年+季+重複,data=sp2))
summary(aov(穗數~品種+年+季+重複,data=sp3))
summary(aov(總重~品種+年+季+重複,data=sp3))
summary(aov(稔實率~品種+年+季+重複+年*重複*季*品種*年,data=sp3))
summary(aov(一穗粒數~品種+年+季+重複,data=sp3))
summary(aov(千粒重~品種+年+季+重複,data=sp3))

 ##anova
sp1<-read.csv(file="D:/bb.csv")
head(sp1)
aov1<-aov(SL~Var+Bac+Plant+Bac*Var*Plant,data=sp1)
data<-LSD.test(aov1,"Var")

data
summary(aov1)
write.csv(capture.output(summary(aov1)), paste0("D:/summary",format(Sys.time(), "%d-%b-%Y %H%M%S"),".csv"))
summary(aov1)

##LSD
head(sp1)
L1 <- LSD.test ( aov1 , c("Var","Bac") , p.adj = "bonferroni" )
LSDOUT(L1)
L2 <- LSD.test ( aov1 , "Var" , p.adj = "bonferroni" )
LSDOUT(L2)

#LSD
plot(L1 <- LSD.test(ch1 , "line", p.adj = "bonferroni"))
plot(L2 <- LSD.test(ch2 , "line", p.adj = "bonferroni"))

aov1<-aov(bb$SL~bb$Var+bb$Bac)
o<-LSD.test(aov1,c("bb$Var","bb$Bac"))
o$means


##無母數分析KS檢定單因子
k1 = capture.output ( kruskal.test ( SL ~ Var , data = sp1))
k2 <- capture.output ( kruskal.test ( SL ~ Bac , data = sp1))

#無母數分析wilcox兩兩檢定，pairwise
#Bac內SL對Var做兩兩比較 (先用split將變數分組成不同list,再用lapply對每個變數進行檢定)
#Var內SL對Bac做兩兩比較
#-----#
wcTest<-function(data,ob,group){
   attach(data)
   dataGroupList <- split(ob,group)
   dataOut <-
      lapply(dataGroupList , function(x) pairwise.wilcox.test(ob ,group))
   detach()
   return(dataOut)
}
#-----#
sp1 <- read.csv ( "D:/bball.csv" , header = T , sep = "," )
sp1 %>% edit()
wcTest(sp1,sp1$LP,sp1$Var)
outData <- capture.output(list(dataList1,dataList2))
write.csv(outData,paste0("D:/wilcox",format(Sys.time(),"%d-%b-%Y%H%M%S"),".csv"))

#----NOT RUM----#
#sp1GVar <- split(sp1,sp1$Var)sp1GBac <- split(sp1,sp1$Bac)sp1GVar %>% lapply(., function(x) pairwise.wilcox.test(x$SL,x$Bac))sp1GVar %>% lapply(., function(x) pairwise.wilcox.test(x$LP,x$Bac))sp1GBac %>% lapply(., function(x) pairwise.wilcox.test(x$SL,x$Var))sp1GBac %>% lapply(., function(x) pairwise.wilcox.test(x$LP,x$Var))#





#friedman.test不接受重複，要先平均
x<-dataSum( list(sp1$FL,sp1$SL,sp1$LP), list(sp1$Var,sp1$Bac,sp1$Plant) )
head(x)
boxplot(SL ~ Var, data = bbbbb, xlab = "品種", ylab = "病斑長", main = "BB")
boxplot(FL ~ Var, data = x, xlab = "品種", ylab = "病斑長", main = "BB")
friedman.test ( SL ~ Var | Block , data = x)
edit(sp1)

######t-test######
?t.test
head(sp1)
t.test( lod~Var,data=sp1,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = TRUE,
       conf.level = 0.95)

