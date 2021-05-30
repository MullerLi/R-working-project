# Package names
packages <- c("rJava","jiebaR","tmcn","wordcloud","wordcloud2","ggplot2","tidyverse","carData","magrittr" , "dplyr", "tidyr", "reshape2", "agricolae","car", "multcomp",  "rcompanion", "openxlsx", "ggpubr", "carData", "magrittr","corrplot","datarium","rstatix", "FSA", "dslabs", "magrittr", "tidyverse")
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
upgrade(packageStatus())
invisible(lapply(packages, library, character.only = TRUE))


#

x<-1
y<-c(1,2,3,4)
y2<-c(x,y[1])
y2

for (i=1:50){
  x<-c(i)
  paste0("x",i)<-x
}

%>% #----------function used in the analysis--------------#
LSDOUT <- function(x){
  Var <- row.names (x$groups) 
  newGroup <- data.frame ( Var  , x$groups[,1:2] ) 
  sort <- newGroup [ order ( newGroup$Var ), ] #???照變數???稱??????
  rownames(sort) <- c() #??????ROWNAME
  plotdata <- data.frame (sort,"N"= x$means [,3],"SEM"<- x$means[,2]/sqrt(x$means[,3]) ,"SD"=x$means[,2],"CV"=x$means[,2]/x$means[,1] )
  names(plotdata) <- c( "Factor","mean", "Sign","n" , "SEM",  "SD" ,"CV")
  write.csv(plotdata,paste0("D:/LSD",format(Sys.time(), "-%Y%b%d-%H%M%S"),".csv"))
  return(plotdata)
}

dataSum<-function (ob , var){
  se<-function(x){sd(x,na.rm = T)/sqrt(length(na.omit(x)))}
  out<-aggregate(ob, by = var,FUN= function(x)
                    c("mean" = mean(x), "median" = median(x),"SD"=sd(x), "SE"=se(x),N=length(na.omit(x)),"CV"= cv <- sd(x) / mean(x)))
  write.csv(out,paste0("D:/summary",format(Sys.time(),"%d-%b-%Y%H%M%S"),".csv"))
  return(out)
}
?aggregate
#???數
max(seq_along( colnames(sp1) ))
seq_along( rownames(sp1) )
data(Oats)
library(nlme)
str(Oats)
head(Oats)
x<-c(1,2,3,4,5,6,NA)
mD1<-lm(yield~Variety*nitro,data=Oats)
summary(mD1)
mD2<-lme(yield~Variety*nitro,data=Oats,random=~1|Block/Variety)

summary(mD2)
length()
na.omit(x)
#------------------------------------------------------------------------#
# input excel -> setting factor
edit ( sp1 <- read.csv ( "D:/bball.csv" , header = T , sep = "," ))
edit ( sp2 <- read.csv ( "D:/yield.csv" , header = T , sep = "," ))
edit ( sp3 <- read.csv ( "D:/yieldcom.csv" , header = T , sep = "," ))
edit ( sp4 <- read.csv ( "D:/PlantHeight.csv" , header = T , sep = "," ))
#----???[grepl]???subset??????出不???曲??????進?????????---#
BB__a<-sp1[grepl("A",sp1$Block),]
BA<-subset(sp1,Block==c("A"))
BC<-subset
seq_along(c)

dataSum(list("yield"=sp2$Y),list(sp2$year,sp2$season,sp2$variety))
sum(is.na(sp2))

#???aggregate?????????平???值、??????差?????????差(sd)???平???值?????????差(se)
head(sp2)
dataSum(list("yield"=sp2$Y),
        list("seqson"=sp2$season,"variety"=sp2$variety,"rep"=sp2$rep))

dataSum(list("yield"=sp2$Y),
        list("variety"=sp2$variety)) %>% head()


map_lgl(c(1,2,3), is.even)
map_chr(c(1,2,3), is.even)
map_dbl(c(1,2,3), sqrt)
map_int(c(1,2,3), is.even)
map_at(c(1,2,3,4), 2, sqrt)
map_if(c(1,2,3,4), function(x) {x>2},sqrt)
#{}??????if????????????件??????

install.packages("Rcmdr")
library("Rcmdr")
#???方觀測值以cbind(a,b)~變數???式寫法??????以list???????????????測值??????數
#x<-aggregate(cbind(SL,FL,LP)~Var+Bac+Block,data=sp1,mean)
#y<-aggregate(cbind(SL,FL,LP)~Var+Bac+Block,data=sp1,sd,na.rm=T)
#z<-aggregate(cbind(SL,FL,LP)~Var+Bac+Block,data=sp1,se)
#name<-c("Var","Bac","Block","SL_mean","FL_mean","LP_mean","SL_sd","FL_sd"#,"LP_sd","SL_se","FL_se","LP_se")#
#out<-data.frame(x,y[,4:6],z[,4:6])
#names(out)<-name
#head(out)
#out %>% write.csv(paste0("D:/summary",format(Sys.time(), "%d-%b-%Y %H%M%S"),".csv"))
#變方???質???測???
#1. KS.test:檢???兩?????????是???來自???????????????
#   如???輸???"pnorm"就是檢???該??????是???符???常?????????(Ho: a=常?????????)
#   p<0.001 -> 該數不屬???常?????????
head(sp2)
ggqqplot(sp3[,6])
plot(ggqqplot(sp3$實???數))
ks.test(x=sp2$??????,"pnorm")
ks.test(x=sp2$小???,"pnorm")
ks.test(x=sp3$實??????,"pnorm")
x<-ks.test(x=sp1$FL,"pnorm")
capture.output(x)

#2. qqplot，ggqqplot???qqnorm???可
ggqqplot(sp2$??????)
qqnorm(sp1$SL)
head(sp4)
#3.  	Shapiro-Wilk normality test，P<0.05顯?????????符常???
shapiro.test(sp3[,6])
head(sp4)
ggqqplot(sp4, "score", ggtheme = theme_bw()) +
facet_grid(time ~ group)
#4.  Levene'test
#檢???兩組數植??????異???????????????
leveneTest(SL~Var,sp1)


##????????????(?????????)
#method=spearman ??????母數統??????本???皮???森相??????數
#???為??????森相??????數???要兩??????數符???常?????????
#如???符???常??????可以??????面method??????
x=sp1$SL
y=sp1$FL
cor(x,y)
cor.test(x,y,method='spearman')
summary(cor.test(x,y,method='spearman'))


x<-lm(??????~???種+???+???+??????,data=sp2)
summary(aov(??????~???種+???+???+??????,data=sp2))
summary(aov(穗數~???種+???+???+??????,data=sp3))
summary(aov(總???~???種+???+???+??????,data=sp3))
summary(aov(稔實???~???種+???+???+??????+???*??????*???*???種*???,data=sp3))
summary(aov(一穗???數~???種+???+???+??????,data=sp3))
summary(aov(?????????~???種+???+???+??????,data=sp3))

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


##???母數??????KS檢???單??????
k1 = capture.output ( kruskal.test ( SL ~ Var , data = sp1))
k2 <- capture.output ( kruskal.test ( SL ~ Bac , data = sp1))

#???母數??????wilcox??????檢??????pairwise
#Bac???SL對Var???兩???比??? (???用split將???數???????????????list,???用lapply對???個???數??????檢???)
#Var???SL對Bac???兩???比???
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





#friedman.test不接??????????????????平???
x<-dataSum( list(sp1$FL,sp1$SL,sp1$LP), list(sp1$Var,sp1$Bac,sp1$Plant) )
head(x)
boxplot(SL ~ Var, data = bbbbb, xlab = "???種", ylab = "??????長", main = "BB")
boxplot(FL ~ Var, data = x, xlab = "???種", ylab = "??????長", main = "BB")
friedman.test ( SL ~ Var | Block , data = x)
edit(sp1)

######t-test######
?t.test
head(sp1)
t.test( lod~Var,data=sp1,
        alternative = c("two.sided", "less", "greater"),
        mu = 0, paired = FALSE, var.equal = TRUE,
        conf.level = 0.95)

