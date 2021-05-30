rm(list=ls())
#分析流程
#敘述統計，將各個觀測值大於０與小於０的比值列出，如香：不相＝7:6，代表此品種比對照品種來的香
#無母數分析one-way anova－kruskal.wallis test，以品種為rank，對個觀測值進行kruskal.test，P value代表如果品種間無差異的第一型錯誤率，若不顯著，代表各品種與對照品種無顯著差異
#無母數分析RCBD－Friedman's test，以品種為處理，批次與品評人員為區集，對個觀測值進行test
#無母數分析RCBD+缺值-Skillings.Mack test：若資料有缺值，Friedman's test改執行Skillings.Mack test
#上面三項分析有顯著差異者，再以wilcoxon-test進行兩兩比較


library(magrittr) 
install.packages("magrittr")

head ( tas <- read.csv ( "D:/b.csv" , header = T , sep = "," ))


#將資料根據要檢定的類別排序 (這邊是品種)
levels(tas$Var)
tas$Var <- ordered ( tas$Var, levels = c("PAM3", "PAM2", "PAM1") )
head(tas)
#怪怪ㄉ


#kruskal.wallis test檢定各品種的觀測值間是否有顯著差異

k1 = capture.output ( kruskal.test ( Oa ~ Peo , data = tas))
k2 = capture.output ( kruskal.test ( App ~ Peo , data = tas))

timetime<-format(Sys.time(), "%F_%H%M%S")

write.csv ( c(k1,k2), file = "D:/KWTESTnoblock.csv")

#如果有顯著，則執行ｗｉｌｃｏｘ　ｔ　ｔｅｓｔ，倆倆比較
w1 <- read.csv ( "D:/C1.csv" , header = T , sep = "," )
w2 <- read.csv ( "D:/C2.csv" , header = T , sep = "," )
w3 <- read.csv ( "D:/C3.csv" , header = T , sep = "," )

wr1<-wilcox.test ( App ~ Var , data = w1)
wr2<-wilcox.test ( App ~ Var , data = w2)
wr3<-wilcox.test ( App ~ Var , data = w3)

?wilcox.test

##如果要指定區集，則使用Friedman test
install.packages(c("tidyverse","ggpubr","rstatix"))
library(tidyverse)
library(ggpubr)
library(rstatix) #friedman_test()函數在此packages
?skillingsMackTest

friedman.test ( App ~ Var | Peo , data = tas)


##如果區集不完全或是有缺直，改使用"Skillings.Mack"　TEST
install.packages("Skillings.Mack")
library (Skillings.Mack)
?Ski.Mack
head(tas)
##ski.mack函數不接受有重複的資料，所以重複的資料要先取平均
#再outpt
tas <- read.csv(file="D:/C.csv",header=T,sep=",")
tas2<- aggregate ( App ~ Var+Peo , tas, mean )
out<-capture.output( Ski.Mack ( tas2$App , groups = tas2$Var , blocks = tas2$Peo)) 
write.csv(out,paste0("D:/silmack", format(Sys.time(), "%d-%b-%Y %H%M%S"), ".csv"))

tas2 <- aggregate ( Aro ~ Var+Peo , tas, mean )
out<-capture.output( Ski.Mack ( tas2$Aro , groups = tas2$Var , blocks = tas2$Peo)) 
write.csv(out,paste0("D:/silmack", format(Sys.time(), "%d-%b-%Y %H%M%S"), ".csv"))

tas2 = aggregate ( Tas ~ Var+Peo , tas, mean )
out<-capture.output( Ski.Mack ( tas2$Tas , groups = tas2$Var , blocks = tas2$Peo)) 
write.csv(out,paste0("D:/silmack", format(Sys.time(), "%d-%b-%Y %H%M%S"), ".csv"))

tas2 = aggregate ( Coh ~ Var+Peo , tas, mean )
out<-capture.output( Ski.Mack ( tas2$Coh , groups = tas2$Var , blocks = tas2$Peo)) 
write.csv(out,paste0("D:/silmack", format(Sys.time(), "%d-%b-%Y %H%M%S"), ".csv"))

tas2 = aggregate ( Har ~ Var+Peo , tas, mean )
out<-capture.output( Ski.Mack ( tas2$Har , groups = tas2$Var , blocks = tas2$Peo)) 
write.csv(out,paste0("D:/silmack", format(Sys.time(), "%d-%b-%Y %H%M%S"), ".csv"))

tas2 = aggregate ( Ove ~ Var+Peo , tas, mean )
out<-capture.output( Ski.Mack ( tas2$Ove , groups = tas2$Var , blocks = tas2$Peo)) 
write.csv(out,paste0("D:/silmack", format(Sys.time(), "%d-%b-%Y %H%M%S"), ".csv"))

write.csv(tas2,paste0("D:/silmack", format(Sys.time(), "%d-%b-%Y %H%M%S"), ".csv"))
?dunn.test

#如果有顯著，則執行ｗｉｌｃｏｘ　ｔ　ｔｅｓｔ，倆倆比較
foo <- pairwise.wilcox.test(Data$Value, Data$Group, p.adjust.method="none")

wx<-pairwise.wilcox.test(sp1$SL,sp1$Var,p.adjust.method = "none")
summary(wx)
wx_result<-capture.output(wx,split="TRUE")
write.table(wx_result, file = "foo2.csv", sep = " ", col.names = NA,           qmethod = "double")


library(dplyr)
separate(wx_result," ")
separate(wx_result," ")




write.xlsx(wx_result,paste0("D:/wilcon"  , format (Sys.time() , "%d-%b-%Y %H%M%S"), ".xlsx"))
write.csv(wx_result,paste0("D:/wilcon"  , format (Sys.time() , "%d-%b-%Y %H%M%S"), ".csv"))













w1 <- read.csv ( "D:/w12.csv" , header = T , sep = "," )
w2 <- read.csv ( "D:/w23.csv" , header = T , sep = "," )
w3 <- read.csv ( "D:/w13.csv" , header = T , sep = "," )

wr1<-wilcox.test ( App ~ Var , data = w1)
out<-capture.output(wr1)
write.csv(out,paste0("D:/wilcon"  , format (Sys.time() , "%d-%b-%Y %H%M%S"), ".csv"))

wr2<-wilcox.test ( App ~ Var , data = w2)
out<-capture.output(wr2)
write.csv(out,paste0("D:/wilcon"  , format (Sys.time() , "%d-%b-%Y %H%M%S"), ".csv"))

wr3<-wilcox.test ( App ~ Var , data = w3)
out<-capture.output(wr3)
write.csv(out,paste0("D:/wilcon"  , format (Sys.time() , "%d-%b-%Y %H%M%S"), ".csv"))

w1c<-read.csv("D:/w1c.csv")
wr1<-wilcox.test ( App ~ Var , data = w1c)
out<-capture.output(wr1)
write.csv(out,paste0("D:/wilcon"  , format (Sys.time() , "%d-%b-%Y %H%M%S"), ".csv"))

w2c<-read.csv("D:/w2c.csv")
wr2<-wilcox.test ( App ~ Var , data = w2c)
out<-capture.output(wr2)
write.csv(out,paste0("D:/wilcon"  , format (Sys.time() , "%d-%b-%Y %H%M%S"), ".csv"))

w3c<-read.csv("D:/w3c.csv")
wr3<-wilcox.test ( App ~ Var , data = w3c)
out<-capture.output(wr3)
write.csv(out,paste0("D:/wilcon"  , format (Sys.time() , "%d-%b-%Y %H%M%S"), ".csv"))

App	Aro	Tas	Coh	Har	Ove


data<-PAM2
CK <- read.csv( file  = "D:/PAM2", header = T , sep = ",")
re<-wilcox.test ( CK$App  , mu = 0 , data = CK, alternative = "two.sided", conf.level = 0.95)
out<-capture.output(re)
write.csv(out,paste0("D:/app.csv"))

CK <- read.csv( file  = "D:/PAM1CK2.csv", header = T , sep = ",")
re<-wilcox.test ( CK$Aro ,mu = 0 , data = CK)
out<-capture.output(re)
write.csv(out,paste0("D:/aro.csv"))

CK <- read.csv( file  = "D:/PAM1CK3.csv", header = T , sep = ",")
re<-wilcox.test ( CK$Tas , data = CK)
out<-capture.output(re)
write.csv(out,paste0("D:/tas.csv"))

CK <- read.csv( file  = "D:/PAM1CK4.csv", header = T , sep = ",")
re<-wilcox.test ( CK$Coh , data = CK)
out<-capture.output(re)
write.csv(out,paste0("D:/coh.csv"))

CK <- read.csv( file  = "D:/PAM1Ck5.csv", header = T , sep = ",")
re<-wilcox.test ( CK$Har , data = CK)
out<-capture.output(re)
write.csv(out,paste0("D:/har.csv"))

CK <- read.csv( file  = "D:/PAM1CK6.csv", header = T , sep = ",")
re<-wilcox.test ( CK$Ove , data = CK)
out<-capture.output(re)
write.csv(out,paste0("D:/ove.csv"))



data<-read.csv(file = "D:/PAM3.csv", sep = "," , header = T)

re<-wilcox.test ( data$App  , mu = 0 , data = data , alternative = "two.sided")
out<-capture.output(re)
write.csv(out,paste0("D:/app.csv"))

re<-wilcox.test ( data$Aro ,mu = 0 , data = data , alternative = "two.sided", conf.level = 0.95)
out<-capture.output(re)
write.csv(out,paste0("D:/aro.csv"))

re<-wilcox.test ( data$Tas , mu = 0 , data = data , alternative = "two.sided", conf.level = 0.95)
out<-capture.output(re)
write.csv(out,paste0("D:/tas.csv"))

re<-wilcox.test ( data$Coh , mu = 0 , data = data , alternative = "two.sided", conf.level = 0.95)
out<-capture.output(re)
write.csv(out,paste0("D:/coh.csv"))

re<-wilcox.test ( data$Har , mu = 0 , data = data , alternative = "two.sided", conf.level = 0.95)
out<-capture.output(re)
write.csv(out,paste0("D:/har.csv"))

re<-wilcox.test ( data$Ove , mu = 0 , data = data , alternative = "two.sided", conf.level = 0.95)
out<-capture.output(re)
write.csv(out,paste0("D:/ove.csv"))






