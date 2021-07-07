PH <- read_excel("DataSet/雅玲品種.xlsx",sheet="株高歲數")
SL <- read_excel("DataSet/雅玲品種.xlsx",sheet="穗長")
SEED <- read_excel("DataSet/雅玲品種.xlsx",sheet="考種")
Yield <- read_excel("DataSet/雅玲品種.xlsx",sheet="產量")
days <- read_excel("DataSet/雅玲品種.xlsx",sheet="生育")

se<-function(x){
  x<-x[complete.cases(x) ]
  sd(x)/sqrt(length(x))
}
str(PH)
cbind(株高,穗數...16)~品種...14+重複...17+期作...18

DataSum<-function(daTa,forMu,name){
  data<-aggregate(data=daTa, forMu ,FUN= function(x) c("mean" = mean(x), "median" = median(x),"SD"=sd(x), "SE"=se(x),N=length(x),"CV"=cv <- sd(x) / mean(x) ))
  write.csv(data,paste0("雅玲-",name,"敘述統計量.csv"))
}
str(Yield)

DataSum(PH,cbind(株高,穗數)~品種+期作,"株高")
DataSum(SL,cbind(穗長)~品種+期作,"穗長")
DataSum(SEED,cbind(穗數,總重,空粒數,實粒數,總粒數,實粒重,一穗粒數,稔實率,千粒重
)~品種+期作,"穗長")

DataSum(Yield,產量~品種+期作,"產量")


DataSum(SL,cbind(穗長)~品種+期作,"穗長")

data1<-aggregate(data=PH, 產量~品種...1+期作...4,FUN= function(x) c("mean" = mean(x), "median" = median(x),"SD"=sd(x), "SE"=se(x),N=length(x),"CV"=cv <- sd(x) / mean(x) ))

data2<-aggregate(data=MySheet, cbind(株高,穗數...16)~品種...14+重複...17+期作...18,FUN= function(x) c("mean" = mean(x), "median" = median(x),"SD"=sd(x), "SE"=se(x),N=length(x),"CV"=cv <- sd(x) / mean(x) ))

data3<-aggregate(data=MySheet, cbind(穗長)~品種...20+重複...22+期作...23,FUN= function(x) c("mean" = mean(x), "median" = median(x),"SD"=sd(x), "SE"=se(x),N=length(x),"CV"=cv <- sd(x) / mean(x) ))


str(MySheet)
data4<-aggregate(data=MySheet, cbind(穗數...26,總重,空粒數,實粒數,總粒數,實粒重,一穗粒數,稔實率,千粒重)~品種...25+as.character(期作...36),
                 FUN= function(x) c("mean" = mean(x), "median" = median(x),"SD"=sd(x), "SE"=se(x),N=length(x),"CV"=cv <- sd(x) / mean(x)))

data4 %>% write.csv("雅玲-考種.csv")
view(MySheet)
dataSum(MySheet,forMu,"考種")

edit(data2)