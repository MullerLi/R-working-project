#=========================讀取套件=======================
upgrade(packageStatus())
packages <- c("lme4","lmerTest" , "reshape2", "agricolae","car", "multcomp",  "rcompanion", "ggpubr", "carData","corrplot", "FSA", "dslabs","funModeling","corrplot","janitor", "tidyverse")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))
#========1.氣象資料爬蟲=======
{
packages <- c("lubridate","jsonlite", "rvest", "magrittr")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# 載入所需套件
lapply(packages, library, character.only = TRUE) %>%
  invisible()
url_start <- "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=467660&stname=%25E8%2587%25BA%25E6%259D%25B1&datepicker="


start_date <- as_Date("2000-01")
end_date <- as.Date("2020-12")
end_date-start_date

data1 <- data.frame()
data2 <- data.frame()
for(d in c(0:(end_date - start_date))){
  
  url <- paste0(url_start,as.character(start_date + d),"#")
  
  # 時間資料
  time_H <- url %>% 
    read_html() %>%
    html_nodes(xpath='//*[(@id = "MyTable")]//td[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]') %>% 
    html_text(trim	= T) %>% 
    as.numeric()
  
  # 降雨資料
  population <- url %>%
    read_html() %>%
    html_nodes(xpath='//td[(((count(preceding-sibling::*) + 1) = 11) and parent::*)]') %>%
    html_text(trim	= T) %>% 
    gsub("X","-9999", .) %>% 
    as.numeric()
  
  data1 <- cbind(as.character(start_date + d), time_H, population)
  data2 <- rbind(data2, data1)
  write.csv(data1,file = paste0("wea", start_date + d, ".csv"))
}
write.csv(data2,file = paste0("wea.csv"))
}

#=========================1.A合併氣象資料=======================

list.files(path="C:/Users/612/Desktop/氣象資料", full.names = TRUE) %>%
  lapply(read_csv) %>% 
  bind_rows() %>% 
  write.csv("wea.csv")

#=========================1.B氣象資料分析=======================
library(dplyr)
wea<-read.csv("D:/OneDrive/TTD-CHL/Rworking/Dataset/weather.csv") %>% as_tibble
wea
wea$year<-as.factor(wea$year)
wea$month<-as.factor(wea$month)
wea$season<-as.factor(wea$season)
wea %>% select(-year_mon) %>% 
  mutate(y_m=paste0(year,"_",month))->wea2

summary(wea2)
#======= 計算逐月積溫=====
# 用YxWAC.csv
library(dplyr)
YxW<-read_csv("D:/Onedrive/TTD-CHL/Rworking/DataSet/YxW.csv") %>% as_tibble
wea<-read.csv("D:/OneDrive/TTD-CHL/Rworking/Dataset/weather.csv") %>% as_tibble
names(YxW)<-c("year","season","variety","rep","Env","SP","GN","FR","TW","yield","Tem","TMax","TMin","Precp","SunHr","GloblRad","CloudAmount","TemMm","ACtem")
library(tidyverse)
# 用迴圈合併data
for ( i in 1:12){
  z<-wea %>% 
    mutate("ACtem"=(T.Max-T.Min)/2-10 ) %>% 
    relocate(Env,year,month,year_mon,ACtem,Tem:`TemM.m`) %>% 
    select((1:5)) %>% 
    filter(month == i) %>% 
    mutate("year"=as.factor(year))
  names(z) <- c("Env","year","month","y_m",paste0("ACtem_",i))
  
  YxW<-YxW %>% 
    mutate("year"=as.factor(year)) %>% 
    right_join(z,by="year")}


# 合併後整理並去掉不要的資料
YxW %>% 
  relocate(year,season,variety,rep,Env.x,SP,GN,FR,TW,yield,Tem,TMax,TMin,Precp,SunHr,GloblRad,CloudAmount,ACtem_1,ACtem_2,ACtem_3,ACtem_4,ACtem_5,ACtem_6,ACtem_7,ACtem_8,ACtem_9,ACtem_10,ACtem_11,ACtem_12) %>% 
  dplyr::select( -(30:115) ) -> YxW_AC

YxW_AC %>% 
  mutate("variety"=as.factor(variety),"season"=as.factor(season),"rep"=as.factor(rep),"Env.x"=as.factor(Env.x) )

se<-function(x){sd(x)/sqrt(length(x))}

unique(wea$year)
wea$year<-as.factor(wea$year)
wea$month<-as.factor(wea$month)
wea$season<-as.factor(wea$season)
wea %>% 
  filter(year==(2000:2010) , season!=0 ) %>% 
  group_by(month) %>% 
  summarise(mean(ACtem))

wea %>% 
  filter(year==(2000:2010) , season!=0 )

#=========================1.C十年敘述統計=======================

wea<-read.csv("D:/OneDrive/TTD-CHL/Rworking/Dataset/weather.csv") %>% as_tibble

wea %>% 
  dplyr::select(-(T.Max.Time:T.Min.Time),-TemM.m) %>% 
  mutate(ACtem=((T.Max+T.Min)/2)-10)->wea3
wea3 %>% 
  pivot_wider(values_from = ACtem,names_from = month,names_glue = "ACtem_{month}"  )->wea3

wea3 %>% view
# 挑選生長季 season!=0
# 挑選11-20 00-10兩時段
wea3 %>% filter(season!=0) %>% 
  filter(year %in% c(2011:2020)) -> wea_season11_20 
#2011-2020生長季天氣

wea3 %>% filter(season!=0) %>% 
  filter(year %in% c(2000:2010)) -> wea_season00_10 
#2000-2010生長季天氣

#2011-2020
weaSts1<-aggregate(cbind("Tem"=wea_season00_10$Tem,"MTem"=wea_season00_10$T.Max,"mTem"=wea_season00_10$T.Min,"Precp"=wea_season00_10$Precp,"Sun"=wea_season00_10$SunHr,"GloblRad"=wea_season00_10$GloblRad,"Cloud"=wea_season00_10$Cloud.Amount,"ACtem"=wea_season00_10$ACtem)~wea_season00_10$season,FUN= function(x) c("mean" = mean(x), "median" = median(x),"SD"=sd(x), "SE"=se(x),N=length(x),"CV"=cv <- sd(x) / mean(x)))
# 2000-2010
weaSts2<-aggregate(cbind("Tem"=wea_season00_10$Tem,"MTem"=wea_season00_10$T.Max,"mTem"=wea_season00_10$T.Min,"Precp"=wea_season00_10$Precp,"Sun"=wea_season00_10$SunHr,"GloblRad"=wea_season00_10$GloblRad,"Cloud"=wea_season00_10$Cloud.Amount,"ACtem"=wea_season00_10$ACtem)~wea_season00_10$season,FUN= function(x) c("mean" = mean(x), "median" = median(x),"SD"=sd(x), "SE"=se(x),N=length(x),"CV"=cv <- sd(x) / mean(x)))
write.csv(weaSts2,"2000-2010_wea_stat.csv")
write.csv(weaSts1,"2011-2020_wea_stat.csv")

#=========================1.D氣候作圖=======================
library(ggpubr)
library(dplyr)
wea_season11_20 %>% 
  mutate( "year"=as.factor(year) ,"season"=as.factor(season), "month"=as.factor(month) , year_mon = as.factor(year_mon)  ) ->wea_season11_20 

wea_season00_10 %>% 
  mutate( "year"=as.factor(year) ,"season"=as.factor(season), "month"=as.factor(month) , year_mon = as.factor(year_mon)  ) ->wea_season00_10 

# 積溫
plot1<-
  ggplot(data=wea_season00_10,mapping = aes(x= month,y=ACtem))+
  geom_boxplot(aes(fill=season))+
  labs(title = "Accumulated temperature during 2000-2010")+
  xlab("Month")+
  ylab("Accumulated temperature (℃)")+
  ylim(10,21)

plot2<-
  ggplot(data=wea_season11_20,mapping = aes(x=  month,y=ACtem))+
  geom_boxplot(aes(fill=season))+
  labs(title = "Accumulated temperature during 2011-2020")+
  xlab("Month")+
  ylab("Accumulated temperature (℃)")+
  ylim(10,21)

allplot<-ggpubr::ggarrange(plot1,plot2)
allplot
print(allplot)
ggsave(filename="ACtem.png",width = 11,height = 7)

# 雨量
precp_plot1<-ggplot(wea_season00_10,mapping=aes(x=month,y=Precp))+
  geom_boxplot(aes(fill=season))+
  labs(title = "Precption during 2000-2010")+
  xlab("Month")+
  ylab("Precipitation (mm)")+
  ylim(0,1000)

precp_plot2<-ggplot(wea_season11_20, mapping = aes(x= month,y= Precp))+
  geom_boxplot(aes(fill=season))+
  labs(title = "Precption during 2011-2020")+
  xlab("Month")+
  ylab("Precipitation (mm)")+
  ylim(0,1000)

ggarrange(precp_plot1,precp_plot2)
ggsave(filename="Precption.png",width = 11,height = 7)

# 光照時間
sun_plot1<-ggplot(wea_season00_10,mapping=aes(x=month,y=SunHr))+
  geom_boxplot(aes(fill=season))+
  labs(title = "Sun hour during 2000-2010")+
  xlab("Month")+
  ylab("Hours")+
  ylim(50,350)
sun_plot2<-ggplot(wea_season11_20,mapping=aes(x=month,y=SunHr))+
  geom_boxplot(aes(fill=season))+
  labs(title = "Sun hour during 2000-2010")+
  xlab("Month")+
  ylab("Hours")+
  ylim(50,350)

ggarrange(sun_plot1,sun_plot2)
ggsave(filename="SunHr.png",width = 11,height = 7)

# 輻射量
sung_plot1<-ggplot(wea_season00_10,mapping=aes(x=month,y=GloblRad))+
  geom_boxplot(aes(fill=season))+
  labs(title = "Radiation during 2000-2010")+
  xlab("Month")+
  ylab("Global radiation (MJ/㎡)")+
  ylim(200,750)
sung_plot2<-ggplot(wea_season11_20,mapping=aes(x=month,y=GloblRad))+
  geom_boxplot(aes(fill=season))+
  labs(title = "Radiation during 2011-20")+
  xlab("Month")+
  ylab("Global radiation (MJ/㎡)")+
  ylim(200,750)

ggarrange(sung_plot1,sung_plot2)
ggsave(filename="Sung.png",width = 11,height = 7)


# 最高溫與最低溫
Tem_plot1<-ggplot(wea_season00_10)+
  geom_pointrange(mapping=aes(x=month,y=Tem,ymin=T.Min,ymax=T.Max,color=season))+
  labs(title = "Temperature during 2000-2010")+
  xlab("Month")+
  ylab("Temperature of month (℃)")+
  ylim(10,40)
Tem_plot2<-ggplot(wea_season11_20)+
  geom_boxplot(mapping=aes(x=month,y=Tem ,color=season))+
  geom_linerange(mapping=aes(x=month,ymin=T.Min,ymax=T.Max,color=season))+
  labs(title = "Temperature during 2011-2020")+
  xlab("Month")+
  ylab("Temperature of month (℃)")+
  ylim(10,40)

Tem_plot1<-ggplot(wea_season00_10)+
  geom_boxplot(mapping=aes(x=month,y=Tem ,color=season))+
  geom_linerange(mapping=aes(x=month,ymin=T.Min,ymax=T.Max,color=season))+
  labs(title = "Temperature during 2000-2010")+
  xlab("Month")+
  ylab("Temperature of month (℃)")+
  ylim(10,40)




ggarrange(Tem_plot1,Tem_plot2)
ggsave(filename="Sung.png",width = 11,height = 7)
#Plot以1100x700為主
#=========================2.綜合變方=======================
#=======2.A資料整理=======

YCmean<-aggregate(cbind("SP"=YC$SP,"TW"=YC$TW,"EN"=YC$EN,"GN"=YC$GN ,"TN"=YC$TN , "GW"=YC$GW, "SGN"=YC$SGN, "FR"=YC$FR,"TW2"=YC$TW2)  ~YC$rep+YC$variety+YC$season+YC$year, FUN =mean )
YCmean %>% write.csv("YCmean.csv")
yc$year<-as.factor(yc$year)
yc$season<-as.factor(yc$season)
yc$variety<-as.factor(yc$variety)
yc$rep<-as.factor(yc$rep)
yc$env<-as.factor(yc$env)

#=======2.B綜合辯方=======
canovaF<-function(da,ob){
  attach(da)
# Table 2
  aov(ob~env+variety+Error(rep:env+env:variety),da) %>% summary() %>% capture.output() -> out1
# F value for env and variety
  aov(ob~rep:env+variety*env,da) %>% anova()->out2
# F value for block:env and env:line
  out3 = aov(ob~env*variety,da) %>% anova() ->out3
# F value for env:line
  name<-substitute(ob)
  capture.output(c(out1,out2,out3)) %>% write.csv(paste0("Fvalue",name,".csv"))
  plot<-ggplot(data=da,mapping=aes (x= variety,y = ob))+
  geom_boxplot(data=da, mapping = aes(fill=season,linetype=season))+
  xlab("variety")+
  ylab(paste0(name))
  print(plot)
  dev.off()
  return(plot)
  detach(da)}

# Table 2
summary(aov(yield~env+variety+Error(rep:env+env:variety),yc))
# F value for env and variety
aov(yield~rep:env+variety*env,yc) %>% anova()
# F value for block(rep:env) and env:line and residuals
aov(yield~env*variety,yc) %>% anova() 
# F value for env:line 
# 觀測值可替換為每株穗數 yield 千粒重 每穗粒數
# 可手算p值，或從上方三個aov中提取P值
pf(2.4132 , 50, 104, lower.tail = FALSE)


# ========以下不用=====
# Make Plot: variable ~ year with facet_wrap=season, 點圖與平滑線圖
# colNam<-yyc[5:14]
PicDraw<-function(yvar){
  ggplot(data=yyc,mapping=aes(x=year,y=yvar,shape=variety,color=variety))+
  geom_point(size=2,show.legend = FALSE)+
  geom_smooth( aes(group=""),show.legend = FALSE)+
  facet_wrap(~season,nrow = 2) }

Picset<-sapply(yyc[5:14],simplify=FALSE, function(x) PicDraw(x))

figure <- ggarrange(plotlist =Picset, ncol = 5, nrow = 2)
print(figure)
for ( i in 5:14){
pic<-ggplot(data=yyc, mapping = aes( x= year, y=yyc[i], shape = variety, color=variety,labs(x='Year',y=paste0(colnames(yyc[i])))))+geom_point()}
colNam<-colnames(yyc)[5:14]
Picset<-lapply( yyc[,5:14], function(yvar){
  ggplot(data=yyc,mapping = aes(x = year,y = yvar ,shape=variety,color=variety))+
    geom_point(size=5)+
    geom_smooth( aes(group = "" ),show.legend = FALSE)+
    facet_wrap(~season,nrow = 2)+
    labs(x = 'Year', y = paste0(colNam[5]))})

Picset<-sapply( yyc[,5:14], function(yvar) ggplot(data=yyc,mapping=aes(x=year,y=yvar,shape=variety,color=variety))+
    geom_point(size=0.5,show.legend = FALSE)+
    geom_smooth(aes(group=""),show.legend=FALSE)+
    facet_wrap(~season,nrow = 2))

colNam<-colnames(yyc)[5:14]

figure <- ggarrange(plotlist =Picset,ncol = 5, nrow = 2,labels = c("(A)","(B)","(C)","(D)","(E)","(F)","(G)","(H)","(I)","(J)"),  font.label = list(size = 12, color = "black"))


Picset<-sapply(yyc[5:14],simplify = FALSE,function(yvar) {
  ggplot(data=yyc,mapping=aes(x=year,y=yvar,shape=variety,color=variety,labs(x= 'year', y = 'xlabs')))+
    geom_point(size=0.5,show.legend = FALSE)+
    geom_smooth(aes(group=""),show.legend=FALSE)+
    facet_wrap(~season,nrow = 2)+
    ylab(xlabs) +
    xlab('year')
  })

# 常態性檢定

for (i in 5:14){
  shapiro.test(yyc[,i])
        if (shapiro.test(yyc[,i])$p < 0.01){
    print(paste0(colnames(yyc[i]),"(P=",shapiro.test(yyc[,i])$p,") is not Normaility."))
  }}


# 變數中文化
# yyc2<-yyc
# colnames(yyc2)<-c("year","season","env","variety","穗數","總重","空粒數","實粒數","總粒數","實粒重","一穗粒數","稔實率","ob","產量")
# ========以上不用=====

#=======3.A 相關分析與繪圖=======
YxW<-read_csv("D:/Onedrive/TTD-CHL/Rworking/DataSet/YxW.csv")
names(YxW)<-c("year","season","variety","rep","Env","SP","GN","FR","TW","yield","Tem","TMax","TMin","Precp","SunHr","GloblRad","CloudAmount","TemMm","ACtem")
YxW_AC<-read_csv("D:/Onedrive/TTD-CHL/Rworking/DataSet/YxW逐月積溫.csv")
YxW_AC %>% dplyr::select(-X1) %>% 
  mutate("year"=as.factor(year),"season"=as.factor(season),variety=as.factor(variety),rep=as.factor(rep)) -> YxW_AC

YxW_AC %>% filter(variety=="TK2") -> YxW_TK2
YxW_AC %>% filter(variety=="TNG67") -> YxW_TNG67
YxW_AC %>% filter(variety=="TT30") -> YxW_TT30

YxW_TK2 %>% filter(season=="1") -> YxW_TK2_1
YxW_TNG67 %>% filter(season=="1") -> YxW_TNG67_1
YxW_TT30 %>% filter(season=="1") -> YxW_TT30_1

YxW_TK2 %>% filter(season=="2") -> YxW_TK2_2
YxW_TNG67 %>% filter(season=="2") -> YxW_TNG67_2
YxW_TT30 %>% filter(season=="2") -> YxW_TT30_2
# set factor
library(Hmisc)
library(ggcorrplot)
# ======== Normaility test via shapiro test=======
Nortest<-function(da){
  NumLength<-length(colnames(select_if(as.data.frame(da),is.numeric)))
  for (i in 4:NumLength){
  da_n<-select_if(da,is.numeric) %>% as.data.frame
  da_st<-shapiro.test(da_n[,i])
  if (da_st$p.value<0.05)
    {
    print(paste0(substitute(da),"-",colnames(da_n[i]),"_非常態性"))}
}
}

sink(file="nor.txt")
cat(
Nortest(YxW_TK2_1) ,
Nortest(YxW_TK2_2) ,
Nortest(YxW_TNG67_1), 
Nortest(YxW_TNG67_2) ,
Nortest(YxW_TT30_1) ,
Nortest(YxW_TT30_2) ,
)
sink()



#========= 計算相關矩陣,P並印出 ==========
library(dplyr)
library(tidyverse)
YxW_AC %>% write.csv("YxWAC.csv")
rm(da)
da=YxW_AC
YxW_AC %>% filter(variety=="TK2") -> YxW_TK2
YxW_AC %>% filter(variety=="TNG67") -> YxW_TNG67
YxW_AC %>% filter(variety=="TT30") -> YxW_TT30

YxW_TK2 %>% filter(season=="1") -> YxW_TK2_1
YxW_TNG67 %>% filter(season=="1") -> YxW_TNG67_1
YxW_TT30 %>% filter(season=="1") -> YxW_TT30_1

YxW_TK2 %>% filter(season=="2") -> YxW_TK2_2
YxW_TNG67 %>% filter(season=="2") -> YxW_TNG67_2
YxW_TT30 %>% filter(season=="2") -> YxW_TT30_2
# 注意select和合併資料code要隨不同資料樣態調整
cor_analysis<-function(da){
  da %>% dplyr::select(-(year:Env.x))->da  #注意，調整選取的變數
  da_numeric<-select_if(as.data.frame(da), is.numeric)
  cor_numVar1 <- cor(da_numeric, method="pearson", use="pairwise.complete.obs")
  cor_numVar2 <- cor(da_numeric, method="spearman", use="pairwise.complete.obs")
  res1 <- rcorr(as.matrix(da_numeric),type="pearson") #P-value
  res1$type="pearson"
  res2 <- rcorr(as.matrix(da_numeric),type="spearman") #P-value
  res2$type="spearman"
  file_name<-substitute(da)
  cor_type<-row.names(cor_numVar1)

    CodeNum<-dim(cor_numVar1)[1]
    as_tibble(cor_numVar1) %>% 
    mutate("type"=cor_type,"code"=1:CodeNum    #注意這裡
           ,"method"=rep("pearson",time=CodeNum)) %>% 
    relocate(method,type) %>% 
    as.data.frame()->cor_1
  
  as_tibble(cor_numVar2) %>% 
    mutate("type"=cor_type,"code"=1:CodeNum    #注意這裡
           ,"method"=rep("spearman",time=CodeNum)) %>%  
    relocate(method,type) %>% 
    as.data.frame()->cor_2
  
  as_tibble(res1$P) %>% 
    mutate("type"=row.names(res2$P),"cor"="pearson_P","code"=1:24) %>% 
    relocate(cor,type) ->cor_p1
  
  as_tibble(res2$P) %>% 
    mutate("type"=row.names(res2$P),"cor"="spearman_P","code"=1:24) %>% 
    relocate(cor,type) ->cor_p2
  cor_1 %>% 
    left_join(cor_p1,by="code") %>% 
    left_join(cor_2,by="code") %>% 
    left_join(cor_p2,by="code") %>% return
} 


cor_analysis(YxW_TK2_1) %>% write_csv(file = paste0("YxW_TK2_1cor.csv"))
cor_analysis(YxW_TK2_2) %>% write_csv(file = paste0("YxW_TK2_2cor.csv"))
cor_analysis(YxW_TNG67_1) %>% write_csv(file = paste0("YxW_TNG67_1cor.csv"))
cor_analysis(YxW_TNG67_2) %>% write_csv(file = paste0("YxW_TNG67_2cor.csv"))
cor_analysis(YxW_TT30_1) %>% write_csv(file = paste0("YxW_TT30_1cor.csv"))
cor_analysis(YxW_TT30_2) %>% write_csv(file = paste0("YxW_TT30_2cor.csv"))

# 相關矩陣計算(pearson和spearman)、利用ggcorrplot繪圖，並輸出Tiff
# 注意"檔名"是從data.frame cell位置抓的，不同資料樣態程式碼要調整
# 同時輸出spearman跟pearson相關的結果
# 刪掉自己加上的溫差跟總雨量總日照變數
cor_draw<-function(da){
  sea<-da[1,2]　#季節
  var<-da[1,3]  #品種
  file.name<-paste0(var,"_s",sea) #檔名
  da_numeric<-select_if(as.data.frame(da), is.numeric)
  names(da_numeric)<-c("SP","SGN","FR","TW","Y","Tem","T.Max","T.Min","Precp","SunHr","GloblRad","Cloud","TemM.m")
  da_numeric$TemM.m<-NULL
  cor_numVar_P <- cor(da_numeric, use="pairwise.complete.obs",
                      method= "pearson")
  cor_numVar_S <- cor(da_numeric, use="pairwise.complete.obs",
                      method= "spearman")
  
  cor_numVar_P[order(cor_numVar_P[,5],decreasing=TRUE),]
  cor_numVar_P
  plot_P<-ggcorrplot(cor_numVar_P,
             type = "lower",
             insig = "blank",
             lab = TRUE, 
             digits = 2) #位數
  plot_s<-ggcorrplot(cor_numVar_S,
                     type = "lower",
                     insig = "blank",
                     lab = TRUE, 
                     digits = 2) #位數
  tiff(paste0(file.name,"_pearson.tiff"),width=2000,height=2000,res=300)
  print(plot_P)
  dev.off()
  tiff(paste0(file.name,"_spearman.tiff"),width=2000,height=2000,res=300)
  print(plot_s)
  dev.off()
  }

# lapply
data2<-list(TK2_s1,TK2_s2,TT30_s1,TT30_s2,TNG67_s1,TNG67_s2)
lapply(data2,cor_draw)

#====== 4.A 資料建模======
unique(TK2_wea$year)
TK2_wea$總日射量<-NULL
TT30_wea$總日射量<-NULL
TNG67_wea$總日射量<-NULL

# 注意!模式p值的函數 跟 formula我直接寫在函數裡面!
# 命名也要小心!
YxW_TK2_1 %>% str
as.data.frame(YxW_TK2_1) %>% colnames
MoDe<-function(dat,yvar,xvar){
  filename<-substitute(dat)
  season<-dat[10,2]
  vaR<-dat[3,3]
  datanames<-paste0(filename,"_",season,"_",vaR)
  datanames %>% print
  #data modeling
  FM<-as.formula(yvar~Tem+TMax+TMin+Precp+SunHr+CloudAmount+GloblRad+TemMm+ACtem)
  model1<-lm(FM,data=dat)
  step_Mod<-stepAIC(model1, direction = "both",trace=FALSE)
  #drawing
  #plot<-ggplot(data = dat, aes(y = yvar, x = xvar)) + 
   # geom_point(color='blue') +
    #geom_smooth(method="lm")
  #print(plot)
  # 計算模型總P值
  lmp <- function (modelobject) {
    if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
  }
  model_P<-lmp(step_Mod)
  #計算模型的係數coeffecient p值 調整R並輸出
  lmsum<-summary(step_Mod)
  adjR<-lmsum$adj.r.squared
  Mod_Co<-lmsum$coefficients
  print(summary(step_Mod))
  print(model_P)
  out<-list("df"=datanames,"MODEL_P"=model_P,"adj.R"= adjR , Mod_Co)
  capture.output(out) %>% 
    write.csv(file=
      paste0("model",datanames,".csv")
    )
}

#run each model(太長先隱藏)
MoDe(YxW_TK2_1,YxW_TK2_1$SP,YxW_TK2_1$ACtem,"每株穗數")
MoDe(YxW_TK2_1,YxW_TK2_1$GN,YxW_TK2_1$ACtem,"每穗粒數")
MoDe(YxW_TK2_1,YxW_TK2_1$FR,YxW_TK2_1$ACtem,"稔實率")
MoDe(YxW_TK2_1,YxW_TK2_1$TW,YxW_TK2_1$ACtem,"千粒重")
MoDe(YxW_TK2_1,YxW_TK2_1$yield,YxW_TK2_1$ACtem,"yield")

MoDe(YxW_TK2_2,YxW_TK2_2$SP,YxW_TK2_2$ACtem,"每株穗數")
MoDe(YxW_TK2_2,YxW_TK2_2$GN,YxW_TK2_2$ACtem,"每穗粒數")
MoDe(YxW_TK2_2,YxW_TK2_2$FR,YxW_TK2_2$ACtem,"稔實率")
MoDe(YxW_TK2_2,YxW_TK2_2$TW,YxW_TK2_2$ACtem,"千粒重")
MoDe(YxW_TK2_2,YxW_TK2_2$yield,YxW_TK2_2$ACtem,"yield")

MoDe(YxW_TNG67_1,YxW_TNG67_1$SP,YxW_TNG67_1$ACtem,"每株穗數")
MoDe(YxW_TNG67_1,YxW_TNG67_1$GN,YxW_TNG67_1$ACtem,"每穗粒數")
MoDe(YxW_TNG67_1,YxW_TNG67_1$FR,YxW_TNG67_1$ACtem,"稔實率")
MoDe(YxW_TNG67_1,YxW_TNG67_1$TW,YxW_TNG67_1$ACtem,"千粒重")
MoDe(YxW_TNG67_1,YxW_TNG67_1$yield,YxW_TNG67_1$ACtem,"yield")

MoDe(YxW_TNG67_2,YxW_TNG67_2$SP,YxW_TNG67_2$ACtem,"每株穗數")
MoDe(YxW_TNG67_2,YxW_TNG67_2$GN,YxW_TNG67_2$ACtem,"每穗粒數")
MoDe(YxW_TNG67_2,YxW_TNG67_2$FR,YxW_TNG67_2$ACtem,"稔實率")
MoDe(YxW_TNG67_2,YxW_TNG67_2$TW,YxW_TNG67_2$ACtem,"千粒重")
MoDe(YxW_TNG67_2,YxW_TNG67_2$yield,YxW_TNG67_2$ACtem,"yield")

MoDe(YxW_TT30_1,YxW_TT30_1$SP,YxW_TT30_1$ACtem,"每株穗數")
MoDe(YxW_TT30_1,YxW_TT30_1$GN,YxW_TT30_1$ACtem,"每穗粒數")
MoDe(YxW_TT30_1,YxW_TT30_1$FR,YxW_TT30_1$ACtem,"稔實率")
MoDe(YxW_TT30_1,YxW_TT30_1$TW,YxW_TT30_1$ACtem,"千粒重")
MoDe(YxW_TT30_1,YxW_TT30_1$yield,YxW_TT30_1$ACtem,"yield")

MoDe(YxW_TT30_2,YxW_TT30_2$SP,YxW_TT30_2$ACtem,"每株穗數")
MoDe(YxW_TT30_2,YxW_TT30_2$GN,YxW_TT30_2$ACtem,"每穗粒數")
MoDe(YxW_TT30_2,YxW_TT30_2$FR,YxW_TT30_2$ACtem,"稔實率")
MoDe(YxW_TT30_2,YxW_TT30_2$TW,YxW_TT30_2$ACtem,"千粒重")
MoDe(YxW_TT30_2,YxW_TT30_2$yield,YxW_TT30_2$ACtem,"yield")


#data combination and output
list.files("D:/OneDrive/TTD-CHL/Rworking/abc",full.names=TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows() %>% 
  write.csv("wea.csv",fileEncoding = "ASCII")

#load car package
library(car)

#produce added variable plots
str(TK2_s1)
lm(yield~T.Max+T.Min+Cloud.Amount+GloblRad,data=TK2_s1)

avPlots(lm(yield~T.Max+T.Min+Cloud.Amount+GloblRad,data=TK2_s1))

ggplot(data=TK2_s1,aes(y=yield,x=GloblRad))+
  geom_smooth(method="lm")+
  geom_point()



## ============== GGE biplot
install.packages("agridat")
install.packages("gge")
library(agridat)
data(yan.winterwheat)
edit(dat1 <- yan.winterwheat)
library(gge)
m1 <- gge(dat1, yield~gen*env, scale=FALSE)
biplot(m1, main="yan.winterwheat - GGE biplot",
       flip=c(1,0), origin=0, hull=TRUE)
