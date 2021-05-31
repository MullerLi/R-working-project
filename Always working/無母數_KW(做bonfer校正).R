rm(list=ls())
####Kruskal-Wallis test############相對應CRDesign(每處裡內個數可以不同)
##x: 觀測值  
##g: 相對應X的分群 (x和g長度相等)
##afa:顯著水準  一般0.05或0.1
##兩兩比較(Dunn) 顯著水準做bonfer.法校正
#output: KW test 顯著 1.KW統計量 df p.vale 
#                    2.兩兩比較表(列出dunn統計量 SE z 和是否顯著0或1)(放在d槽 KW_compared_table.csv)
#                    3.AB 圖(需整理)(放在d槽 KW_AB_table.csv)
#        KW test 不顯著 1.KW統計量 df p.vale 
#                     2.DO NOT significant! 
#                     3.處裡等級平均值由大到小排列

##########KW.test函數開始#################################

KW.test<-function(x,g,afa){
   g<-factor(g)
   t<-length(table(g)) #No. treatments
   n<-length(x)        #總個數
   ni<-table(g)        #各處理的個數
   
   R.mean<-tapply(rank(x),g,mean)
   R.mean.s<-sort(R.mean,decreasing=T)
   
   kruskal.test(x,g) #kruskal-wllis method
   if(kruskal.test(x,g)$p.value<afa){
      ni.s<-ni[order(R.mean,decreasing=T)] #處理由大到小排序
      
      x.ta<-table(rank(x))
      tie<-x.ta[x.ta>1]
      
      D<-c()
      d.ta<-c()
      se.ta<-c()
      na.s<-names(ni.s)    #處理名字由大到小排序
      na.com<-c()
      i=1
      j=9
      for(i in 1:t){       ####兩兩Dunn比較統計量和SE####
         for(j in i:t){
            if(i==j){
               next
            }
            se<-sqrt((n*(n+1)/12-sum(tie^3-tie)/(12*(n-1)))*((1/ni.s[i])+(1/ni.s[j])))
            se.ta<-c(se.ta,se)
            d<-abs(R.mean.s[i]-R.mean.s[j])
            d.ta<-c(d.ta,d)
            D<-c(D,d/se)
            temp3<-paste(na.s[i],"VS",na.s[j],sep=" ")
            na.com<-c(na.com,temp3)
         }
      }
      D<-round(D,4)
      d.ta<-round(d.ta,2)
      se.ta<-round(se.ta,3)
      afa.j<-afa/(t*(t-1))*2
      Z<-round(qnorm(1-afa.j),3)
      #Z<-round(qnorm(1-afa),3)
      signif<-(D>Z)*1
      com.table<-data.frame(na.com,d.ta,se.ta,D,Z,signif)   ###兩兩比較表#######
      names(com.table)<-c("比較式","處裡等級平均差","SE","Dunn","Z","顯著1不顯著0")
      Filena<-paste("d:/KW_compared_table_",T.x,".csv",sep="")
      write.csv(com.table,Filena)  #######################比較表存在F槽中KW_compared_table.csv
      ####AB比較表###############################  
      ab.table<-round(R.mean.s,2)
      count<-t-1
      count3<-0
      ab.temp<-c('a')
      ab.temp[2:t][signif[1:(t-1)]==0]<-letters[1]
      ab.temp[2:t][signif[1:(t-1)]==1]<-"."
      ab.table<-rbind(ab.table,ab.temp)
      for(i in 2:(t-1)){
         
         count0<-count+1
         count<-count+(t-i)
         ab.temp<-0
         ab.temp[1:(i-1)]<-"."
         ab.temp[i]<-letters[i]
         ab.temp[(i+1):t][signif[count0:count]==0]<-letters[i]
         ab.temp[(i+1):t][signif[count0:count]==1]<-'.'
         ab.table<-rbind(ab.table,ab.temp)
      }
      rownames(ab.table)<-c("mean",na.s[1:(t-1)])
      ab.table1<-data.frame(ab.table)
      names(ab.table1)<-na.s
      Filena<-paste("d:/KW_AB_table_",T.x,".csv",sep="")
      write.csv(ab.table1,Filena) #####比較表存在F槽中KW_AB_table.csv
      ####AB比較表###############################  
      
      list(kruskal.test(x,g),com.table,ab.table1)
      
   }else{
      
      list(kruskal.test(x,g),"DO NOT significant!",R.mean.s)
      
   }
}

##########KW.test函數結束#################################

data1<-read.csv("D:/C.csv")
x<-10-(data1[,4])
g<-data1[,1]
afa<-0.1
T.x<-format(Sys.time(), "%F_%H%M%S") #紀錄日期時間a)

