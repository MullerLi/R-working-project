library(qtl)

## A guide to QTL mapping via R/qtl by yu學長的protocol
##estimate.map function可以計算距離
fake_f2 <- read.cross(format="csv", file="D:/F2.csv", na.strings=".", genotypes=c("A","H","B","",""), alleles=c("A","B"), estimate.map=TRUE, map.function="kosambi")
##計算連鎖群(資料先全部填1號連鎖群)
fake_ungroup<-est.rf(fake_f2)
fake_group<-formLinkageGroups(fake_ungroup,max.rf=0.4, min.lod=3)
fake_group
##計算MARKER順序
revcross<-orderMarkers(fake_f2,chr=4, window=4, use.ripple=TRUE, map.function="kosambi")
?orderMarkers()
x<-pull.map(revcross)
write.csv(capture.output(x),paste0("D:/MARKER.csv"))
plot.map(revcross,show.marke1r.names=TRUE)

## A guide to QTL mapping via R/qtl by yu學長的protocol
##estimate.map function可以計算距離
hot <- read.cross(format="csv", file="D:/hot.csv", na.strings="-", genotypes=c("A","H","B","C"), alleles=c("A","B"), estimate.map=TRUE, map.function="kosambi")
##計算連鎖群(資料先全部填1號連鎖群)
fake_ungroup<-est.rf(hot)
fake_group<-formLinkageGroups(fake_ungroup,max.rf=0.4, min.lod=3)
fake_group
##計算MARKER順序
revcross<-orderMarkers(hot,chr=4, window=4, use.ripple=TRUE, map.function="kosambi")
x<-pull.map(revcross)
write.csv(capture.output(x),paste0("D:/MARKER.csv"))
plot.map(revcross,show.marke1r.names=TRUE)

hot<- calc.genoprob(hot, step=1, map.function="kosambi")
out.perm.hk<-scanone(hot, pheno.col="hot", method="hk", n.perm=1000)
summary(out.perm.hk,alpha=0.05)
out.hk<-scanone(hot, pheno.col="hot", method="hk")
summary(out.hk,threshold=1.5)
plot(out.hk)
abline(h=2)
qtl<-makeqtl(hot,2,170,what="prob")

out.qtl<-fitqtl(hot, pheno.col="hot", method="hk", 
                qtl=qtl,formula=y~Q1)
summary(out.qtl)





#或是這樣，可以讀取Ｄ槽底下的資料
qtl <- read.cross ("csv" , "D:/" , "listeria.csv"
                   + genotypes　= c　("BB","BC","CC"),
                   + alleles　=　c　("B","C")
                   + estimate.map = TRUE)

## Construction of genetic map by R/qt]

library(qtl)
data(mapthis)
summary(mapthis)

##檢定各種marker是否符合分離比1:2:1
gt <- geno.table(mapthis)
gt[gt$P.value < 0.05/totmar(mapthis),] ##p value<0.05 -> 符合
todrop <- rownames(gt[gt$P.value < 1e-10,]) ##刪除一些怪資料
mapthis <- drop.markers(mapthis, todrop)    ##刪除一些怪資料

##計算各種基因型AAABBB的比例
g <- pull.geno(mapthis)
gfreq <- apply(g, 1, function(a) table(factor(a, levels=1:3)))
gfreq <- t(t(gfreq) / colSums(gfreq))
par(mfrow=c(1,3), las=1)
for(i in 1:3)  
  + plot(gfreq[i,], ylab="Genotype frequency", main=c("AA", "AB", "BB")[i]
         +ylim=c(0,1))

mapthis <- est.rf(mapthis)
checkAlleles(mapthis, threshold=5)
mapthis<-markerlrt(mapthis)
checkAlleles(mapthis, threshold=5)

rf <- pull.rf(mapthis)
lod <- pull.rf(mapthis, what="lod")
plot(as.numeric(rf), as.numeric(lod), xlab="Recombination fraction", ylab="LOD score")

mapthis <- orderMarkers(mapthis, chr=5)
pull.map(mapthis, chr=5)


