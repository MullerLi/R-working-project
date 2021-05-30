#R-QTL2 ANALYSIS
install.packages("qtl2", repos="https://rqtl.org/qtl2cran")
install.packages(c("devtools", "yaml", "jsonlite", "data.table", "RcppEigen", "RSQLite", "qtl"))

grav2 <- read_cross2("https://kbroman.org/qtl2/assets/sampledata/grav2/grav2.yaml")
summary(grav2)

iron <- read_cross2("https://kbroman.org/qtl2/assets/sampledata/iron/iron.yaml")


HI <- read_cross2("D:/a.zip")
map <- insert_pseudomarkers(HI$map, step=1)
# r-qtl analysis
install.packages(r/qtl)
library(q)
1+1

Q <- read.cross (format="csv", file="D:/RIQTL.csv", na.strings=".", genotypes=c("A","H","B","",""), alleles=c("A","B"), estimate.map=TRUE, map.function="kosambi")
summary(Q)
plot.map(Q)


Q2<- calc.genoprob(Q, step=1, map.function="kosambi")
out.perm_mr<-scanone(Q, pheno.col="HI6", method="mr", n.perm=1000)

summary(out.perm_mr)
out.mr<-scanone(Q, pheno.col="HI6", method="mr")
summary(out.mr)
plot(out.mr)
abline(h=1.5)

out.perm.hk<-scanone(Q, pheno.col="HI6", method="hk", n.perm=1000)
summary(out.perm.hk,alpha=0.05)
out.hk<-scanone(Q, pheno.col="HI6", method="hk")
summary(out.hk,threshold=1.5)
plot(out.hk)
abline(h=1.5)

