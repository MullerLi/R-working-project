install.packages("qtl2", repos="https://rqtl.org/qtl2cran")
iron <- read_cross2( system.file("extdata", "iron.zip", package="qtl2") )
head(iron)
summary(iron)
map <- insert_pseudomarkers(iron$gmap, step=1)
map
pr <- calc_genoprob(iron, map, error_prob=0.002)
pr <- calc_genoprob(iron, map, error_prob=0.002, cores=4)
apr <- genoprob_to_alleleprob(pr)
out <- scan1(pr, iron$pheno, Xcovar=Xcovar)
out <- scan1(pr, iron$pheno, Xcovar=Xcovar, cores=4)
Xcovar <- get_x_covar(iron)


par(mar=c(5.1, 4.1, 1.1, 1.1))
ymx <- maxlod(out) # overall maximum LOD score
plot(out, map, lodcolumn=1, col="slateblue", ylim=c(0, ymx*1.02))
plot(out, map, lodcolumn=2, col="violetred", add=TRUE)
legend("topleft", lwd=2, col=c("slateblue", "violetred"), colnames(out), bg="gray90")
find_peaks(out, map, threshold=4, drop=1.5)
find_peaks(out, map, threshold=4, peakdrop=1.8, drop=1.5)
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


  