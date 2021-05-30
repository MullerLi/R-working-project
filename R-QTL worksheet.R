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
rm(
