#---------綜合變方分析---------#












#-------GGE biplot--------#
install.packages("agridat")
install.packages("gge")
library(agridat)
data(yan.winterwheat)
edit(dat1 <- yan.winterwheat)
library(gge)
m1 <- gge(dat1, yield~gen*env, scale=FALSE)
biplot(m1, main="yan.winterwheat - GGE biplot",
       flip=c(1,0), origin=0, hull=TRUE)
