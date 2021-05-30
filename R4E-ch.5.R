## R for everyone chapter 5

#5.1 data.frame
#data.frame是由row跟column組成，每個row是一組觀測值，每個column是一個變數
#data.frame是由相同長度的vector組成的，這個特性使data.frame可以在行列之間儲存不同型態的元素


x <- c(1:5)
y <- 11:15
z <- c("a","b","c","d","e")

d <- data.frame(x,y,z)
d
##由此可見，未指定時，向量名稱為變數名稱、相量內容則是觀測值

##另一種方式是在data.frame中宣告變數名稱
d2 <- data.frame( fir=x , sec = y , thr = z)
d2

#資料框架的結構/維度，可以用nrow , ncol 或是 dim查看
nrow(d2)
ncol(d2)
dim(d2)

##資料框架的變數名稱，可以用names查看
d2
names(d2)
#亦或是
rownames(d2)
colnames(d2)

#宣告行列名稱
rownames(d2) <- c("r1","r2","r3","r4","r5")
colnames(d2) <- c("c1","c2","c3")

d2

#查詢變數名稱
d2$c1
d2[5,2] #先row在col
d2$c3

d2[,c("c2","c3")]
 d3 <- d2[,"c1"] 

e1 <- d2["c1"] ##取出d2的c1欄，並指定為data.frame
e2 <- d2[,"c2"]##取出d2的c1欄，並指定為數值
e3 <- d2[,"c3"]##取出d2的c1欄，並指定為factor
e4 <- d2["c3"]
d2
e5 <- d2["r1",] #想取出r1列的話，一定要用","指定data.frame
e6 <- d2["r2"] #否則會失敗

##欄如果有用","指定的話(e.g. e3)，會變成vector, factor或是numeric(因為只有一行/列)；
##欄如果沒有用","指定的話(e.g. e4)，會是data.frame(因為包含rowname跟colname)
##列比較單純，只能用["r1",]取出第幾列，必定是DATA.FRAME
class(e1)
class(e2)
class(e3)
class(e3)
class(e4)
class(e5)
class(e6)
############################################################################################################
############################################################################################################

#chapter 5-2 list

#List可以儲存任何類型的資料,包含numeric character data.frame list

#建立list
l1<-list(1,2,5,3,1)
l1

l1<-list(e1,d2,d)
l1
##包含data.frame; vector; list的list
l2 <- list(d2 , 1:10 , l1)
l2
##用()把指另包起來 可以馬上印出
(mean(d2))

##list可以命名
(names(l2) <- c("e1" , "d2" , "d"))

##取出list的元素 #第d之list的第三個資料的第y欄
l2 $ d [[3]] $ y

##也可以用指定的方式來做list
l3 <- list( a =e1 , b = d2 , c =d)
l3
##或是附加上去
l3

l3[["d"]] <- c(5:10)
l3$d
## 5-4 matrix and array 先略過

