## 精通R語言 Chapter 3
install.packages("coefplot")

#要從GitHub跟BitBucket下載套件的話，要先安裝devtools
install.packages("devtools")
library(devtools)
install_github(repo = "coefplot", username = "jaredlander")

#卸載
detach("package:coefplot")

################################
## 精通R語言 Chapter 4

#運算,變數指定
1+1
1-1
1*1
1/1

x<-1
1->x

x <- y <- 1

assign("s",1)
s

rm(s)

##資料型態 數字numeric ˋ整數integer 
x<-5
class(x)
is.numeric(x)

#邏輯 TRUE=1 FALSE=0
TRUE*5
class(TRUE)

class(TRUE*8)

2 == 3
2 != 3 
"fa" == "ga"

#4-4 vector

#vector內的元素必須同樣屬性，不可以混合數字跟自元
y<-c(1,2,3,5)
x <- c("f","g")
z <- c (1, "d")
z*6
#vector沒有維度，比較像是集合；　c的意思是combine
a<- c(1,5,8,9,2,5)
#像量可以同時做蝶帶
a*5
sqrt(a)

x <- c(5:90)
#像量藥一樣長才能加減
x+a
x<-c(1:10)
y<-c(2:11)

x^y

length(x)

##any all來進行邏輯檢驗
any(x<y)
all(x<y)
##[]來選取第幾個項目
x[4]
x[c(1,8)]

##幫向量的各個元素取名
x <- c(Monday = 50 , Tuesday = 60 , Wensday = 70)
h <- c(January = "Firstly" , Feburary = "Secondy")
h
names(x) <- c("Monday" , "b" , "c")
x
##4-4-2 Factor
#建立Factor的方式：先建立向量，再用as.factor轉換成factor

q2 <- c(q,"Hockey", "Lacrosse", "Hockey")
rm(q2)
q2 <- as.factor(q2)
q2


##4-5 呼叫函數
mean(x)
?'=='
##apropos可以查看相關的函數
apropos('mean')



##4-7遺失值 NA , NA是一個值 不等於NULL

r<-c(NA,NA,1,3,5)
r*5

is.na(r)
mean(r) #有NA就沒辦法算
mean(r , na.rm = TRUE) #加 na.rm 指令移除NA就可以算

##NULL為不存在，不是遺失，而是不存在，因為不存在所已沒辦法存進向量
r<-c(NA,NULL,5,7,9,10)
e<-c(NULL,NULL,NULL)
is.null(r)
is.null(e)


## 4-8 pipe運算子 %>%　，亦即將左方物件帶入右方函數內
library(magrittr)
x<- c(1,5,6)
x %>% mean #其效力等於mean(x)

##例如：計算一個向量內的NA元素數量，可以用兩種寫法，PIPE寫法較為簡潔

d <- c(NA,NA,NA,5:10)
sum(is.na(d)) ##NA有三個

#或是

d %>% is.na %>% sum  #pipe是從最左遞次帶到最右
d %>% is.na %>% mean(na.rm=T)  #pipe是從最左遞次帶到最右

