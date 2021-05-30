##Chapter 7-1 繪圖

library(ggplot2)
data(diamonds)
head(diamonds)

diamonds $ carat ##是一個vector資料
##長條圖
hist ( diamonds $ carat , main = "Carat Histogram" , xlab = "Carat")

##散布圖
plot ( price ~ carat , data = diamonds)
##          ~ 代表想了解price(y軸)對carat(x軸)的關係

##盒鬚圖
boxplot ( diamonds $ carat )


##ggplot2套件的ggplot函數是個優秀的畫圖工具，可以再加號增加層次

##直方圖
ggplot ( data = diamonds ) + geom_histogram( aes ( x = carat ) )


##密度圖
ggplot ( data = diamonds ) + geom_density ( aes ( x = carat ) , fill = "grey50")


##散步圖with ggplot2

##把初始的ggplot存入變數
ggplot ( data = diamonds ) + geom_point( aes ( x = carat , y = price ))

ggplot ( data = diamonds ) + geom_point( aes ( x = carat , y = price , color = color ))

g <- ggplot ( data = diamonds )


head(diamonds)

##圖組(facet)，以clarity為分類
g + geom_point ( aes ( x = carat , y = price , color = color )) + facet_wrap (~clarity)

##圖組(facet)，以cut為x軸 clarity為y軸
g + geom_point ( aes ( x = carat , y = price , color = color )) + facet_grid (cut~clarity)

##將點點圖換成histom也可以

h <- g + geom_histogram( aes ( x = carat , color = color  ))

h + facet_grid ( ~carat )


##盒鬚圖

ggplot ( data = diamonds , aes ( y = carat , x = 1 )) + geom_boxplot ()

ggplot ( data = diamonds , aes ( y = carat , x = cut )) + geom_boxplot ()

ggplot ( data = diamonds , aes ( y = carat , x = cut )) + geom_violin ()

##line plot
data("economics")

ggplot ( economics , aes ( x = date , y = pop)) + geom_line()

head(diamonds)
ggplot ( data = diamonds , aes ( x = price , y = carat )) + geom_line()

##處理日期資料並作圖
install.packages("lubridate")
library (lubridate)

head(economics)

economics $ year <- year (economics$date)
economics $ month <- month (economics$date , label = T)
economics $ month

econ2000 <- economics [which (economics $ year >=2000) , ]
econ2000

ggplot ( data =  econ2000 , aes ( x = year , y = pop) ) + 
  geom_line ( aes (color = factor (month) , group = month)) +
  scale_color_discrete ( name = "Year") +
  scale_y_continuous (labels =comma) +
  labs (title = "Population Grouth" , x ="month" , y = "popular")


## 7-2-5 主題樣式

install.packages("ggthemes")
library(ggthemes)

g2 <- ggplot ( data = diamonds , aes ( x = carat , y = price )) + geom_point (aes (color = color ))

##加入ggthemes色彩主題

g2 + theme_economist()  + scale_colour_economist() ##The economist 主題

g2 + theme_excel() + scale_colour_excel() ## Excel

g2 + theme_tufte()

g2 + theme_wsj()
