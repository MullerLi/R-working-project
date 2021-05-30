##Chapter 6
##讀取資料的方式，最適合的方法是將資料存在ＥＸＣＥＬ內再轉存維CSV，用read.csv指令讀取
##但是，其實也可以用readxl套件讀取.xls .xlsx檔案
library(readxl)

download.file( url = 'http://www.jaredlander.com/data/ExcelExample.xlsx' ,  ##下載范立檔案
               destfile = 'D:/ExcelExample.xlsx' , mode = 'wb' )

excel_sheets('D:/ExcelExample.xlsx')

tomato <- read_excel('D:/ExcelExample.xlsx') #用這個指令讀取的檔案，非data.frame而是tibble

head(tomato)

tomatowine <- read_excel('D:/ExcelExample.xlsx' , sheet = 'Wine' ) #用sheet指令，指定wine活頁簿；或是用sheet=2讀取第二夜

tomatowine

data() ##查看R內建的資料

##6-7 抓取網路資料
##html表格
install.packages("XML")
library(XML)

theURL <- "http://www.flag.com.tw/data/FT736_sample.html"

bowlpool <- readHTMLTable(theURL , which = 1, header = F , stringsAsFactors = F)
##網站掛掉 先略過













