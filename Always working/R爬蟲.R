install.packages("rvest")
library(rvest)



# class ="xxx" -> .xxx
# id = "xxx" -> #xxx
  # attribute xxx ->[xxx]
# [xxx=zzz] -> attribute xxx ="zzz"
ht="https://www.ptt.cc/bbs/Movie/index1.html"
ht %>% read_html() %>% html_elements(".title a") %>% html_text()
ht %>% read_html() %>% html_elements(".item a") %>% html_text()

read_html("https://zh.wikipedia.org/wiki/%E8%8B%B1%E5%9B%BD%E5%AF%B9%E6%9B%BC%E5%93%88%E9%A1%BF%E8%AE%A1%E5%88%92%E7%9A%84%E8%B4%A1%E7%8C%AE") ->ht2

ht2 %>% html_elements("p") %>% html_text() 

library(httr)
library(rvest)

httr::GET("http://www.wp.pl/", 
          set_cookies(`_SMIDA` = "7cf9ea4bfadb60bbd0950e2f8f4c279d",
                      `__utma` = "29983421.138599299.1413649536.1413649536.1413649536.1",
                      `__utmb` = "29983421.5.10.1413649536",
                      `__utmc` = "29983421",
                      `__utmt` = "1",
                      `__utmz` = "29983421.1413649536.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none)")) %>%
  read_html %>%  html_text()
  


all_url_page = paste0('https://www.ptt.cc/bbs/movie/index',8655:9036,'.html') 
all_url_data = c(); recom=c(); title=c(); date=c()

#爬每一篇文章的url，每一頁的推文數、標題及日期
for(i in 1:length(all_url_page)){
  all_url_data = c(all_url_data, read_html (all_url_page[i]) %>% 
                     html_nodes(css = ".title a") %>% html_attr('href'))
  recom = c(recom,read_html(all_url_page[i]) %>% html_nodes(css = ".nrec") %>% html_text())
  title = c(title, read_html(all_url_page[i]) %>% html_nodes(css = ".title a") %>% html_text())
  date = c(date, read_html(all_url_page[i]) %>% html_nodes(css = ".date") %>% html_text())
  if(i %% 30 == 0) Sys.sleep(runif(1,2,5))
}

my_data<- data.frame(recom,date,title)