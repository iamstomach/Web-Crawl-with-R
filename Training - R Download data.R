setwd("M:/02.Working/9.Download Data/Old Car")


# library(XLConnect)
# library(XML)
# library(reshape2)
# library(RJSONIO)
# library(RCurl)
# library(rJava)

- Introduction
- Part1: Downloading HTML table directly
- Part2: using Xpath
- Part3: from JSON to List
- Part4: post para to web
- Part5: save to Excel file

########### Introduction ----

## software
R / Rstudio

SAS: PDV, program data vector
R: array program

help()
wikibooks:http://en.wikibooks.org/wiki/R_programming
stack overflow
google
统计之都


## quick start

- Both of SAS and R have its own BASE part
- similar with SAS Components, R has many Packages

# computation
1 + 2*3 
# objects
a <- c(1, 3, 5)
b <- c("A", "B", "C")
# Use function
mean(a)
sd(a)
# fuchiton help
help("mean")
# more function, install package firstly
install.package("ggplot2")
# use package
library("ggplot2")


## other language
HTML 超文本标记语言，表现或展示数据
XML/XPath  可拓展标记语言，传送及携带数据
JSON  JavaScript对象表示法，类似XML
regular expression (string) : http://en.wikibooks.org/wiki/R_programming/Text_Processing
java (export to excel)




########### Part1: Downloading HTML table directly ----

## download parameters of main model in autohome ###
web <- "http://car.autohome.com.cn/duibi/chexing/carids=14941,0,0,0"
doc <- htmlParse(web, encoding = "gbk")
tableNodes <- getNodeSet(doc, "//table")
tb <- readHTMLTable(tableNodes[[4]])
apply(tb, 2, function(x)iconv(x, "UTF-8","gbk"))


########### Part2: using Xpath ----

library(XML)

## demo2.1-get segmentation of autohome

web <- "http://www.autohome.com.cn"
a <- htmlParse(web, encoding = "GBK")

## only one record
(a1 <- getNodeSet(a, path = '//div[@id="header"]/div[2]/ul/li[1]/a'))
(a1_value <- xmlValue(a1[[1]]))
iconv(a1_value, "UTF-8","gbk")
(a1_att1 <- xmlGetAttr(a1[[1]], 'href'))
(a1_att2 <- xmlGetAttr(a1[[1]], 'target'))

## get all records
## function sapply(object, function)
segment_raw <- getNodeSet(a, path = '//div[@id="header"]/div[2]/ul/li/a')
segment_name <-  iconv(sapply(segment_raw, xmlValue), "UTF-8","gbk")
segment_id <- sapply(seq_len(length(segment_raw)), 
                     function(i){xmlGetAttr(segment_raw[[i]], 'href')})
seg_list <- cbind(segment_name, segment_id)


## demo2.2-get information of old car in che168(autohome)

## get base information
web <- "http://www.che168.com/china/fute/"
a <- htmlParse(web, encoding = "gbk")
a1 <- getNodeSet(a, path = '//ul[@class="piclist_ul"]/li')
value <- iconv(sapply(a1, xmlValue), "UTF-8","gbk")
page_max <- iconv(sapply(getNodeSet(a, path = '//div[@class="list_page"]/span[2]'), xmlValue), "UTF-8","gbk")
page_max0 <- as.numeric(str_match(page_max, "\\d+"))

## for loop
web0 <- "http://www.che168.com/china/fute/"
data_final <- character(0)
for (page_n in 1034:1164){
  web <- paste("http://www.che168.com/china/fute/", "?page=", page_n, sep="")
  a <- htmlParse(web, encoding = "gbk")
  a1 <- getNodeSet(a, path = '//ul[@class="piclist_ul"]/li')
  selldate <- iconv(sapply(getNodeSet(a, path = '//ul[@class="piclist_ul"]/li/div[@class="pcar_pic"]/a'), xmlValue), "UTF-8","gbk")
  model_version <- iconv(sapply(getNodeSet(a, path = '//ul[@class="piclist_ul"]/li/div[@class="fortiti"]'), xmlValue), "UTF-8","gbk")
  price <- iconv(sapply(getNodeSet(a, path = '//ul[@class="piclist_ul"]/li/div[@class="pcar_grey"]/div[1]/span[@class="pcar_price"]/span[1]'), xmlValue), "UTF-8","gbk")
  city <- iconv(sapply(getNodeSet(a, path = '//ul[@class="piclist_ul"]/li/div[@class="pcar_grey"]/div[1]/span[@class="fr gray99"]'), xmlValue), "UTF-8","gbk")
  info <- iconv(sapply(getNodeSet(a, path = '//ul[@class="piclist_ul"]/li/div[@class="pcar_grey"]/div[2]'), xmlValue), "UTF-8","gbk")
  type <- iconv(sapply(getNodeSet(a, path = '//ul[@class="piclist_ul"]/li/div[@class="pcar_grey"]/div[3]/span[1]'), xmlValue), "UTF-8","gbk")
  pos <- unlist(gregexpr("\\d{4}款", model_version))
  len <- str_length(model_version)
  pos2 <- cbind(pos, len)
  pos2[pos2[, 1]==-1,1] <- pos2[pos2[,1]==-1,2]+1
  pos3 <- pos2[, 1]
  model <- str_sub(model_version, 1, pos3-1)
  modelyear <- str_sub(model_version, pos3, pos3+4)
  version <- str_trim(str_sub(model_version, pos3+5))
  data_temp <- cbind(selldate, model, modelyear, version, price, city, info, type)
  data_final <- rbind(data_final, data_temp)
  print(paste("page=", page_n))
}



########### Part3: from JSON to List ----
# get version from Autohome.com

# find the information in this website "http://car.autohome.com.cn/duibi/chexing/"

# model list
web <- "http://car.autohome.com.cn/javascript/NewSpecCompare.js"
a <- htmlParse(web, encoding = "gbk")
a1 <- getNodeSet(a, path = '//p')
a2 <- iconv(xmlValue(a1[[1]]), "UTF-8","gbk")
a3 <- (strsplit(as.character(a2), "=|;"))[[1]][2]
library(RJSONIO)
b <- fromJSON(a3)
b1 <- unlist(b)
b1[names(b1)=="N"]
which(names(b1)=="N")


########### Part4: post para to web ----
http://www.iihs.org/iihs/topics/insurance-loss-information

library(RCurl)
opts = list(verbose=T)
getdata <- postForm('http://www.iihs.org/research/hldi/composite/getviewmodel', .opts=opts,
                    modelYears = "2009-2011", vehicleClassId = "1", vehicleSizeId = "1",
                    style="POST")
library(RJSONIO)
TT <- fromJSON(getdata)
GG <- unlist(TT)
GG[names(GG)=="vehiclesByClass.vehicle"]
GG[names(GG)=="vehiclesByClass.collision.displayText"]


########### Part5: save to Excel file ----
library(XLConnect)
xls <- loadWorkbook("二手车之家旧车价格3.xlsx", create = T)
createSheet(xls, "FORD")
writeWorksheet(xls, data_final, "FORD")
setColumnWidth(xls, "FORD", column = 1:7, width = -1)
saveWorkbook(xls)
