setwd("M:/02.Working/9.Download Data/Old Car")


library(XLConnect)
require(XML)
library(reshape2)
library(RJSONIO)
library(RCurl)
library(rJava)

web <- "http://www.che168.com/china/fute/"
a <- htmlParse(web, encoding = "gbk")
a1 <- getNodeSet(a, path = '//ul[@class="piclist_ul"]/li')

getNodeSet(a, path = '//ul[@class="piclist_ul"]/li')

value <- iconv(sapply(a1, xmlValue), "UTF-8","gbk")
page_max <- iconv(sapply(getNodeSet(a, path = '//div[@class="list_page"]/span[2]'), xmlValue), "UTF-8","gbk")
page_max0 <- as.numeric(str_match(page_max, "\\d+"))



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


library(XLConnect)
xls <- loadWorkbook("二手车之家旧车价格3.xlsx", create = T)
createSheet(xls, "FORD")
writeWorksheet(xls, data_final, "FORD")
setColumnWidth(xls, "FORD", column = 1:7, width = -1)
saveWorkbook(xls)




## download model one by one
setwd("M:/02.Working/9.Download Data/Old Car")


# get modellist and website
model_list <- read.csv("Modellist-WenYe.csv", stringsAsFactors = F)
names(model_list) <- c("model", "model_web")

# get max page
require(XML)
require(stringr)
library(RCurl)
library(doParallel)
# for (model_n in 1:dim(model_list)[1]){
for (model_n in c(39)){
#   model_n <- 38

  rm("data_final", "data_final1", "data_final2")
  modelname <- model_list[model_n, "model"]
  # Add selection
  web0 <- paste(model_list[model_n, "model_web"], "a0_5ms2dgscncgpiltocspex/", sep="")
  
  a <- htmlParse(web0, encoding = "gbk")
  page_max <- iconv(sapply(getNodeSet(a, path = '//div[@class="list_page"]/span[2]'), xmlValue), "UTF-8","gbk")
  page_max0 <- as.numeric(str_match(page_max, "\\d+"))
  print(paste(modelname, web0, page_max0))  
  # get record
  data_final <- character(0)
  Sys.time()
  for (page_n in 1:page_max0){
#     page_n <- 7
    web <- paste(model_list[model_n, "model_web"], "a0_5ms2dgscncgpiltocsp", page_n, "ex/", sep="")
    a <- try(htmlParse(web, encoding = "gbk"))
    
    if (inherits(a, "try-error")) { 
      a <- try(htmlParse(web, encoding = "gbk"))
      if (inherits(a, "try-error")) stop("Error fetching data") 
    } 
    
    a1 <- getNodeSet(a, path = '//ul[@class="piclist_ul"]/li')
    selldate <- iconv(sapply(getNodeSet(a, path = '//ul[@class="piclist_ul"]/li/div[@class="pcar_pic"]/a'), xmlValue), "UTF-8","gbk")
    model_version <- iconv(sapply(getNodeSet(a, path = '//ul[@class="piclist_ul"]/li/div[@class="fortiti"]'), xmlValue), "UTF-8","gbk")
    carid <- sapply(seq_len(length(model_version)), 
                    function(i){xmlGetAttr(
                      getNodeSet(a, path = '//ul[@class="piclist_ul"]/li/div[@class="pcar_pic"]/span[1]')[[i]], 
                      'id')})
    weblist <- sapply(seq_len(length(model_version)), 
                      function(i){xmlGetAttr(
                        getNodeSet(a, path = '//ul[@class="piclist_ul"]/li/div[@class="fortiti"]/a')[[i]], 
                        'href')})
    # get new information  
    newinfo <- character()
      for (j in 1:length(weblist)){
        web_tt <- paste("http://www.che168.com", weblist[j], sep="");
        a_tt <- try(XML::htmlParse(web_tt, encoding = "gbk"));
        
        if (inherits(a_tt, "try-error")) { 
          a_tt <- try(XML::htmlParse(web_tt, encoding = "gbk"))
          if (inherits(a_tt, "try-error")) stop("Error fetching data") 
        } 
        
        if (names(XML::xmlAttrs(XML::getNodeSet(a_tt, '//ul/li[@id="NewCarConifgTab"]')[[1]]))[1] == "style") {
          newinfo_temp <- rep("", 4)
        } else {
          spec <- XML::xmlGetAttr(XML::getNodeSet(a_tt, '//ul[@class="refer_ul"]/li/a[@id="CarNewPrice"]')[[1]],'href'); 
          newspecid <- gsub(".*/(\\d+)/.*", "\\1", spec);
          specinfo <- RCurl::getURL(paste("http://www.che168.com/handler/CarDetail/NewCarOption.ashx?specid=", newspecid, sep = ""),
                                    .encoding = "gbk");
          newversion <- gsub('.*\"Name\":\"车型名称\",\"Value\":\"(.*?)\"}.*', "\\1", specinfo);
          newmsrp <- gsub('.*\"Name\":\"厂商指导价\\(元\\)\",\"Value\":\"(.*?)\"}.*', "\\1", specinfo);
          newgrade <- gsub('.*\"Name\":\"环保标准\",\"Value\":\"(.*?)\"}.*', "\\1", specinfo);
          newinfo_temp <- cbind(newspecid, newversion, newmsrp, newgrade)
        }
       
        newinfo <- rbind(newinfo, newinfo_temp)
        print(paste(modelname, "page=", page_n, j))
      }



    price <- iconv(sapply(getNodeSet(a, path = '//ul[@class="piclist_ul"]/li/div[@class="pcar_grey"]/div[1]/span[@class="pcar_price"]/span[1]'), xmlValue), "UTF-8","gbk")
    location <- iconv(sapply(getNodeSet(a, path = '//ul[@class="piclist_ul"]/li/div[@class="pcar_grey"]/div[1]/span[@class="fr gray99"]'), xmlValue), "UTF-8","gbk")
    info <- iconv(sapply(getNodeSet(a, path = '//ul[@class="piclist_ul"]/li/div[@class="pcar_grey"]/div[2]'), xmlValue), "UTF-8","gbk")
    seller <- iconv(sapply(getNodeSet(a, path = '//ul[@class="piclist_ul"]/li/div[@class="pcar_grey"]/div[3]/span[1]'), xmlValue), "UTF-8","gbk")
    pos <- unlist(gregexpr("\\d+款", model_version))
    pos22 <- unlist(gregexpr("\\d{1}款", model_version))
    len <- str_length(model_version)
    pos2 <- cbind(pos, len)
    pos2[pos2[, 1]==-1,1] <- pos2[pos2[,1]==-1,2]+1
    pos3 <- pos2[, 1]
    model <- str_sub(model_version, 1, pos3-1)
    modelyear <- str_sub(model_version, pos3, pos22)
    version <- str_trim(str_sub(model_version, pos22+2))
    data_temp <- cbind(carid, selldate, model, modelyear, version, price, location, info, seller, newinfo)
    data_final <- rbind(data_final, data_temp)
    print(paste(modelname, "page=", page_n))
  }
  Sys.time()
  data_final1 <- as.data.frame(apply(data_final, 2, str_trim), stringsAsFactors=F)
  
  
  data_final1$sellyear <- gsub('(\\d{4})年(\\d{2})月(\\d{2})日.*', "\\1",data_final1$selldate)
  data_final1$sellmonth <- gsub('(\\d{4})年(\\d{2})月(\\d{2})日.*', "\\2",data_final1$selldate)
  data_final1$sellday <- gsub('(\\d{4})年(\\d{2})月(\\d{2})日.*', "\\3",data_final1$selldate)
  data_final1$Tprice <- gsub('\\￥|\\万|\\(已售\\)', "",data_final1$price)
  data_final1$issell <- gsub('.*(已售).*', "\\1",data_final1$price)
  data_final1$issell[data_final1$issell!="已售"] <- "在售"
  data_final1$Mileage <- gsub('(.*?万公里).*', "\\1",data_final1$info)
  data_final1$Registdate <- gsub('.*公里(.*)年', "\\1",data_final1$info)
  data_final1$selltype <- data_final1$seller
  data_final1$selltype[data_final1$seller!="个人"] <- "商户"
  data_final1$province <- gsub('(^.*) (.*$)', "\\1",data_final1$location)
  data_final1$city <- gsub('(^.*) (.*$)', "\\2",data_final1$location)
  
  select_var <- c("carid", "sellyear","sellmonth","sellday","model","modelyear","version","Tprice", 
                  "Registdate","Mileage","issell","selltype","province","city","seller",
                  "newspecid" , "newversion", "newmsrp", "newgrade")
  data_final2 <- data_final1[, select_var]
  
  write.csv(data_final2, paste("二手车之家旧车价格", modelname, "1-100.csv", sep=""))
}


## 并行运算部分

newinfo <- foreach(j=1:length(weblist), .combine=rbind,.packages=c("XML", "RCurl", "stringr")) %dopar% { 
  web_tt <- paste("http://www.che168.com", weblist[j], sep="");
  a_tt <- try(htmlParse(web_tt, encoding = "gbk"));
  
  if (inherits(a_tt, "try-error")) { 
    a_tt <- try(htmlParse(web_tt, encoding = "gbk"))
    if (inherits(a_tt, "try-error")) stop("Error fetching data") 
  } 
  
  if (names(xmlAttrs(getNodeSet(a_tt, '//ul/li[@id="NewCarConifgTab"]')[[1]]))[1] == "style") {
    newinfo_temp <- rep("", 4)
  } else {
    spec <- xmlGetAttr(getNodeSet(a_tt, '//ul[@class="refer_ul"]/li/a[@id="CarNewPrice"]')[[1]],'href'); 
    newspecid <- gsub(".*/(\\d+)/.*", "\\1", spec);
    specinfo <- getURL(paste("http://www.che168.com/handler/CarDetail/NewCarOption.ashx?specid=", newspecid, sep = ""),
                       .encoding = "gbk");
    newversion <- gsub('.*\"Name\":\"车型名称\",\"Value\":\"(.*?)\"}.*', "\\1", specinfo);
    newmsrp <- gsub('.*\"Name\":\"厂商指导价\\(元\\)\",\"Value\":\"(.*?)\"}.*', "\\1", specinfo);
    newgrade <- gsub('.*\"Name\":\"环保标准\",\"Value\":\"(.*?)\"}.*', "\\1", specinfo);
    newinfo_temp <- cbind(newspecid, newversion, newmsrp, newgrade)
  }
}












