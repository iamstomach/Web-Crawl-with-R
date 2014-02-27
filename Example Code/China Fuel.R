setwd("M:\\02.Working\\9.Download Data\\China Fuel")
library(RCurl)
library(rjson)
require(XML)
library(XLConnect)
options( java.parameters = "-Xmx4g" )
library( "RWeka" )
record.value.final <- character(0)
page.n <- 1

# 注意
# 以下参数需要替换,总页数和总记录条数会改变
# count= 27033,
# pages=541,
for (page.n in 1:541){
  URLname <- "http://gzly.miit.gov.cn:8090/datainfo/miit/babs2.jsp"
  b0 <- postForm(URLname, 
                 count= 27033,
                 pages=541,
                 page=page.n,
                 # vehiclecategory=1,
                 style="POST")
  b2 = htmlParse(b0, asText = TRUE, encoding = 'UTF-8')
  b3.head <- getNodeSet(b2, path = '//div[@id="divBody"]//div[@id="divContent"]//table[@class="list-table"]/tr/th')
  b3.head.value <- sapply(b3.head, xmlValue)
  b3.list <- getNodeSet(b2, path = '//div[@id="divBody"]//div[@id="divContent"]//table[@class="list-table"]/tr')
  record.value <- character(0)
  for (record.n in 2:length(b3.list)){
    b3.value <- getNodeSet(b2, path = paste('//div[@id="divBody"]//div[@id="divContent"]//table[@class="list-table"]/tr[', record.n, ']/td', sep = ""))
    b3.value2 <-  sapply(b3.value, xmlValue)
    b3.value2 <- gsub('\\r|\\t|\\n||(^\\s*)|(\\s*$)', "", b3.value2)
    record.value <- rbind(record.value, b3.value2)     
  }
  colnames(record.value) = b3.head.value
  print(page.n)
  record.value.final <- rbind(record.value.final, record.value)
}

colnames(record.value.final)
summary(record.value.final)

library(xlsx)
# 导出excel
aa <- as.data.frame(unique(record.value.final[, "通用名称"]))
write.xlsx(aa, "China Fuel name.xlsx")

# 替代方式
# 先用分号或tab分隔符导出txt, 再用excel打开txt文件
write.table(record.value.final,row.names=F,sep=";", "China Fuel.txt")



