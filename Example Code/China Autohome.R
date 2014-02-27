setwd("M:/02.Working/9.Download Data/SinaAuto Volume")
library(XLConnect)
require(XML)
library(reshape2)
library(RJSONIO)
library(RCurl)
library(rJava)


###################################################
# sina Vol
Format_Table <- function (web) {
  a <- htmlParse(web)
  record_n <- length(getNodeSet(a, path = '//div[@class="tb1"]/table/tr'))
  max_records <- 1
  for (record_i in 1:record_n){
    n_record <- length(getNodeSet(a, path = paste('//div[@class="tb1"]/table/tr[', record_i, ']/th')))
    max_records <- max(max_records, n_record)
  }
  max_records <- max_records - 3
  record_final <- character()
  for (record_i in 2:record_n){
    test <- getNodeSet(a, path = paste('//div[@class="tb1"]/table/tr[', record_i, ']/th[position()< last()-2]'))
    record <- sapply(test, xmlValue)
    record2 <- c(record, rep(NA, max_records - length(record)))
    record_final <- rbind(record_final, record2)
  }
  record_final2 <- as.data.frame(record_final, stringsAsFactors=F)
  return(record_final2)
}

segment_List <- getNodeSet(a, path = '/html/body/div[19]/div[2]/div[1]/div[2]/p[2]/a')
segment_name <- iconv(sapply(segment_List, xmlValue), "UTF-8","gbk")
segment_id <- sapply(seq_len(length(segment_List)), 
                     function(i){xmlGetAttr(segment_List[[i]], 'href')})
segment <- cbind(segment_id, segment_name)
segment <- segment[-1, ]
segment[, 1] = substr(segment[, 1], 20, 25)


xls <- loadWorkbook("新浪汽车销量数据-201303.xlsx", create = T)
xls2 <- loadWorkbook("新浪汽车销量数据-201303-2.xlsx", create = T)
for (i in c(1:5,7:8)){
  seg_file <- segment[i, 2]
  seg_id <- segment[i, 1]
  web <- paste('http://data.auto.sina.com.cn/xlsjk/onetype.php?start_date=2009-01&end_date=2013-03&',
               seg_id, sep = "")
  b_table <- Format_Table(web)
  b_table[,"V2"] <- iconv(b_table[,"V2"], "UTF-8","gbk")
  b_table1 <- b_table[!duplicated(b_table[,"V2"]),]
  name <- c("排名","子品牌",paste(2013, 3:1, sep = "-"),
  paste(2012, 12:1, sep = "-"),
  paste(2011, 12:1, sep = "-"),
  paste(2010, 12:1, sep = "-"),
  paste(2009, 12:1, sep = "-"))
  names(b_table1) <- name
  
  createSheet(xls2, name=seg_file)
  writeWorksheet(xls2, b_table1, seg_file, header = T)
  setColumnWidth(xls2, seg_file, column = 1:2, width = -1)
  
  # 调整格式
  b_table2 <- melt(b_table1,  id = c("排名","子品牌"))
  b_table2$year = substr(b_table2$variable, 1, 4)
  b_table2$month = substr(b_table2$variable, 6, 8)
  names(b_table2)[match("value", names(b_table2))] = "sina_vol"
  names(b_table2)[match("子品牌", names(b_table2))] = "model"  
  b_table3 <- b_table2[, c("model","year", "month", "sina_vol")]
  b_table3[, "year"] = as.numeric(b_table3[, "year"])
  b_table3[, "month"] = as.numeric(b_table3[, "month"])
  b_table3[, "sina_vol"] = as.numeric(b_table3[, "sina_vol"])
  b_table3 <- b_table3[order(b_table3[,1], b_table3[,2], b_table3[,3]), ]

  createSheet(xls, name=seg_file)
  writeWorksheet(xls, b_table3, seg_file, header = T)
  setColumnWidth(xls, seg_file, column = 1:4, width = -1)
  print(seg_file)
}
saveWorkbook(xls)
saveWorkbook(xls2)





###################################################
# sohu Vol
web <- "http://db.auto.sohu.com/cxdata/index2.shtml"
a <- htmlParse(web, encoding = "gbk")
a1 <- getNodeSet(a, path = '//ul[@id="list"]')
seglist <- getNodeSet(a, path = '//ul[@id="list"]/li/h4')
cc_final <- character()
for (seg_i  in c(1:5, 7:9)){
  path_c3 <- paste("//ul[@id='list']/li/h4[", seg_i, 
                   "]|//ul[@id='list']/li/dl[", seg_i, 
                   "]/dt|//ul[@id='list']/li/dl[", seg_i, 
                   "]/dd", sep = "")
  c3 <- getNodeSet(a, path = path_c3)
  value <- iconv(sapply(c3, xmlValue), "UTF-8","gbk")
  label <- sapply(seq_len(length(c3)), 
                  function(i){strsplit(xmlGetAttr(xmlChildren(c3[[i]])$a, 'href'), "/")[[1]][2]})
  label2 <- do.call(rbind, strsplit(label, "_"))
  cc0 <- as.data.frame(cbind(label2, value), stringsAsFactors = F)
  names(cc0) <- c("label", "modelid", "model")
  cc <- cc0[-1, ]
  segment <- cc0[1, "model"]
  for (i in 1:length(cc$model)){
    if (cc[i, "label"] == "brand"){
      cc[i, "brand"] = cc[i, "model"];
      cc[i, "brandid"] = cc[i, "modelid"]
      brand  <-  cc[i, "brand"];
      brandid <- cc[i, "brandid"]
    } else {
      cc[i, "brand"] = brand
      cc[i, "brandid"] = brandid
    }
  }
  cc1 <- cc[!duplicated(cc[, "modelid"]), ]
  cc2 <- cbind(segment, cc1)
  cc3 <- subset(cc2, model != brand, select = c("segment", "brandid", "brand", "modelid", "model")) 
  cc_final <- rbind(cc_final, cc3)
}

xls <- loadWorkbook("souhu_modellist.xlsx", create = T)
createSheet(xls, name="vol_modellist")
writeWorksheet(xls, cc_final, "vol_modellist", header = T)
setColumnWidth(xls, "vol_modellist", column = 1:6, width = -1)
saveWorkbook(xls)


model_vol <- character()
for (model_i in 1:dim(cc_final)[1]){
  modelid <- cc_final[model_i, "modelid"]
  model_web <- paste("http://db.auto.sohu.com/cxdata/xml/sales/model/model", modelid, "sales.xml", sep = "")
  b <- htmlParse(model_web, encoding = "gbk")
  b0 <- getNodeSet(b, path = "//model/sales")
  if (length(b0)>0){
    vol_date <- sapply(seq_len(length(b0)), 
                       function(i){iconv(xmlGetAttr(b0[[i]], 'date'), "UTF-8","gbk")})
    vol_salesnum <- sapply(seq_len(length(b0)), 
                           function(i){iconv(xmlGetAttr(b0[[i]], 'salesnum'), "UTF-8","gbk")})
    vol0 <- as.data.frame(cbind(vol_date, vol_salesnum), stringsAsFactors = F)
    vol0$year <- substr(vol0$vol_date, 1, 4)
    vol0$month <- substr(vol0$vol_date, 6, 7)
    vol1 <- cbind(cc_final[model_i, ], vol0[, c(3, 4, 2)])
    model_vol <- rbind(model_vol, vol1)
    print(paste(model_i, cc_final[model_i, "model"]))
  } else{
    print(paste("no vol", model_i, cc_final[model_i, "model"]))
  }

}
summary(model_vol)

model_vol[, "year"] <- as.numeric(model_vol[, "year"])
model_vol[, "month"] <- as.numeric(model_vol[, "month"])
model_vol[, "vol_salesnum"] <- as.numeric(model_vol[, "vol_salesnum"])
xls <- loadWorkbook("souhu_vol.xlsx", create = T)
createSheet(xls, name="vol")
writeWorksheet(xls, model_vol, "vol", header = T)
setColumnWidth(xls, "vol", column = 1:6, width = -1)
saveWorkbook(xls)



###################################################
# get version from Autohome.com

# find the information in this website "http://car.autohome.com.cn/duibi/chexing/"

# model list
web <- "http://car.autohome.com.cn/javascript/NewSpecCompare.js"
a <- htmlParse(web, encoding = "gbk")
a1 <- getNodeSet(a, path = '//p')
a2 <- iconv(xmlValue(a1[[1]]), "UTF-8","gbk")
a3 <- (strsplit(as.character(a2), "=|;"))[[1]][2]
b <- fromJSON(a3)
group_final <- character()
for (group_i in 1:length(b)){
  t1 <- unlist(b[[group_i]])
  make_group <- t(t1[1:3])
  make_location <- which(names(t1) %in% c("List.I", "List.N"))
  make_model_final <- character()
  for (make_i in 1:(length(make_location)/2)) {
    make_i <- 2*make_i - 1
    make <- t1[c(make_location[make_i], make_location[make_i+1])]
    if (make_i == length(make_location) - 1) {
      model <- t1[(make_location[make_i+1] + 1): length(t1)]
    } else {
      model <- t1[(make_location[make_i+1] + 1): (make_location[make_i+2] - 1)] 
    }
    model_id <- model[names(model) %in% c("List.List.I")]
    model_name <- model[names(model) %in% c("List.List.N")]
    model_id_name <- cbind(model_id, model_name)
    make_model <- merge(t(make), model_id_name, all.y = all)
    make_model_final <- rbind(make_model_final, make_model)
  }
  gourp_make_model <- merge(make_group, make_model_final, all.y = all)
  group_final <- rbind(group_final, gourp_make_model)
}
names(group_final) <- c("makegroup_id", "makegroup", "letter", "make_id", "make", "model_id", "model")
xls <- loadWorkbook("汽车之家modellist.xlsx", create = T)
createSheet(xls, "modellist")
writeWorksheet(xls, group_final, "modellist")
setColumnWidth(xls, "modellist", column = 1:6, width = -1)
saveWorkbook(xls)


# version list and msrp
make_model_version_final <- character()
for (model_i in 1:dim(group_final)[1]){
  model_id <- group_final[model_i, "model_id"]
  # get
  web <- paste("http://car.autohome.com.cn/duibi/ashx/SpecCompareHandler.ashx?type=1&seriesid=",
               model_id, "&isie6=0", sep = "")
  a <- htmlParse(web, encoding = "gbk")
  a1 <- getNodeSet(a, path = '//p')
  a2 <- iconv(xmlValue(a1[[1]]), "UTF-8","gbk")
  a3 <- (strsplit(as.character(a2), "=|;"))[[1]][2]
  b <- fromJSON(a2)
  
  # format data
  t1 <- unlist(b)
  modelid <- t1[1]
  sale_location <- which(names(t1) %in% c("List.I", "List.N"))
  model_version <- character()
  for (sale_i in 1:(length(sale_location)/2)) {
    sale_i <- 2*sale_i - 1
    sale_type <- t1[c(sale_location[sale_i], sale_location[sale_i+1])]
    if (sale_i == length(sale_location) - 1) {
      version <- t1[(sale_location[sale_i+1] + 1): length(t1)]
    } else {
      version <- t1[(sale_location[sale_i+1] + 1): (sale_location[sale_i+2] - 1)] 
    }
    version_id <- version[names(version) %in% c("List.List.I")]
    version_name <- version[names(version) %in% c("List.List.N")]
    version_msrp <- version[names(version) %in% c("List.List.P")]
    
    version_all <- cbind(modelid, version_id, version_name, version_msrp)
    
    version_all2 <- merge(t(sale_type), version_all, all.y = all)
    
    model_version <- rbind(model_version, version_all2)
}
  make_model_version <- merge(group_final[model_i, ], model_version, all.y = all)
  make_model_version_final <- rbind(make_model_version_final, make_model_version)
  print(paste(model_i, group_final[model_i, "model"]))
}
# make_model_version_f[1, ]
# make_model_version_f[make_model_version_f$modelid == 364,]
options(java.parameters = "-Xmx4g" )

# add the name of make/model will out of memory, so only keep the id, and vlookup in excel
make_model_version_f <- make_model_version_final[, c(1, 4, 6, 8, 10, 11, 12, 13)]

xls <- loadWorkbook("汽车之家version1.xlsx", create = T)
createSheet(xls, "verrsion")
writeWorksheet(xls, make_model_version_f, "verrsion")
setColumnWidth(xls, "verrsion", column = 1:12, width = -1)
saveWorkbook(xls)





#####  get segmentation of autohome ###
# get seg
web <- "http://www.autohome.com.cn"

a <- htmlParse(web, encoding = "GBK")

segment_raw <- getNodeSet(a, path = '//div[@id="header"]/div[2]/ul/li/a')
segment_name <-  iconv(sapply(segment_raw, xmlValue), "UTF-8","gbk")
segment_id <- sapply(seq_len(length(segment_raw)), 
                     function(i){xmlGetAttr(segment_raw[[i]], 'href')})
seg_list <- cbind(segment_name, segment_id)


# get model by seg
model_all_final <- character()
for (seg_i in 1:dim(seg_list)[1]){ 
  web <- seg_list[seg_i, 2]
  a <- htmlParse(web, encoding = "GBK")
  a1 <- getNodeSet(a, path = '//div[@class="pricearea"]/dl/dt/div/a|//div[@class="pricearea"]/dl/dd/ul/li/h4/a')
  name <- iconv(sapply(a1, xmlValue), "UTF-8","gbk")
  id <- sapply(seq_len(length(a1)), 
               function(i){xmlGetAttr(a1[[i]], 'href')})
  model_all <- as.data.frame(cbind(name, id), stringsAsFactors = F)
  for (i in 1:dim(model_all)[1]){
    if (substr(model_all[i, "id"], 1, 4) == "http") {
      model_all[i, "brand"]  <- model_all[i, "name"]
      model_all[i, "brandid"] <- gsub(".*-([0-9]+)..*", '\\1', model_all[ i , 2])
    } else {
      model_all[i, "brand"]  <- model_all[i - 1, "brand"]
      model_all[i, "brandid"] <- model_all[i - 1, "brandid"]
    }
  }
  model_all2 <- merge(seg_list[seg_i, 1], model_all[model_all$name != model_all$brand, ])
  model_all_final <- rbind(model_all_final, model_all2)
}
names(model_all_final) <- c("autohome_seg", "autohome_model", "autohome_modelid", "autohome_brand", "autohome_brandid")
model_all_final[ , "autohome_modelid"] <- gsub("/", "", model_all_final[ , "autohome_modelid"])

# only for SUV segmentation
web <- "http://www.autohome.com.cn"
a <- htmlParse(web, encoding = "GBK")
suv_raw <- getNodeSet(a, path = '//div[@id="div-suv-list"]/dl/dd/a')
suv_name <-  iconv(sapply(suv_raw, xmlValue), "UTF-8","gbk")
suv_id <- sapply(seq_len(length(suv_raw)), 
                      function(i){xmlGetAttr(suv_raw[[i]], 'href')})
suv_list <- cbind(suv_name, suv_id)

web <- "http://www.autohome.com.cn/suv/"
a <- htmlParse(web, encoding = "GBK")
suv_all_final <- character()
for (suv_i in 2:6){  
  a1 <- getNodeSet(a, path = paste('//div[@id="page3"]/div[', suv_i, ']/div/ul/li/h4/a', sep = ""))
  name <- iconv(sapply(a1, xmlValue), "UTF-8","gbk")
  id <- sapply(seq_len(length(a1)), 
               function(i){xmlGetAttr(a1[[i]], 'href')})
  suv_all <- as.data.frame(merge(suv_list[ suv_i - 1, 1] ,cbind(name, id), stringsAsFactors = F))
  suv_all_final <- rbind(suv_all_final, suv_all)
}
names(suv_all_final) <- c("suv_seg", "autohome_model", "autohome_modelid")
suv_all_final[ , "autohome_modelid"] <- gsub("/", "", suv_all_final[ , "autohome_modelid"])



xls <- loadWorkbook("汽车之家segmentation.xlsx", create = T)
createSheet(xls, "segmentation")
writeWorksheet(xls, model_all_final, "segmentation")
setColumnWidth(xls, "segmentation", column = 1:5, width = -1)
createSheet(xls, "suv_seg")
writeWorksheet(xls, suv_all_final, "suv_seg")
setColumnWidth(xls, "suv_seg", column = 1:3, width = -1)
saveWorkbook(xls)




#####  download parameters of main model in autohome ###
web <- "http://car.autohome.com.cn/duibi/chexing/carids=14941,0,0,0"
doc = htmlParse(web, encoding = "gbk")
tableNodes = getNodeSet(doc, "//table")
tb = readHTMLTable(tableNodes[[4]])
apply(tb, 2, function(x)iconv(x, "UTF-8","gbk"))






