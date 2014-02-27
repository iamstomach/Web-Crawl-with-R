# check the model ----
test_all <- character(0)
for (segid in 1:dim(MDM.segment)[1]){
  #  for (segid in c(2:10)){
  #  segid=1
  segname <- MDM.segment[segid,]
  MDM.models.select <- MDM.models[MDM.models[,"segment"] == MDM.segment[segid,],"ModelID"]
  MDM.models.select <- unique(MDM.models.select[!is.na(MDM.models.select)])  
  for (veh.n in MDM.models.select){
    #    veh.n=1
    my <- strsplit(as.character(ModelList.all[veh.n,5]),",")[[1]]
    my <- my[my>=2000]
    makename <- as.character(ModelList.all[veh.n,2])
    modelname <- as.character(ModelList.all[veh.n,4])
    veh <- paste(tolower(makename), '/', tolower(modelname), '/', sep = '')
    equip.URLname.List0 <- paste("http://www.cars.com/", veh, my, '/', sep = '')
    for (my.n in 1:length(equip.URLname.List0)){
      #    my.n <- 1
      URLname <- equip.URLname.List0[my.n]
      URLname <- gsub(" +", "-", URLname)
      URLname <- gsub("&", "and", URLname)
      URLname <- "http://www.cars.com/chevrolet/hhr/2009/"
      b0 <- getURL(URLname) 
      b1 = htmlParse(b0, asText = TRUE, encoding = 'UTF-8')
      (test <- getNodeSet(doc = b1, path = '//div[@id="switcher"]'))
      if (length(test)>0) {
        test_all <- rbind(test_all, URLname)
      }
      print(URLname)
    }
  }
}
write.table(test_all, "test_all1.txt")

need_name <- read.delim("test_all2.txt", sep = " ", header = T)
record_all <- character(0)
for (my.n in 1:dim(need_name)[1]){
  #    my.n <- 1
  URLname <- need_name[my.n, 2]
  URLname <- gsub(" +", "-", URLname)
  URLname <- gsub("&", "and", URLname)
  #  URLname <- "http://www.cars.com/chevrolet/hhr/2009/"
  b0 <- getURL(URLname) 
  b1 = htmlParse(b0, asText = TRUE, encoding = 'UTF-8')
  (test <- getNodeSet(doc = b1, path = '//div[@id="switcher"]//span'))
  
  name22 <- gsub('\\r|\\n|\\"|(^\\s*)|(\\s*$)', "", sapply(test, xmlValue))
  record <- cbind(name22[1], URLname)
  record_all <- rbind(record_all, record)
  print(URLname)
}
write.csv(record_all, "record_all.csv")
read.csv("record_all2.csv")



library(stringr)
# get Standard-equipment ----
setwd("M:/Car.com/MDM Segment/Add")

StartTime <- Sys.time()
Equip.NoRecords.List <- character(0)
new_name <- read.csv("record_all2.csv")
record_all <- character(0)
for (my.n in 1:dim(new_name)[1]){
  #    my.n <- 1
  URLname <- new_name[my.n, 3]
  URLname <- gsub(" +", "-", URLname)
  URLname <- gsub("&", "and", URLname)
  #  URLname <- "http://www.cars.com/chevrolet/hhr/2009/"
  b0 <- getURL(URLname) 
  b1 = htmlParse(b0, asText = TRUE, encoding = 'UTF-8')
  (test <- getNodeSet(doc = b1, path = '//div[@id="switcher"]//span'))
  name22_value <- sapply(seq_len(length(test)),
                        function(i){xmlGetAttr(xmlChildren(test[[i]])$label, "for")})
  name22 <- gsub('\\r|\\n|\\"|(^\\s*)|(\\s*$)', "", sapply(test, xmlValue))
  name33 <- cbind(URLname, name22, name22_value)
  record_all <- rbind(record_all, name33)
  print(URLname)
}
write.csv(record_all, "record_all3.csv", row.names=F )


new_name2 <- read.csv("record_all4.csv")
equip.URLname.List0 <- paste(new_name2[,2], 'standard-equipment/?myid=', str_trim(substr(new_name2[,4], 8, 14)), sep = '')
for (my.n in 1:length(equip.URLname.List0)){
        #    my.n <- 24
        URLname <- equip.URLname.List0[my.n]
        URLname <- gsub(" +", "-", URLname)
        URLname <- gsub("&", "and", URLname)
        b0 <- getURL(URLname) 
        b1 = htmlParse(b0, asText = TRUE, encoding = 'UTF-8')
        
        equipList <- getNodeSet(doc = b1, path = '//table/tbody/tr/td/h2|//td[@class="rowHeader"]')
        equip.name <- as.character(sapply(equipList, xmlValue))
        if (length(equipList)==0){
          print(paste('No records',segname, URLname))
          Equip.NoRecords.List <- rbind(Equip.NoRecords.List, paste(segname, URLname))
        } else {
          SRSList <- getNodeSet(doc = b1, path = '//table/thead/tr//div[@class="trimHeaderName"]/h2')
          SRSList.value <- getNodeSet(doc = b1, path = '//table/thead/tr//div[@class="trimHeaderName"]/span')        
          trim.value.base <- character(0)
          for (trim.n in 1:length(SRSList)+1){
            trim.value <- getNodeSet(doc = b1, path = paste('//table/tbody/tr/td/h2|//table/tbody/tr[position()>1]/td[', trim.n, ']', sep=''))        
            trim.value1 <- sapply(seq_len(length(trim.value)),
                                  function(i){ifelse(is.null(xmlChildren(trim.value[[i]])$img), 
                                                     xmlValue(trim.value[[i]]), xmlGetAttr(xmlChildren(trim.value[[i]])$img, 'src'))})
            trim.value1 <- gsub('\\r|\\t', "", trim.value1)
            trim.value1[trim.value1==c("http://www.cars.com/go/configurator/images/standard.gif")]='Std'
            trim.value1[trim.value1==c("http://www.cars.com/go/configurator/images/available.gif")]='Opt'
            trim.value1[trim.value1==c("http://www.cars.com/go/configurator/images/notavailable.gif")]='Not'
            trim.value.base <- cbind(trim.value.base, trim.value1)
          }
          trim.name <- sapply(SRSList, xmlValue)
          MSRP <- sapply(SRSList.value, xmlValue)
          
          equip.value <- rbind(cbind("MSRP", t(MSRP)), cbind(equip.name,trim.value.base))
          
          colnames(equip.value) <- c("trim", trim.name)
          # step4: export data to excel file
          file.name <- paste("Equip add", ".xlsx", sep = "")
          sheet.name  <- as.character(new_name2[my.n,1])
          xls <- loadWorkbook(file.name, create=TRUE)
          createSheet(xls, name=sheet.name)
          writeWorksheet(xls, equip.value, sheet.name, header=TRUE)
          setColumnWidth(xls, sheet.name, column = 1:(length(SRSList)+2), width = -1)
          saveWorkbook(xls)
          print(paste(URLname))
        }
      }
  
(UseTime <- Sys.time() - StartTime)
Equip.NoRecords.List <- as.data.frame(Equip.NoRecords.List)
write.table(Equip.NoRecords.List, "Equip-NoRecords-List.txt")
  
################ get specification ----
  setwd("M:/Car.com/MDM Segment")
  dir.create("Specification")
  setwd("M:/Car.com/MDM Segment/Add")
  StartTime <- Sys.time()
  NoRecords.List <- character(0)
  Style.NoRecords.List <- character(0)
  list30 <- character(0)
spec.URLname.List0 <- paste(new_name2[,2], 'specifications/?myid=', str_trim(substr(new_name2[,4], 8, 14)), sep = '')
for (Sty.n0 in 1:length(spec.URLname.List0)){
  for (Sty.n0 in 19:19){
  spec.URLname0 <- spec.URLname.List0[Sty.n0];
  spec.URLname0 <- gsub(" +", "-", spec.URLname0)
  spec.URLname0 <- gsub("&", "and", spec.URLname0)
  spec <- getURL(spec.URLname0) 
  spec1 = htmlParse(spec, asText = TRUE, encoding = 'utf-8')
  StyleList <- getNodeSet(doc = spec1, path = '//div/ul[@id="trimpop"]/li')
  if (is.null(StyleList)){
    print(paste('No records',segname, spec.URLname0))
    NoRecords.List <- rbind(NoRecords.List, paste(segname, spec.URLname0))
  } else {
    
    StyleList1 = sapply(StyleList, xmlValue)[1:length(StyleList)]
    StyleList1.final <- StyleList1[-1]
    StyleList2 <- getNodeSet(doc = spec1, path = '//div/ul[@id="trimpop"]')
    
    #        try.test <- try(StyleID <- sapply(seq(2, length(StyleList))*2-1,
    #                              function(i){xmlAttrs(xmlChildren(StyleList2[[1]])[[i]])[['id']]}),
    #            silent = T)
    StyleID <- sapply(seq(2, length(StyleList))*2-1,
                      function(i){xmlAttrs(xmlChildren(StyleList2[[1]])[[i]])[['id']]})
    #save style list
    StyleList3 <- cbind(StyleID, StyleList1.final)
    colnames(StyleList3) = c("StyleID", "Style")
    file.name <- paste(paste("Spec", as.character(new_name2[Sty.n0,1])), ".xlsx", sep = "")
    xls <- loadWorkbook(file.name, create=TRUE)
    createSheet(xls, name="Style List")
    writeWorksheet(xls, StyleList3, "Style List", header=TRUE)
    setColumnWidth(xls, "Style List", column = 1:2, width = -1)
    saveWorkbook(xls)
    if (length(StyleID)>25) {
      file.name <- paste(paste("Spec", as.character(new_name2[Sty.n0,1]), "-2"), ".xlsx", sep = "")
      xls <- loadWorkbook(file.name, create=TRUE)
      createSheet(xls, name="Style List")
      writeWorksheet(xls, StyleList3, "Style List", header=TRUE)
      setColumnWidth(xls, "Style List", column = 1:2, width = -1)
      saveWorkbook(xls)
      list30 <- rbind(list30, paste(segname, spec.URLname0))
    }
    
    
    spec.URLname.List <- paste(spec.URLname0,"&acode=", substr(StyleID, 7, 20), sep = '')
    
    
    for (Sty.n in 1:length(spec.URLname.List)){
      #        for (Sty.n in 22:28){
      spec.URLname <- spec.URLname.List[Sty.n]
      spec.URLname <- gsub(" +", "-", spec.URLname)
      # spec.URLname <- gsub("&", "and", spec.URLname)
      spec <- getURL(spec.URLname)
      spec = iconv(spec, 'gbk', 'utf-8')
      spec1 = htmlParse(spec, asText = TRUE, encoding = 'UTF-8')
      # more than one style
      Engine.specname <- getNodeSet(doc = spec1, path = '//div[@id="specifications"]//div[@class="module-body"]/div[2]/h2|//div[@id="specifications"]//div[@class="module-body"]/div[2]//span[@class="spec-name"]')
      Transm.specname <- getNodeSet(doc = spec1, path = '//div[@id="specifications"]//div[@class="module-body"]/div[3]/h2|//div[@id="specifications"]//div[@class="module-body"]/div[3]//span[@class="spec-name"]')
      Other.specname <- getNodeSet(doc = spec1, path = '//div[@id="specifications"]//div[@class="module-body"]/div[4]/h2|//div[@id="specifications"]//div[@class="module-body"]/div[4]//span[@class="spec-name"]')
      if (is.null(Engine.specname) & is.null(Transm.specname) & is.null(Other.specname)){
        print(paste('No records',segname, spec.URLname))
        Style.NoRecords.List <- rbind(Style.NoRecords.List, paste(segname, spec.URLname))
      } else {
        spec.Engine <- sapply(Engine.specname,xmlValue)
        spec.Transm <- sapply(Transm.specname,xmlValue)
        spec.result1 <- cbind(rbind(t(t(spec.Engine)), t(t(spec.Transm))), "")
        spec.result1[spec.result1[,1] == "Engines", 2] = "Engines"
        spec.result1[spec.result1[,1] == "Transmissions", 2] = "Transmissions"
        
        Other.specdetail <- getNodeSet(doc = spec1, path = '//div[@id="specifications"]//div[@class="module-body"]/div[4]/h2|//div[@id="specifications"]//div[@class="module-body"]/div[4]//span[@class="spec-detail float-right"]')
        spec.Other.name <- sapply(Other.specname,xmlValue)
        spec.Other.detail <- sapply(Other.specdetail,xmlValue)
        kk_final <- rbind(spec.result1, cbind(spec.Other.name, spec.Other.detail))
        kk_final <- gsub('\\r|\\n|\\"|(^\\s*)|(\\s*$)', "",kk_final)
        # get Style name
        stylename <- getNodeSet(doc = spec1, path = '//div[@id="specifications"]//div[@class="module-body"]/p/strong')
        kk_final2 <- rbind(c("Style Name", sapply(stylename,xmlValue)), kk_final)
        
        colnames(kk_final2) <- c("SpecName", "SpecDetail")
        
        # export data to excel file
        if (Sty.n>25) {
          file.name <- paste(paste("Spec",as.character(new_name2[Sty.n0,1]), "-2"), ".xlsx", sep = "")
          xls <- loadWorkbook(file.name, create=TRUE)
          sheet.name  <- paste(StyleID[Sty.n])
          createSheet(xls, name=sheet.name)
          writeWorksheet(xls, kk_final2, sheet.name, header=TRUE)
          setColumnWidth(xls, sheet.name, column = 1:2, width = -1)
          saveWorkbook(xls)
        } else {
          file.name <- paste(paste("Spec",as.character(new_name2[Sty.n0,1])), ".xlsx", sep = "")
          xls <- loadWorkbook(file.name, create=TRUE)
          sheet.name  <- paste(StyleID[Sty.n])
          createSheet(xls, name=sheet.name)
          writeWorksheet(xls, kk_final2, sheet.name, header=TRUE)
          setColumnWidth(xls, sheet.name, column = 1:2, width = -1)
          saveWorkbook(xls)
        }
        
        print(spec.URLname)
      }
    }
  }
}

    (UseTime <- Sys.time() - StartTime)
    write.table(NoRecords.List, "Spec-NoRecords-List4.txt")
    write.table(Style.NoRecords.List, "Style-NoRecords-List4.txt")
    write.table(list30, "list304.txt") 

################# get warranty ----
setwd("M:/Car.com/MDM Segment")
dir.create("Warranty")
setwd("M:/Car.com/MDM Segment/Warranty")
StartTime <- Sys.time()
Warr.NoRecords.List <- character(0)
new_name2 <- read.csv("record_all4.csv")
warr.URLname.List <- paste(new_name2[,2], 'warranty/?myid=', str_trim(substr(new_name2[,4], 8, 14)), sep = '')
for (warr.n in 1:length(warr.URLname.List)){ 
  warr.URLname <- warr.URLname.List[warr.n]
  warr.URLname <- gsub(" +", "-", warr.URLname)
  warr.URLname <- gsub("&", "and", warr.URLname)
  warr <- getURL(warr.URLname)
  warr = iconv(warr, 'gbk', 'utf-8')
  warr1 = htmlParse(warr, asText = TRUE, encoding = 'UTF-8')
  
  warr.specname <- getNodeSet(doc = warr1, path = '//div[@id="warranty"]//div[@class="module-body"]/div//span[@class="spec-name"]')
  if (length(warr.specname)==0){
    print(paste('No records', segname, warr.URLname))
    Warr.NoRecords.List <- rbind(Warr.NoRecords.List, paste(segname, warr.URLname))
  } else {
    warr.specdetail <- getNodeSet(doc = warr1, path = '//div[@id="warranty"]//div[@class="module-body"]/div//span[@class="spec-detail float-right"]')
    spec.warr.name <- sapply(warr.specname,xmlValue)
    spec.warr.detail <- sapply(warr.specdetail,xmlValue)
    warr_final <- cbind(spec.warr.name, spec.warr.detail)
    warr_final <- gsub('\\r|\\n|\\"|(^\\s*)|(\\s*$)', "",warr_final)
    colnames(warr_final) <- c("WarrantyName", "WarrantyDetail")
    # step4: export data to excel file
    file.name <- paste("Warranty add", ".xlsx", sep = "")
    sheet.name  <- as.character(new_name2[warr.n,1])
    xls <- loadWorkbook(file.name, create=TRUE)
    createSheet(xls, name=sheet.name)
    writeWorksheet(xls, warr_final, sheet.name, header=TRUE)
    setColumnWidth(xls, sheet.name, column = 1:(length(SRSList)+2), width = -1)
    saveWorkbook(xls)
    print(paste(warr.URLname))       
    }
  }   

(UseTime <- Sys.time() - StartTime)
Warr.NoRecords.List <- as.data.frame(Warr.NoRecords.List)
write.table(Warr.NoRecords.List, "Warr-NoRecords-List.txt")
        
################# get Safety Rating ----
setwd("M:/Car.com/MDM Segment")
dir.create("Safety Rating")
setwd("M:/Car.com/MDM Segment/Safety Rating")

StartTime <- Sys.time()

rate.NoRecords.List <- character(0)
NHTSA.NoRecords.List <- character(0)
IIHS.NoRecords.List <- character(0)
rate.URLname.List <- paste(new_name2[,2], 'safety-ratings/?myid=', str_trim(substr(new_name2[,4], 8, 14)), sep = '')
      
for (rate.n in 1:length(rate.URLname.List)){
    rate.URLname <- rate.URLname.List[rate.n]
    rate.URLname <- gsub(" +", "-", rate.URLname)
    rate.URLname <- gsub("&", "and", rate.URLname)
    rate <- getURL(rate.URLname)
    rate = iconv(rate, 'gbk', 'utf-8')
    rate1 = htmlParse(rate, asText = TRUE, encoding = 'UTF-8')
    
    rate.specname <- getNodeSet(doc = rate1, path = '//div[@id="nhtsa-ratings"]//div[@class="module-body"]//div[@class="rowbottomborder nhtsaDataStyle"]|//div[@class="nhtsaDataStyle"]')
    rate.specname22 <- getNodeSet(doc = rate1, path = '//div[@id="iihs-ratings"]//div[@class="module-body"]//div[@class="iihsDataStyle"]|//div[@class="rowbottomborder"]')
    if (length(rate.specname)==0 & length(rate.specname22)==0) {
      print(paste('All No records', rate.URLname))
      rate.NoRecords.List <- rbind(rate.NoRecords.List, paste(rate.URLname))
    } else {
      # NHTSA Crash-Test Ratings
      if (length(rate.specname)==0){
        print(paste('NHTSA No records', rate.URLname))
        NHTSA.NoRecords.List <- rbind(NHTSA.NoRecords.List, paste(rate.URLname))
      } else {
        rate.specdetail <- getNodeSet(doc = rate1, path = '//div[@id="nhtsa-ratings"]//div[@class="module-body"]//div[@class="rowbottomborder nhtsaDataStyle"]/div/span|//div[@class="nhtsaDataStyle"]/div/span')
        spec.rate.name <- sapply(rate.specname,xmlValue)
        spec.rate.detail <- sapply(seq(1, length(rate.specdetail)),
                                   function(i){substr(xmlAttrs(rate.specdetail[[i]])[['style']], 8, 11)})
        rate.header <- sapply(getNodeSet(doc = rate1, path = '//div[@id="nhtsa-ratings"]//div[@class="module-header"]/h3'),xmlValue)
        rate.type <- strsplit(rate.header, " ")[[1]][1]
        rate_final <- cbind(rate.type, spec.rate.name, spec.rate.detail)
        rate_final <- gsub('\\r|\\n|\\"|(^\\s*)|(\\s*$)', "",rate_final)
        colnames(rate_final) <- c("RatingType", "RatingName", "Rating")
        # change to 5-4-3-2-1
        rate_final[rate_final[,3]=="83.7", 3]=5
        rate_final[rate_final[,3]=="67.0", 3]=4
        rate_final[rate_final[,3]=="50.2", 3]=3
        rate_final[rate_final[,3]=="33.5", 3]=2
        rate_final[rate_final[,3]=="16.7", 3]=1
      }
      
      # IIHS Crash-Test Data
      if (length(rate.specname22)==0){
        print(paste('IIHS No records', rate.URLname))
        IIHS.NoRecords.List <- rbind(IIHS.NoRecords.List, paste(rate.URLname))
        
      } else {
        rate.specdetail22 <- getNodeSet(doc = rate1, path = '//div[@id="iihs-ratings"]//div[@class="module-body"]//div[@class="iihsDataStyle"]|//div[@id="iihs-ratings"]//div[@class="module-body"]/div/span')
        spec.rate.name22 <- sapply(rate.specname22,xmlValue)
        spec.rate.detail22 <- sapply(rate.specdetail22,xmlValue)
        rate.header22 <- sapply(getNodeSet(doc = rate1, path = '//div[@id="iihs-ratings"]//div[@class="module-header"]/h3'),xmlValue)
        rate.type22 <- strsplit(rate.header22, " ")[[1]][1]
        rate_final22 <- cbind(rate.type22, spec.rate.name22, spec.rate.detail22)  
        rate_final22 <- gsub('\\r|\\n|\\"|(^\\s*)|(\\s*$)', "",rate_final22)
        colnames(rate_final22) <- c("RatingType", "RatingName", "Rating")
      }
      flag <- paste(length(rate.specname)==0, length(rate.specname22)==0, sep = " and ")
      rate_final33 <- switch(flag,
                             "FALSE and TRUE" = rate_final,
                             "TRUE and FALSE" = rate_final22,
                             "FALSE and FALSE" = rbind(rate_final, rate_final22))
      # step4: export data to excel file
      file.name <- paste("Safety-Ratings add", ".xlsx", sep = "")
      sheet.name  <- as.character(new_name2[rate.n,1])
      xls <- loadWorkbook(file.name, create=TRUE)
      createSheet(xls, name=sheet.name)
      writeWorksheet(xls, rate_final33, sheet.name, header=TRUE)
      setColumnWidth(xls, sheet.name, column = 1:(length(SRSList)+2), width = -1)
      saveWorkbook(xls)
      print(paste(rate.URLname))  
    }
  }

  (UseTime <- Sys.time() - StartTime)
  
  write.table(rate.NoRecords.List, "Allrate-NoRecords-List.txt")
  write.table(NHTSA.NoRecords.List, "NHTSA-NoRecords-List.txt")
  write.table(IIHS.NoRecords.List, "IIHS-NoRecords-List.txt")
  
  
  
  # change ford name
  library(stringr)
  setwd("M:/Car.com/Safety Rating")
  fordnames <- list.files()
  MDM.models0 <- MDM.models[str_trim(MDM.models[,"Make"])=="FORD",]
  MDM.models1 <- unique(cbind(paste(paste(MDM.models0[,6], "Safety-Ratings"), ".xlsx", sep=""), 
                              paste(paste("Safety-Ratings", MDM.models0[,2], MDM.models0[,6]), ".xlsx", sep = ""))) 
  for (i in 1:dim(MDM.models1)[1]){
    eval(parse(text = paste0("file.rename('", MDM.models1[i,1], "' , '", MDM.models1[i,2], "')")))
  }
          
# Add to old data
  library(stringr)
  new_name3 <- read.csv("record_all4.csv", stringsAsFactors = F)
  dataid <- "Equip"
  for (nid in 3:17){
    # load
    setwd("M:/Car.com/MDM Segment/Add")
    wb <- loadWorkbook(paste(dataid, "add.xlsx"))
    if (existsSheet(wb, new_name3[nid,"newname"])){
      data <- readWorksheet(wb, sheet = new_name3[nid,"newname"])
      # add
      setwd(paste("M:/Car.com/MDM Segment/", "Equipment", sep = ""))  
      wb2 <- loadWorkbook(paste(paste(dataid, new_name3[nid ,"file"]), ".xlsx", sep = ""))
      clearSheet(wb2, sheet = paste(new_name3[nid ,"filesheet"]))
      createSheet(wb2, paste(new_name3[nid ,"filesheet"]))
      writeWorksheet(wb2, data, paste(new_name3[nid ,"filesheet"]), header=TRUE)
      setColumnWidth(wb2, paste(new_name3[nid ,"filesheet"]), column = 1:20, width = -1)
      saveWorkbook(wb2)
      print(new_name3[nid,"newname"])
    }
  }

  
  load
  file_names <- "M:/Car.com/MDM Segment/Specification/SPD_TRUCK/Ford F350"
  setwd(file_names)
  (fordnames <- list.files())
  (newfordnames <- paste("Spec SPD_TRUCK", gsub(" Spec", "", fordnames)))
  for (i in 1:length(fordnames)){
    eval(parse(text = paste0("file.rename('", fordnames[i], "' , '", newfordnames[i], "')")))
  }
  
  # change ford name in Specification
  library(stringr)
  file_names <- "M:/Car.com/MDM Segment/Warranty"
  setwd(file_names)
  (fordnames <- list.files())
  file.create("M:/Car.com/MDM Segment/DownloadModelList.xlsx")
  
  library("xlsx")
  setwd("M:/Car.com/MDM Segment")
  write.xlsx(as.data.frame(fordnames), "DownloadModelList.xlsx", sheetName="Warranty", append = T)
        
          
          
          