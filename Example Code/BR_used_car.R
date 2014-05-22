setwd("M:\\02.Working\\9.Download Data\\BR Old Car")

require(RCurl)
require(XML)

######################copy request header in Chorme toolkit#######################
#### http://www.fipe.org.br/web/index.asp?p=51&aspx=/web/indices/veiculos/default.aspx
#### find element "default.aspx?p=51" in "document"

# Host: www.fipe.org.br
# Connection: keep-alive
# Cache-Control: max-age=0
# Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8
# User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/33.0.1750.117 Safari/537.36
# Referer: http://www.fipe.org.br/web/index.asp?p=51&aspx=/web/indices/veiculos/default.aspx
# Accept-Encoding: gzip,deflate,sdch
# Accept-Language: en-US,en;q=0.8,zh-CN;q=0.6,zh;q=0.4
# Cookie: ASPSESSIONIDSQBTCDDT=HCMHIEGBDMGIGCLFCJKEKNHC; __utmz=106123796.1393812852.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); ASPSESSIONIDSSCSBDDT=LINLDNACJJIFIJCEODJJNOOL; __utma=106123796.1314711177.1393812852.1393827001.1393830974.3; __utmc=106123796; __utmb=106123796
###################################################################################

# step1: create http header, delete cookie ----
# myhead <- function(x){
#   post <- scan(x, what="character",quiet=TRUE,sep="\n")
#   abcd=unlist(post)
#   abc=gsub("(^.*)(: )(.*$)","\\3",abcd)
#   abcnames=gsub("(^.*)(: )(.*$)","\\1",abcd)
#   names(abc)=abcnames
#   return(abc)
# }
# myHttpheader <- myhead("clipboard")

# following is the http header
myHttpheader <- list(
  "Host"="www.fipe.org.br",
  "Connection"="keep-alive",
  "Cache-Control"= "max-age=0",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
  "User-Agent"='Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/33.0.1750.117 Safari/537.36',
  'Referer'= 'http://www.fipe.org.br/web/index.asp?p=51&aspx=/web/indices/veiculos/default.aspx',
  "Accept-Encoding" ='gzip,deflate,sdch',
  "Accept-Language" ='en-US,en;q=0.8,zh-CN;q=0.6,zh;q=0.4'
)

# step2: get __VIEWSTATE and  __EVENTVALIDATION of asp.net  ---- 
ch <- getCurlHandle(verbose = F, 
                    httpheader=myHttpheader)
curlSetOpt(cookiejar = 'cookies.txt',
           followlocation = TRUE, 
           verbose = F, 
           autoreferer = TRUE, 
           curl = ch)
url <- 'http://www.fipe.org.br/web/indices/veiculos/default.aspx?azxp=1'
doc <- getURL(url, curl=ch, .encoding = "utf-8")
txt <- htmlParse(doc)
VIEWSTATE <- xpathApply(txt, path = "//form//input[@name='__VIEWSTATE']",
                    xmlGetAttr, "value")[[1]]
EVENTVALIDATION <- xpathApply(txt, path = "//form//input[@name='__EVENTVALIDATION']",
                        xmlGetAttr, "value")[[1]]

# get monthlist
monweb <- getNodeSet(txt, "//select[@id='ddlTabelaReferencia']/option")
monlist <- do.call(rbind, lapply(monweb, function(x) c(aa=xmlValue(x), bb=xmlGetAttr(x, "value"))))

# get makelist
makeweb <- getNodeSet(txt, "//select[@id='ddlMarca']/option")
makeist <- do.call(rbind, lapply(makeweb, function(x) c(aa=xmlValue(x), bb=xmlGetAttr(x, "value"))))


# step3: post __VIEWSTATE and  __EVENTVALIDATION and return the information  ----

mmk <- makeist[makeist[,2] %in% c(59, 56, 48, 44, 43, 41, 31, 26, 25, 23, 22, 21, 13),]
mmn <- head(monlist, 17)

mon_select <- 150

mon_return <- postForm(url,
                 "ScriptManager1" = "ScriptManager1|ddlTabelaReferencia",
                 "__EVENTTARGET" = "ddlTabelaReferencia",
                 "__EVENTARGUMENT" = "",
                 "__LASTFOCUS" = "",
                 "__VIEWSTATE" = VIEWSTATE,
                 "__EVENTVALIDATION" = EVENTVALIDATION,  
                 "ddlTabelaReferencia"= mon_select,
                 "txtCodFipe" = "",
                 "ddlMarca" = 0, 
                 "ddlModelo" = 0,
                 "ddlAnoValor" = 0,
                 "__ASYNCPOST" = "true",
                 style = 'post', curl = ch, .encoding = "utf-8")
tt <- as.character(mon_return)
VIEWSTATE <- gsub('.*__VIEWSTATE\\|(.*)\\|\\d+\\|hiddenField\\|__EVENTVALIDATION\\|(.*)\\|\\d+\\|asyncPostBackControlIDs.*', "\\1", tt)
EVENTVALIDATION <- gsub('.*__VIEWSTATE\\|(.*)\\|\\d+\\|hiddenField\\|__EVENTVALIDATION\\|(.*)\\|\\d+\\|asyncPostBackControlIDs.*', "\\2", tt)


make_id <- 44

mod_id <- 0
my_id <- 0
make_return <- postForm(url,
                       "ScriptManager1" = "UdtMarca|ddlMarca",
                       "__EVENTTARGET" = "ddlMarca",
                       "__EVENTARGUMENT" = "",
                       "__LASTFOCUS" = "",
                       "__VIEWSTATE" = VIEWSTATE,
                       "__EVENTVALIDATION" = EVENTVALIDATION,  
                       "ddlTabelaReferencia"= mon_select,
                       "txtCodFipe" = "",
                       "ddlMarca" = make_id, 
                       "ddlModelo" = mod_id,
                       "ddlAnoValor" = my_id,
                       "__ASYNCPOST" = "true",
                       style = 'post', curl = ch, .encoding = "utf-8")
tt <- as.character(make_return)
VIEWSTATE <- gsub('.*__VIEWSTATE\\|(.*)\\|\\d+\\|hiddenField\\|__EVENTVALIDATION\\|(.*)\\|\\d+\\|asyncPostBackControlIDs.*', "\\1", tt)
EVENTVALIDATION <- gsub('.*__VIEWSTATE\\|(.*)\\|\\d+\\|hiddenField\\|__EVENTVALIDATION\\|(.*)\\|\\d+\\|asyncPostBackControlIDs.*', "\\2", tt)

# get modellist
tt1 <- gsub('.*\\|[0-9]+\\|updatePanel\\|updModelo\\|\\r\\n(.*)\\|[0-9]+\\|updatePanel\\|updAnoValor\\|\\r\\n.*', "\\1\\2", tt)
tt2 <- htmlParse(tt1, encoding = "UTF-8")
modellist <- do.call(rbind, lapply(getNodeSet(tt2, "//option[@value!='0']"), function(x) c(aa=xmlValue(x), bb=xmlGetAttr(x, "value"))))

tp_j <- character()

############# Loop-model
for (j in 1:dim(modellist)[1]){
  mod_id <- modellist[j, 2]
  model_return <- postForm(url,
                           "ScriptManager1" = "updModelo|ddlModelo",
                           "__EVENTTARGET" = "ddlModelo",
                           "__EVENTARGUMENT" = "",
                           "__LASTFOCUS" = "",
                           "__VIEWSTATE" = VIEWSTATE,
                           "__EVENTVALIDATION" = EVENTVALIDATION,  
                           "ddlTabelaReferencia"= mon_select,
                           "txtCodFipe" = "",
                           "ddlMarca" = make_id, 
                           "ddlModelo" = mod_id,
                           "ddlAnoValor" = my_id,
                           "__ASYNCPOST" = "true",
                           style = 'post', curl = ch, .encoding = "utf-8")
  tt <- as.character(model_return)
  VIEWSTATE <- gsub('.*__VIEWSTATE\\|(.*)\\|\\d+\\|hiddenField\\|__EVENTVALIDATION\\|(.*)\\|\\d+\\|asyncPostBackControlIDs.*', "\\1", tt)
  EVENTVALIDATION <- gsub('.*__VIEWSTATE\\|(.*)\\|\\d+\\|hiddenField\\|__EVENTVALIDATION\\|(.*)\\|\\d+\\|asyncPostBackControlIDs.*', "\\2", tt)
  print(modellist[j, 1])
  
  ############# Loop-MY
  tt1 <- gsub('.*\\|[0-9]+\\|updatePanel\\|updAnoValor\\|\\r\\n(.*)\\|[0-9]+\\|updatePanel\\|updValor\\|\\r\\n.*', "\\1", tt)
  tt2 <- htmlParse(tt1, encoding = "UTF-8")
  mylist <- do.call(rbind, lapply(getNodeSet(tt2, "//option[@value!='0']"), function(x) c(aa=xmlValue(x), bb=xmlGetAttr(x, "value"))))
  
  tp_i <- character()
  for (i in 1:dim(mylist)[1]){
    my_id <- mylist[i, 2]
    data_return <- postForm(url,
                            "ScriptManager1" = "updAnoValor|ddlAnoValor",
                            "__EVENTTARGET" = "ddlAnoValor",
                            "__EVENTARGUMENT" = "",
                            "__LASTFOCUS" = "",
                            "__VIEWSTATE" = VIEWSTATE,
                            "__EVENTVALIDATION" = EVENTVALIDATION,  
                            "ddlTabelaReferencia"= mon_select,
                            "txtCodFipe" = "",
                            "ddlMarca" = make_id, 
                            "ddlModelo" = mod_id,
                            "ddlAnoValor" = my_id,
                            "__ASYNCPOST" = "true",
                            style = 'post', curl = ch, .encoding = "utf-8")
    tt <- as.character(data_return)
    tt1 <- gsub('.*\\|[0-9]+\\|updatePanel\\|updValor\\|\\r\\n(.*)\\|0\\|hiddenField\\|\\_\\_EVENTTARGET\\|.*', "\\1", tt)
    tt2 <- htmlParse(tt1, encoding = "UTF-8")
    tp <- xpathSApply(tt2, "//span", xmlValue)
    tp_i <- rbind(tp_i, tp)
    
    VIEWSTATE <- gsub('.*__VIEWSTATE\\|(.*)\\|\\d+\\|hiddenField\\|__EVENTVALIDATION\\|(.*)\\|\\d+\\|asyncPostBackControlIDs.*', "\\1", tt)
    EVENTVALIDATION <- gsub('.*__VIEWSTATE\\|(.*)\\|\\d+\\|hiddenField\\|__EVENTVALIDATION\\|(.*)\\|\\d+\\|asyncPostBackControlIDs.*', "\\2", tt)
    print(mylist[i, 1])
  }
  
  tp_j <- rbind(tp_j, tp_i)
}

tp_222 <- as.data.frame(tp_j)

library(xlsx)
mkname <- mmk[mmk[,2] %in% make_id, 1]
monname <- sub(" / ", "", monlist[monlist[,2] %in% mon_select, 1])
(flname <- paste(mkname, "_", monname, ".xlsx", sep=""))
write.xlsx(tp_222, flname, row.names=F) 

