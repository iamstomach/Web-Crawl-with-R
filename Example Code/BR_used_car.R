
###########
# Useage
###########
## BR_used_price(year, mon = 1:12, make="Ford")

###########
# 参数说明
###########
## year: 年份, 指定年份，无默认值;
## mon:  月份, 默认为1:12. mon=1:3, 表示1月到3月; 可单独写单个月份, mon=8, 表示只下载8月份数据;
## make: 品牌，默认为"Ford", 不区分大小写. 常用取值如下所示;
# aa                bb  
# [1,] "Citro毛n"        "13"
# [2,] "Fiat"            "21"
# [3,] "Ford"            "22"
# [4,] "GM - Chevrolet"  "23"
# [5,] "Honda"           "25"
# [6,] "Hyundai"         "26"
# [7,] "Kia Motors"      "31"
# [8,] "Mitsubishi"      "41"
# [9,] "Nissan"          "43"
# [10,] "Peugeot"         "44"
# [11,] "Renault"         "48"
# [12,] "Toyota"          "56"
# [13,] "VW - VolksWagen" "59"

###########
# 示例
###########
# 下载renault2013所有数据
# aa <- BR_used_price(year = 2013, make="renault")
# aa <- BR_used_price(year = 2013, mon = 8, make="renault")


###########
# 正式使用
##########

# 如有需要，安装相应程序包; 如存在，不用重复安装
pkgs <- installed.packages()[, 1]
if(!'RCurl' %in% pkgs){
  install.packages('RCurl')
} 
if (!'XML' %in% pkgs){
  install.packages('XML')
} 
if (!'xlsx' %in% pkgs){
  install.packages('xlsx')
} 

# 设定工作目录，调用函数
setwd("M:\\02.Working\\9.Download Data\\BR Old Car")
source("BR_used_car_function.R", chdir = T)

# 下载数据
BR_used_price(year = 2013, mon = 1, make="renault")

