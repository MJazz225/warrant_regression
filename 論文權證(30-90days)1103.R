###
###   K/S 的moneyness 、
##    依stock排出每一天的warrant,算出每一档warrant moneyness and volume權重, 
##    加权平均得到avg moneyness（每一天只有一个）
##    open new data只有每一天的avg moneyness, Rt, Rt+1 return
##

rm(list=ls())
setwd("C:\\Users\\User\\Documents\\研究所\\論文\\2.清洗資料")
data_all <- read.csv("data_warrant_0828.csv", sep= ",")
data_all <- data_all[order(data_all$COID, decreasing = F), ]
data_sub <- subset(data_all, VOLUME>50) ##交易量超過50
data_sub <- subset(data_sub, RTIME<90) ##到期日少於90
data_sub <- subset(data_sub, RTIME>30) ##到期日超過30

setwd("C:\\Users\\User\\Documents\\研究所\\論文\\3.分割資料")
write.csv(data_sub, "datawarrant1(30-90).csv", sep = ",", row.names = F, col.names = T)
##去excel改資料 把putdata移去右邊


rawdata <- read.csv("datawarrant1(30-90).csv", sep = ",", stringsAsFactors = FALSE)
calldata <- rawdata[,c(1:13)]
calldata <- na.omit(calldata)
putdata <- rawdata[,c(15:27)]
putdata <- na.omit(putdata)
setwd("C:\\Users\\User\\Documents\\研究所\\論文\\4.call and put data")
write.csv(calldata, "callwarrant(30-90days).csv", sep = ",", row.names = F, col.names = T)
write.csv(putdata, "putwarrant(30-90days).csv", sep = ",", row.names = F, col.names = T)
########################################################################  清洗資料  ##############################################################

rm(list=ls())
if(!require(data.table))install.packages("data.table")
if(!require(dplyr))install.packages("dpylr")
library(data.table)
library(xts)
library(stargazer)
library(dplyr)

setwd("C:\\Users\\User\\Documents\\研究所\\論文\\4.call and put data")
calldata <- read.csv("callwarrant(30-90).csv", sep = ",", stringsAsFactors = F)
call_code <- calldata[,13] ##股票代碼
call_code <- data.frame(call_code)
call_code <- unique(call_code) ##存股票代碼的list
colnames(call_code) <- "COID" 

calldata <- calldata[order(calldata[,13], decreasing = F), ]
calldata[calldata == "      "] <- NA
calldata <- na.omit(calldata)
calldata <- calldata[with(calldata, order(STOCK_CODE, COID, as.Date(MDATE))),]

##sort calldata by stock code
call_list <- split(calldata, f = calldata$STOCK_CODE) ## sort by stock code
call_list1 <- list()
for (i in 1:length(call_list)) { ###去掉少過30row的list
  
  cat(i, "/", length(call_list), "\n")
  
  if (nrow(call_list[[i]])>30) {
    
    call_list1[[i]] <- call_list[[i]]
    
  }
  
}
data1 <- rbindlist(call_list1)


calldata1 <- data1
rm(data1)
call_list1 <- split(calldata1, f = calldata1$STOCK_CODE) ## 去掉NULL

calldata1 <- rbindlist(call_list1)
calldata1$MDATE <- as.Date(calldata1$MDATE)
getwd()
write.csv(calldata1, file = "callwarrant(30-90).csv", sep = ",", col.names = TRUE)

#####################################################   從這邊開始跑程式   #####################################################################################
rm(list=ls())
if(!require(data.table))install.packages("data.table")
if(!require(dplyr))install.packages("dpylr")
library(data.table)
library(xts)
library(stargazer)
library(dplyr)
setwd("C:\\Users\\User\\Documents\\研究所\\論文\\4.call and put data")
calldata1 <- read.csv("callwarrant(30-90).csv", sep = ",", stringsAsFactors = F)
calldata1$MDATE <- as.Date(calldata1$MDATE)
calldata1 <- calldata1[,2:ncol(calldata1)] #第一行是行名

###排成最旧到最新的calldata
calldata_date <- calldata1[order(as.Date(calldata1$MDATE), decreasing = F),] ##temporary data, going convert into list
call_date <- as.data.frame(unique(calldata_date[,2]))
colnames(call_date) <- "DATE"

##排成最早到最新的calldata list
call_list_date <- split(calldata_date, f = calldata_date$MDATE) ##依照時間排序的list
call_code <- unique(calldata1$STOCK_CODE)
date_list <- split(call_date, f = call_date$DATE)
date_code <- rbindlist(date_list)
date_code <- t(date_code)

call_list_date1 <- list()
temlist <- list()
for (i in 1:length(call_list_date)) {
  
  cat(i, "/", length(call_list_date), "\n")
  
  tem <- call_list_date[[i]]
  
  temlist[[i]] <- split(tem, f = tem$STOCK_CODE)
  
  names(temlist[i]) <- call_date[i,1] ##取名
  
}

call_list_date1 <- temlist  ##call_list_date1是每一天分開每一檔標的

names(call_list_date1) <- date_code[,c(1:length(date_code))]
rm(temlist)
######################################################
#
#
#      2）算出每一天每一檔moneyness  
#
#
#########################################################


## 每檔標的在每一天的的moneyness
moneyness <- NULL
for (i in 1:length(call_list_date1)) {
  
  cat(i, "/", length(call_list_date1), "\n")
  
  for (j in 1:length(call_list_date1[[i]])) {
    
    moneyness <- call_list_date1[[i]][[j]]$STRIKE_PRICE/call_list_date1[[i]][[j]]$STOCK_PRICE
    
    moneyness <- round(moneyness, 3)
    
    call_list_date1[[i]][[j]] <- cbind(call_list_date1[[i]][[j]], moneyness)
    
  }
  
}


################################################################
#
#
#     3）OI權重 X moneyness算出每一日每一檔權證的加權moneyness  
#
#
##################################################################

## 每檔標的在每一天的的average moneyness
avg.moneyness <- NULL
weighted.IV <- NULL
for (i in 1:length(call_list_date1)) {
  
  cat(i, "/", length(call_list_date1), "\n")
  
  for (j in 1:length(call_list_date1[[i]])) {
    
    avg.moneyness <- weighted.mean(call_list_date1[[i]][[j]]$moneyness, w = call_list_date1[[i]][[j]]$VOLUME)
    
    avg.moneyness <- round(avg.moneyness, 3)
    
    weighted.IV <- weighted.mean(call_list_date1[[i]][[j]]$IV, w = call_list_date1[[i]][[j]]$VOLUME)
    
    call_list_date1[[i]][[j]] <- cbind(call_list_date1[[i]][[j]], avg.moneyness, weighted.IV)
    
  }
  
}


getwd()


##################################################################
#
#
#     4）new table=當日加權moneyness, return, t+1 return ##### 
#
#
##################################################################

call_data2 <- NULL
for (i in 1:length(call_list_date1)) {##依照標的排成list
  
  cat(i, "/", length(call_list_date1), "\n")
  
  tem <- rbindlist(call_list_date1[[i]])
  
  call_data2 <- rbind(call_data2, tem)
  
}
call_data2 <- call_data2[,c(1,2,3,4,5,16,7,8,9,10,11,12,13,14,15)]
call_list2 <- split(call_data2, f = call_data2$COID)

##處理t+1 return
t1_return <- NULL  ##returnTEJ_call是 t+1現貨ROI,直接用TEJ的資料
t2_return <- NULL
##需要一點時間跑
for (i in 1:length(call_list2)) {
  
  cat(i, "/", length(call_list2), "\n")
  
  t1_return <- call_list2[[i]]$STOCK_PRICE ## 12是現貨ROI
  
  t1_return <- diff(log(t1_return))
  
  t1_return <- as.data.frame(t1_return)
  
  t1_return <- rbind(t1_return, NA)
  
  t1_return <- as.data.frame(t1_return)
  
  t2_return <- c(NA, t1_return[1:(nrow(t1_return)-1), ])
  
  t2_return <- as.data.frame(t2_return)
  
  call_list2[[i]] <-  cbind(call_list2[[i]], t1_return, t2_return) 
  
  tem_return <- call_list2[[i]][[12]]
  
  tem_return <- c(tem_return[2:length(tem_return)], NA) %>% as.data.frame()
  
  call_list2[[i]] <- cbind(call_list2[[i]], tem_return)
}

call_data3 <- rbindlist(call_list2, fill = TRUE)
call_data3 <- call_data3[,-19]
call_data3 <- na.omit(call_data3)

##################################################################
#
#
#     5）newtable2 = avgmoney, callmoneynes, roi現貨, roi_t+1現貨,
#
#
##################################################################
if(!require(plm))install.packages("plm")
library(plm)
library(data.table)
setwd("C:\\Users\\User\\Documents\\研究所\\論文")
data_all1 <- read.csv("data_all1215.csv", sep = ",", stringsAsFactors = F) #控制變數的data

call_code <- unique(call_data3$STOCK_CODE)
call_data4 <- call_data3[,c(1,13,2,6,12,15,16,17,18)]
colnames(call_data4) <- c("COID", "STOCK_CODE", "MDATE", "IV","STOCK_ROI", "avg.moneyness", "t1_return", "prior_day_return", "data_t1_return")

data_all2 <- na.omit(data_all1)
data_all2 <- data_all2[,c(1,2,10,11,12,13)]
data_coid <- data_all2$COID
data_coid <- as.data.frame(data_coid)
data_coid <- unique(data_coid)
call_list <- split(call_data4, f = call_data4$STOCK_CODE)
control_list <- split(data_all2, f = data_all2$MDATE)
control_list <- control_list[c(23:length(control_list))]
data_all3 <- rbindlist(control_list)
control_list <- split(data_all3, f = data_all3$COID)  

call_code1 <- c(call_code, "0050", "0051", "0056", "0061", "0643", "0636", "0639", "0668")
call_code1 <- as.data.frame(call_code1)
colnames(call_code1) <- "COID"
colnames(data_coid) <- "COID"
control_list1 <- list(control_list$`00631L `,
                      control_list$`00632R `,control_list$`00633L `,control_list$`00634R `,control_list$`00635U `,control_list$`00637L `,control_list$`00638R `,
                      control_list$`00642U `,control_list$`00648R `,control_list$`00655L `,control_list$`00656R `,control_list$`00658L `,control_list$`00659R `,
                      control_list$`00664R `,control_list$`00669R `,control_list$`00672L `,control_list$`00673R `,control_list$`00676R `,control_list$`00677U `,
                      control_list$`00683L `,control_list$`00693U `,control_list$`00708L `,
                      control_list$`1101   `,control_list$`1102   `,control_list$`1201   `,control_list$`1210   `,control_list$`1215   `
                      ,control_list$`1216   `,control_list$`1218   `,control_list$`1262   `,control_list$`1301   `,control_list$`1303   `,control_list$`1304   `,control_list$`1305   `
                      ,control_list$`1310   `,control_list$`1312   `,control_list$`1313   `,control_list$`1314   `,control_list$`1319   `,control_list$`1326   `
                      ,control_list$`1338   `,control_list$`1402   `,control_list$`1409   `,control_list$`1444   `,control_list$`1455   `,control_list$`1464   `
                      ,control_list$`1476   `,control_list$`1477   `,control_list$`1504   `,control_list$`1513   `,control_list$`1515   `,control_list$`1522   `
                      ,control_list$`1525   `,control_list$`1527   `,control_list$`1532   `,control_list$`1536   `,control_list$`1537   `,control_list$`1560   `
                      ,control_list$`1565   `,control_list$`1569   `,control_list$`1580   `,control_list$`1582   `,control_list$`1586   `,control_list$`1589   `
                      ,control_list$`1590   `,control_list$`1597   `,control_list$`1605   `,control_list$`1608   `,control_list$`1609   `,control_list$`1704   `
                      ,control_list$`1707   `,control_list$`1710   `,control_list$`1714   `,control_list$`1718   `,control_list$`1722   `,control_list$`1723   `
                      ,control_list$`1752   `,control_list$`1760   `,control_list$`1795   `,control_list$`1802   `,control_list$`1808   `,control_list$`1815   `
                      ,control_list$`1904   `,control_list$`1905   `,control_list$`1907   `,control_list$`1909   `,control_list$`2002   `,control_list$`2014   `
                      ,control_list$`2023   `,control_list$`2027   `,control_list$`2029   `,control_list$`2031   `,control_list$`2034   `,control_list$`2049   `
                      ,control_list$`2103   `,control_list$`2104   `,control_list$`2105   `,control_list$`2208   `,control_list$`2228   `,control_list$`2231   `
                      ,control_list$`2233   `,control_list$`2239   `,control_list$`2301   `,control_list$`2303   `,control_list$`2308   `,control_list$`2311   `
                      ,control_list$`2312   `,control_list$`2313   `,control_list$`2314   `,control_list$`2316   `,control_list$`2317   `,control_list$`2323   `
                      ,control_list$`2324   `,control_list$`2327   `,control_list$`2328   `,control_list$`2330   `,control_list$`2331   `,control_list$`2332   `
                      ,control_list$`2337   `,control_list$`2338   `,control_list$`2340   `,control_list$`2344   `,control_list$`2345   `,control_list$`2347   `
                      ,control_list$`2351   `,control_list$`2352   `,control_list$`2353   `,control_list$`2354   `,control_list$`2355   `,control_list$`2356   `
                      ,control_list$`2360   `,control_list$`2363   `,control_list$`2367   `,control_list$`2368   `,control_list$`2371   `,control_list$`2375   `
                      ,control_list$`2376   `,control_list$`2377   `,control_list$`2379   `,control_list$`2382   `,control_list$`2383   `,control_list$`2392   `
                      ,control_list$`2393   `,control_list$`2401   `,control_list$`2404   `,control_list$`2406   `,control_list$`2408   `,control_list$`2409   `
                      ,control_list$`2412   `,control_list$`2421   `,control_list$`2426   `,control_list$`2428   `,control_list$`2439   `,control_list$`2448   `
                      ,control_list$`2449   `,control_list$`2451   `,control_list$`2454   `,control_list$`2455   `,control_list$`2456   `,control_list$`2458   `
                      ,control_list$`2464   `,control_list$`2472   `,control_list$`2474   `,control_list$`2478   `,control_list$`2481   `,control_list$`2485   `
                      ,control_list$`2489   `,control_list$`2492   `,control_list$`2498   `,control_list$`2515   `,control_list$`2520   `,control_list$`2534   `
                      ,control_list$`2542   `,control_list$`2545   `,control_list$`2548   `,control_list$`2601   `,control_list$`2603   `,control_list$`2605   `
                      ,control_list$`2606   `,control_list$`2609   `,control_list$`2610   `,control_list$`2614   `,control_list$`2615   `,control_list$`2617   `
                      ,control_list$`2618   `,control_list$`2633   `,control_list$`2723   `,control_list$`2726   `,control_list$`2727   `,control_list$`2732   `
                      ,control_list$`2801   `,control_list$`2823   `,control_list$`2834   `,control_list$`2880   `,control_list$`2881   `,control_list$`2882   `
                      ,control_list$`2883   `,control_list$`2884   `,control_list$`2885   `,control_list$`2886   `,control_list$`2887   `,control_list$`2888   `
                      ,control_list$`2890   `,control_list$`2891   `,control_list$`2892   `,control_list$`2913   `,control_list$`2915   `,control_list$`2928   `
                      ,control_list$`2929   `,control_list$`3003   `,control_list$`3005   `,control_list$`3006   `,control_list$`3008   `,control_list$`3016   `
                      ,control_list$`3017   `,control_list$`3019   `,control_list$`3023   `,control_list$`3026   `,control_list$`3030   `,control_list$`3034   `
                      ,control_list$`3035   `,control_list$`3036   `,control_list$`3037   `,control_list$`3042   `,control_list$`3044   `,control_list$`3045   `
                      ,control_list$`3049   `,control_list$`3059   `,control_list$`3068   `,control_list$`3078   `,control_list$`3081   `,control_list$`3083   `
                      ,control_list$`3088   `,control_list$`3090   `,control_list$`3092   `,control_list$`3105   `,control_list$`3131   `,control_list$`3141   `
                      ,control_list$`3152   `,control_list$`3163   `,control_list$`3189   `,control_list$`3205   `,control_list$`3211   `,control_list$`3217   `
                      ,control_list$`3221   `,control_list$`3224   `,control_list$`3227   `,control_list$`3231   `,control_list$`3234   `,control_list$`3236   `
                      ,control_list$`3260   `,control_list$`3264   `,control_list$`3265   `,control_list$`3289   `,control_list$`3293   `,control_list$`3299   `
                      ,control_list$`3317   `,control_list$`3323   `,control_list$`3324   `,control_list$`3346   `,control_list$`3362   `,control_list$`3363   `
                      ,control_list$`3374   `,control_list$`3376   `,control_list$`3380   `,control_list$`3406   `,control_list$`3413   `,control_list$`3428   `
                      ,control_list$`3443   `,control_list$`3450   `,control_list$`3454   `,control_list$`3455   `,control_list$`3479   `,control_list$`3481   `
                      ,control_list$`3484   `,control_list$`3490   `,control_list$`3491   `,control_list$`3498   `,control_list$`3504   `,control_list$`3508   `
                      ,control_list$`3514   `,control_list$`3515   `,control_list$`3522   `,control_list$`3526   `,control_list$`3527   `,control_list$`3529   `
                      ,control_list$`3532   `,control_list$`3533   `,control_list$`3541   `,control_list$`3545   `,control_list$`3546   `,control_list$`3548   `
                      ,control_list$`3552   `,control_list$`3556   `,control_list$`3558   `,control_list$`3563   `,control_list$`3576   `,control_list$`3587   `
                      ,control_list$`3596   `,control_list$`3611   `,control_list$`3617   `,control_list$`3624   `,control_list$`3645   `,control_list$`3653   `
                      ,control_list$`3661   `,control_list$`3665   `,control_list$`3673   `,control_list$`3689   `,control_list$`3691   `,control_list$`3698   `
                      ,control_list$`3702   `,control_list$`3706   `,control_list$`3707   `,control_list$`3708   `,control_list$`3711   `,control_list$`4105   `
                      ,control_list$`4107   `,control_list$`4114   `,control_list$`4119   `,control_list$`4120   `,control_list$`4123   `,control_list$`4126   `
                      ,control_list$`4128   `,control_list$`4129   `,control_list$`4130   `,control_list$`4137   `,control_list$`4147   `,control_list$`4148   `
                      ,control_list$`4157   `,control_list$`4162   `,control_list$`4163   `,control_list$`4167   `,control_list$`4190   `,control_list$`4426   `
                      ,control_list$`4528   `,control_list$`4532   `,control_list$`4536   `,control_list$`4551   `,control_list$`4552   `,control_list$`4721   `
                      ,control_list$`4725   `,control_list$`4728   `,control_list$`4736   `,control_list$`4739   `,control_list$`4743   `,control_list$`4746   `
                      ,control_list$`4763   `,control_list$`4803   `,control_list$`4804   `,control_list$`4903   `,control_list$`4904   `,control_list$`4906   `
                      ,control_list$`4912   `,control_list$`4915   `,control_list$`4919   `,control_list$`4927   `,control_list$`4935   `,control_list$`4938   `
                      ,control_list$`4943   `,control_list$`4947   `,control_list$`4953   `,control_list$`4958   `,control_list$`4960   `,control_list$`4961   `
                      ,control_list$`4966   `,control_list$`4971   `,control_list$`4974   `,control_list$`4977   `,control_list$`4979   `,control_list$`4991   `,control_list$`0050   `
                      ,control_list$`5009   `,control_list$`5014   `,control_list$`5234   `,control_list$`5243   `,control_list$`5245   `,control_list$`5263   `
                      ,control_list$`5264   `,control_list$`5269   `,control_list$`5274   `,control_list$`5287   `,control_list$`5288   `,control_list$`5289   `
                      ,control_list$`5299   `,control_list$`5305   `,control_list$`5306   `,control_list$`5315   `,control_list$`5317   `,control_list$`5347   `
                      ,control_list$`5349   `,control_list$`5356   `,control_list$`5371   `,control_list$`5388   `,control_list$`5392   `,control_list$`5425   `
                      ,control_list$`5434   `,control_list$`5439   `,control_list$`5443   `,control_list$`5457   `,control_list$`5469   `,control_list$`5478   `
                      ,control_list$`5483   `,control_list$`5490   `,control_list$`5512   `,control_list$`5530   `,control_list$`5534   `,control_list$`5536   `,control_list$`0056   `
                      ,control_list$`5871   `,control_list$`5876   `,control_list$`0061   `,control_list$`6104   `,control_list$`6105   `,control_list$`6107   `,control_list$`6111   `
                      ,control_list$`6116   `,control_list$`6120   `,control_list$`6121   `,control_list$`6138   `,control_list$`6139   `,control_list$`6143   `
                      ,control_list$`6146   `,control_list$`6147   `,control_list$`6150   `,control_list$`6153   `,control_list$`6158   `,control_list$`6173   `
                      ,control_list$`6175   `,control_list$`6176   `,control_list$`6180   `,control_list$`6182   `,control_list$`6187   `,control_list$`6188   `
                      ,control_list$`6196   `,control_list$`6202   `,control_list$`6205   `,control_list$`6206   `,control_list$`6207   `,control_list$`6209   `
                      ,control_list$`6213   `,control_list$`6214   `,control_list$`6220   `,control_list$`6223   `,control_list$`6230   `,control_list$`6238   `
                      ,control_list$`6239   `,control_list$`6244   `,control_list$`6245   `,control_list$`6257   `,control_list$`6261   `,control_list$`6269   `
                      ,control_list$`6271   `,control_list$`6274   `,control_list$`6278   `,control_list$`6279   `,control_list$`6282   `,control_list$`6283   `
                      ,control_list$`6284   `,control_list$`6285   `,control_list$`6290   `,control_list$`00636  `,control_list$`00639  `,control_list$`6411   `,control_list$`6412   `,control_list$`6414   `
                      ,control_list$`6415   `,control_list$`6426   `,control_list$`00643  `,control_list$`6435   `,control_list$`6438   `,control_list$`6451   `,control_list$`6452   `
                      ,control_list$`6456   `,control_list$`6462   `,control_list$`6472   `,control_list$`6488   `,control_list$`6496   `,control_list$`6509   `,control_list$`6510   `
                      ,control_list$`6523   `,control_list$`6525   `,control_list$`6548   `,control_list$`6552   `,control_list$`6554   `,control_list$`6561   `
                      ,control_list$`6568   `,control_list$`6569   `,control_list$`6613   `,control_list$`6643   `,control_list$`6666   `,control_list$`6670   `,control_list$`00668  `
                      ,control_list$`8016   `,control_list$`8027   `,control_list$`8038   `,control_list$`8039   `,control_list$`8042   `,control_list$`8043   `
                      ,control_list$`8044   `,control_list$`8046   `,control_list$`8050   `,control_list$`8064   `,control_list$`8066   `,control_list$`8069   `
                      ,control_list$`8070   `,control_list$`8076   `,control_list$`8091   `,control_list$`8105   `,control_list$`8110   `,control_list$`8112   `
                      ,control_list$`8114   `,control_list$`8150   `,control_list$`8155   `,control_list$`8163   `,control_list$`8210   `,control_list$`8213   `
                      ,control_list$`8215   `,control_list$`8234   `,control_list$`8255   `,control_list$`8299   `,control_list$`8341   `,control_list$`8349   `
                      ,control_list$`8358   `,control_list$`8383   `,control_list$`8404   `,control_list$`8406   `,control_list$`8415   `,control_list$`8416   `
                      ,control_list$`8433   `,control_list$`8436   `,control_list$`8437   `,control_list$`8446   `,control_list$`8450   `,control_list$`8462   `
                      ,control_list$`8464   `,control_list$`8489   `,control_list$`8916   `,control_list$`8924   `,control_list$`8926   `,control_list$`8936   `
                      ,control_list$`8938   `,control_list$`8942   `,control_list$`9136   `,control_list$`9802   `,control_list$`9904   `,control_list$`9911   `
                      ,control_list$`9914   `,control_list$`9921   `,control_list$`9934   `,control_list$`9938   `,control_list$`9939   `,control_list$`9941   `
                      ,control_list$`9951   `,control_list$`9958   `,control_list$`M2300  `,control_list$`M2800  `,control_list$`Y9999  `)

names(control_list1) <- y[c(1:nrow(y)),]


names(call_list) <- y[c(1:nrow(y)),]

newdata <- NULL
for (i in 1:length(call_list)) {#把控制變數的資料丟進data
  
  cat(i, "/", length(call_list), "\n")
  
  temcontrol <- control_list1[[i]]
  
  temcontrol$MDATE <- as.Date(temcontrol$MDATE)
  
  temdata <- call_list[[i]]
  
  temlist <- split(temdata, f= temdata$COID)
  
  for (j in 1:length(temlist)) {
    
    jlist <- temlist[[j]]
    
    jlist$MDATE <- as.Date(jlist$MDATE)
    
    data_x <- merge(jlist, temcontrol, by = "MDATE", all = F)
    
    newdata <- rbind(newdata, data_x)
  }
}

newdata1 <- newdata[,-10] ##去掉多一列的coid
colnames(newdata1[,2]) <- "COID"

write.csv(newdata1, file = "call_model_data.csv", sep = ",", col.names = TRUE)
#newdata1 <- read.csv("call_model_data.csv", sep = ",", stringsAsFactors = F) #控制變數的data


call_pooling_model <- plm(newdata1$t1_return~newdata1$avg.moneyness+newdata1$STOCK_ROI+newdata1$prior_day_return+newdata1$IV+newdata1$MONTHRET+newdata1$MONTHTURNOVER+newdata1$ILLIQUIDITY+newdata1$ME, data = newdata1,
                          index = c("MDATE", "COID.x"), model = "pooling")
call_fix_model <- plm(newdata1$t1_return~newdata1$avg.moneyness+newdata1$STOCK_ROI+newdata1$prior_day_return+newdata1$IV+newdata1$MONTHRET+newdata1$MONTHTURNOVER+newdata1$ILLIQUIDITY+newdata1$ME, data = newdata1,
                      index = c("MDATE", "COID.x"), model = "within", effect = "individual")
call_fix_model1 <- plm(newdata1$t1_return~newdata1$avg.moneyness+newdata1$STOCK_ROI+newdata1$prior_day_return+newdata1$IV+newdata1$MONTHRET+newdata1$MONTHTURNOVER+newdata1$ILLIQUIDITY+newdata1$ME, data = newdata1,
                       index = c("MDATE", "COID.x"), model = "within", effects = "twoways")
call_random_model <- plm(newdata1$t1_return~newdata1$avg.moneyness+newdata1$STOCK_ROI+newdata1$prior_day_return+newdata1$IV+newdata1$MONTHRET+newdata1$MONTHTURNOVER+newdata1$ILLIQUIDITY+newdata1$ME, data = newdata1,
                         index = c("MDATE", "COID.x"), model = "random")

summary(call_pooling_model)
summary(call_fix_model)
summary(call_fix_model1)
summary(call_random_model)

if(!require(stargazer))install.packages("stargazer")
sink("call_model(30-90).txt")
bind_model <- stargazer(call_pooling_model, call_fix_model, call_fix_model1, call_random_model, title="Results", digits = 5,  header = TRUE, type = "text", report = "vc*p",
                        column.labels = c("pooling", "FE-individuals", "FE", "RE"), model.names = TRUE)
sink()


call_pooling_model_data <- plm(newdata1$data_t1_return~newdata1$avg.moneyness+newdata1$STOCK_ROI+newdata1$prior_day_return+newdata1$IV+newdata1$MONTHRET+newdata1$MONTHTURNOVER+newdata1$ILLIQUIDITY+newdata1$ME, data = newdata1,
                               index = c("MDATE", "COID.x"), model = "pooling")
call_fix_model_data <- plm(newdata1$data_t1_return~newdata1$avg.moneyness+newdata1$STOCK_ROI+newdata1$prior_day_return+newdata1$IV+newdata1$MONTHRET+newdata1$MONTHTURNOVER+newdata1$ILLIQUIDITY+newdata1$ME, data = newdata1,
                           index = c("MDATE", "COID.x"), model = "within", effect = "individual")
call_fix_model1_data <- plm(newdata1$data_t1_return~newdata1$avg.moneyness+newdata1$STOCK_ROI+newdata1$prior_day_return+newdata1$IV+newdata1$MONTHRET+newdata1$MONTHTURNOVER+newdata1$ILLIQUIDITY+newdata1$ME, data = newdata1,
                            index = c("MDATE", "COID.x"), model = "within", effects = "twoways")
call_random_model_data <- plm(newdata1$data_t1_return~newdata1$avg.moneyness+newdata1$STOCK_ROI+newdata1$prior_day_return+newdata1$IV+newdata1$MONTHRET+newdata1$MONTHTURNOVER+newdata1$ILLIQUIDITY+newdata1$ME, data = newdata1,
                              index = c("MDATE", "COID.x"), model = "random")

sink("call_model(30-90)data return.txt")
bind_model <- stargazer(call_pooling_model_data, call_fix_model_data, call_fix_model1_data, call_random_model_data, title="Results", digits = 5,  header = TRUE, type = "text", report = "vc*p",
                        column.labels = c("pooling", "FE-individuals", "FE", "RE"), model.names = TRUE)
sink()

pFtest(call_fix_model1, call_pooling_model)
pFtest(call_fix_model1_data, call_pooling_model_data)
'混合回歸模型和固定效果模型的檢定，
p<0.05，顯著，因此拒絕虛無假説，
使用固定效果模型較爲適當
p-value is < 0.05 then the fixed effects model is a better choice'

phtest(call_random_model, call_fix_model1)
phtest(call_random_model_data, call_fix_model1_data)
'隨即效果模型和固定效果模型的檢定，
p<0.05，不顯著，因此不拒絕虛無假説，
使用固定效果模型較爲適當'

##################################################################################################################################################
###############################################################     put DATA    ##################################################################
##################################################################################################################################################
rm(list=ls())
if(!require(data.table))install.packages("data.table")
if(!require(dplyr))install.packages("dpylr")
library(data.table)
library(xts)
library(stargazer)
library(dplyr)
setwd("C:\\Users\\User\\Documents\\研究所\\論文\\4.call and put data")
putdata <- read.csv("putwarrant(30-90).csv", sep = ",", stringsAsFactors = F)
put_code <- putdata[,13] ##股票代碼
put_code <- data.frame(put_code)
put_code <- unique(put_code) ##存股票代碼的list
colnames(put_code) <- "COID" 

putdata <- putdata[order(putdata[,13], decreasing = F), ]
putdata[putdata == "      "] <- NA
putdata <- na.omit(putdata)
abc <- c("COID", "MDATE", "CLOSE", "VOLUME", "OI", "IV", "RTIME", "LEVERAGE", "STRIKE_PRICE", "STOCK_PRICE", "ROI", "STOCK_ROI", "STOCK_CODE")
colnames(putdata) <- abc
putdata <- putdata[with(putdata, order(STOCK_CODE, COID, as.Date(MDATE))),]

##sort putdata by stock code
put_list <- split(putdata, f = putdata$STOCK_CODE) ## sort by stock code
put_list1 <- list()
for (i in 1:length(put_list)) { ###去掉少過5000row的list
  
  cat(i, "/", length(put_list), "\n")
  
  if (nrow(put_list[[i]])>30) {
    
    put_list1[[i]] <- put_list[[i]]
    
  }
  
}
data1 <- rbindlist(put_list1)
put_list1 <- split(data1, f = data1$STOCK_CODE) ## 去掉NULL

putdata1 <- rbindlist(put_list1)
putdata1$MDATE <- as.Date(putdata1$MDATE)
###排成最旧到最新的putdata
putdata_date <- putdata1[order(as.Date(putdata1$MDATE), decreasing = F),] ##temporary data, going convert into list
put_date <- as.data.frame(unique(putdata_date[,2]))
colnames(put_date) <- "DATE"

##排成最早到最新的putdata list
put_list_date <- split(putdata_date, f = putdata_date$MDATE) ##依照時間排序的list
date_list <- split(put_date, f = put_date$DATE)
date_code <- rbindlist(date_list)
date_code <- t(date_code)
put_list_date1 <- list()
temlist <- list()
for (i in 1:length(put_list_date)) {
  
  cat(i, "/", length(put_list_date), "\n")
  
  tem <- put_list_date[[i]]
  
  temlist[[i]] <- split(tem, f = tem$STOCK_CODE)
  
  names(temlist[i]) <- put_date[i,1] ##取名
  
}

put_list_date1 <- temlist

names(put_list_date1) <- date_code[,c(1:length(date_code))]

######################################################
#
#
#      2）算出每一天每一檔moneyness  
#
#
#########################################################


## 每檔標的在每一天的的moneyness
moneyness <- NULL
for (i in 1:length(put_list_date1)) {
  
  cat(i, "/", length(put_list_date1), "\n")
  
  for (j in 1:length(put_list_date1[[i]])) {
    
    moneyness <- put_list_date1[[i]][[j]]$STRIKE_PRICE/put_list_date1[[i]][[j]]$STOCK_PRICE
    
    moneyness <- round(moneyness, 3)
    
    put_list_date1[[i]][[j]] <- cbind(put_list_date1[[i]][[j]], moneyness)
    
  }
  
}


################################################################
#
#
#     3）OI權重 X moneyness算出每一日每一檔權證的加權moneyness  
#
#
##################################################################

## 每檔標的在每一天的的average moneyness

avg.moneyness <- NULL
weighted.IV <- NULL
for (i in 1:length(put_list_date1)) {
  
  cat(i, "/", length(put_list_date1), "\n")
  
  for (j in 1:length(put_list_date1[[i]])) {
    
    avg.moneyness <- weighted.mean(put_list_date1[[i]][[j]]$moneyness, w = put_list_date1[[i]][[j]]$VOLUME)
    
    avg.moneyness <- round(avg.moneyness, 3)
    
    weighted.IV <- weighted.mean(put_list_date1[[i]][[j]]$IV, w = put_list_date1[[i]][[j]]$VOLUME)
    
    put_list_date1[[i]][[j]] <- cbind(put_list_date1[[i]][[j]], avg.moneyness, weighted.IV)
    
  }
  
}



##################################################################
#
#
#     4）new table=當日加權moneyness, return, t+1 return ##### 
#
#
##################################################################

put_data2 <- NULL
for (i in 1:length(put_list_date1)) {##依照標的排成list
  
  cat(i, "/", length(put_list_date1), "\n")
  
  tem <- rbindlist(put_list_date1[[i]])
  
  put_data2 <- rbind(put_data2, tem)
  
}
put_data2 <- put_data2[,c(1,2,3,4,5,16,7,8,9,10,11,12,13,14,15)]
put_list2 <- split(put_data2, f = put_data2$COID)


##處理t+1 return
t1_return <- NULL  ##returnTEJ_put是 t+1現貨ROI,直接用TEJ的資料
t2_return <- NULL
##需要一點時間跑
for (i in 1:length(put_list2)) {
  
  cat(i, "/", length(put_list2), "\n")
  
  t1_return <- put_list2[[i]]$STOCK_PRICE ## 12是現貨ROI
  
  t1_return <- diff(log(t1_return))
  
  t1_return <- as.data.frame(t1_return)
  
  t1_return <- rbind(t1_return, NA)
  
  t1_return <- as.data.frame(t1_return)
  
  t2_return <- c(NA, t1_return[1:(nrow(t1_return)-1), ])
  
  t2_return <- as.data.frame(t2_return)
  
  put_list2[[i]] <-  cbind(put_list2[[i]], t1_return, t2_return) 
  
  tem_return <- put_list2[[i]][[12]]
  
  tem_return <- c(tem_return[2:length(tem_return)], NA) %>% as.data.frame()
  
  put_list2[[i]] <- cbind(put_list2[[i]], tem_return)
}

put_data3 <- rbindlist(put_list2, fill = TRUE)
put_data3 <- put_data3[,-19]
put_data3 <- na.omit(put_data3)

##################################################################
#
#
#     5）newtable2 = avgmoney, putmoneyneSs, roi現貨, roi_t+1現貨,
#
#
##################################################################
if(!require(plm))install.packages("plm")
library(plm)
library(data.table)
setwd("C:\\Users\\User\\Documents\\研究所\\論文")
data_all1 <- read.csv("data_all1215.csv", sep = ",", stringsAsFactors = F) #控制變數的data

put_code <- unique(put_data3$STOCK_CODE)
put_data4 <- put_data3[,c(1,13,2,6,12,15,16,17,18)]
colnames(put_data4) <- c("COID", "STOCK_CODE", "MDATE", "IV", "STOCK_ROI", "avg.moneyness", "t1_return", "prior_day_return", "data_t1_return")

data_all2 <- na.omit(data_all1)
data_all2 <- data_all2[,c(1,2,10,11,12,13)]
data_coid <- data_all2$COID
data_coid <- as.data.frame(data_coid)
data_coid <- unique(data_coid)
put_list <- split(put_data4, f = put_data4$STOCK_CODE)
control_list <- split(data_all2, f = data_all2$MDATE)
control_list <- control_list[c(23:length(control_list))]
data_all3 <- rbindlist(control_list)
control_list <- split(data_all3, f = data_all3$COID)  

put_code1 <- c(put_code, "0050", "0051", "0056", "0061", "0643", "0636", "0639", "0668")
put_code1 <- as.data.frame(put_code1)
colnames(put_code1) <- "COID"
colnames(data_coid) <- "COID"
control_list1 <- list(control_list$`00631L `,
                      control_list$`00632R `,control_list$`00633L `,control_list$`00634R `,control_list$`00635U `,control_list$`00637L `,control_list$`00638R `,
                      control_list$`00642U `,control_list$`00648R `,control_list$`00655L `,control_list$`00656R `,control_list$`00658L `,control_list$`00659R `,
                      control_list$`00664R `,control_list$`00669R `,control_list$`00672L `,control_list$`00673R `,control_list$`00676R `,control_list$`00677U `,
                      control_list$`00683L `,control_list$`00693U `,control_list$`00708L `,
                      control_list$`1101   `,control_list$`1102   `,control_list$`1201   `,control_list$`1210   `,control_list$`1215   `
                      ,control_list$`1216   `,control_list$`1218   `,control_list$`1262   `,control_list$`1301   `,control_list$`1303   `,control_list$`1304   `,control_list$`1305   `
                      ,control_list$`1310   `,control_list$`1312   `,control_list$`1313   `,control_list$`1314   `,control_list$`1319   `,control_list$`1326   `
                      ,control_list$`1338   `,control_list$`1402   `,control_list$`1409   `,control_list$`1444   `,control_list$`1455   `,control_list$`1464   `
                      ,control_list$`1476   `,control_list$`1477   `,control_list$`1504   `,control_list$`1513   `,control_list$`1515   `,control_list$`1522   `
                      ,control_list$`1525   `,control_list$`1527   `,control_list$`1532   `,control_list$`1536   `,control_list$`1537   `,control_list$`1560   `
                      ,control_list$`1565   `,control_list$`1569   `,control_list$`1580   `,control_list$`1582   `,control_list$`1586   `,control_list$`1589   `
                      ,control_list$`1590   `,control_list$`1597   `,control_list$`1605   `,control_list$`1608   `,control_list$`1609   `,control_list$`1704   `
                      ,control_list$`1707   `,control_list$`1710   `,control_list$`1714   `,control_list$`1718   `,control_list$`1722   `,control_list$`1723   `
                      ,control_list$`1752   `,control_list$`1760   `,control_list$`1795   `,control_list$`1802   `,control_list$`1808   `,control_list$`1815   `
                      ,control_list$`1904   `,control_list$`1905   `,control_list$`1907   `,control_list$`1909   `,control_list$`2002   `,control_list$`2014   `
                      ,control_list$`2023   `,control_list$`2027   `,control_list$`2029   `,control_list$`2031   `,control_list$`2034   `,control_list$`2049   `
                      ,control_list$`2103   `,control_list$`2104   `,control_list$`2105   `,control_list$`2208   `,control_list$`2228   `,control_list$`2231   `
                      ,control_list$`2233   `,control_list$`2239   `,control_list$`2301   `,control_list$`2303   `,control_list$`2308   `,control_list$`2311   `
                      ,control_list$`2312   `,control_list$`2313   `,control_list$`2314   `,control_list$`2316   `,control_list$`2317   `,control_list$`2323   `
                      ,control_list$`2324   `,control_list$`2327   `,control_list$`2328   `,control_list$`2330   `,control_list$`2331   `,control_list$`2332   `
                      ,control_list$`2337   `,control_list$`2338   `,control_list$`2340   `,control_list$`2344   `,control_list$`2345   `,control_list$`2347   `
                      ,control_list$`2351   `,control_list$`2352   `,control_list$`2353   `,control_list$`2354   `,control_list$`2355   `,control_list$`2356   `
                      ,control_list$`2360   `,control_list$`2363   `,control_list$`2367   `,control_list$`2368   `,control_list$`2371   `,control_list$`2375   `
                      ,control_list$`2376   `,control_list$`2377   `,control_list$`2379   `,control_list$`2382   `,control_list$`2383   `,control_list$`2392   `
                      ,control_list$`2393   `,control_list$`2401   `,control_list$`2404   `,control_list$`2406   `,control_list$`2408   `,control_list$`2409   `
                      ,control_list$`2412   `,control_list$`2421   `,control_list$`2426   `,control_list$`2428   `,control_list$`2439   `,control_list$`2448   `
                      ,control_list$`2449   `,control_list$`2451   `,control_list$`2454   `,control_list$`2455   `,control_list$`2456   `,control_list$`2458   `
                      ,control_list$`2464   `,control_list$`2472   `,control_list$`2474   `,control_list$`2478   `,control_list$`2481   `,control_list$`2485   `
                      ,control_list$`2489   `,control_list$`2492   `,control_list$`2498   `,control_list$`2515   `,control_list$`2520   `,control_list$`2534   `
                      ,control_list$`2542   `,control_list$`2545   `,control_list$`2548   `,control_list$`2601   `,control_list$`2603   `,control_list$`2605   `
                      ,control_list$`2606   `,control_list$`2609   `,control_list$`2610   `,control_list$`2614   `,control_list$`2615   `,control_list$`2617   `
                      ,control_list$`2618   `,control_list$`2633   `,control_list$`2723   `,control_list$`2726   `,control_list$`2727   `,control_list$`2732   `
                      ,control_list$`2801   `,control_list$`2823   `,control_list$`2834   `,control_list$`2880   `,control_list$`2881   `,control_list$`2882   `
                      ,control_list$`2883   `,control_list$`2884   `,control_list$`2885   `,control_list$`2886   `,control_list$`2887   `,control_list$`2888   `
                      ,control_list$`2890   `,control_list$`2891   `,control_list$`2892   `,control_list$`2913   `,control_list$`2915   `,control_list$`2928   `
                      ,control_list$`2929   `,control_list$`3003   `,control_list$`3005   `,control_list$`3006   `,control_list$`3008   `,control_list$`3016   `
                      ,control_list$`3017   `,control_list$`3019   `,control_list$`3023   `,control_list$`3026   `,control_list$`3030   `,control_list$`3034   `
                      ,control_list$`3035   `,control_list$`3036   `,control_list$`3037   `,control_list$`3042   `,control_list$`3044   `,control_list$`3045   `
                      ,control_list$`3049   `,control_list$`3059   `,control_list$`3068   `,control_list$`3078   `,control_list$`3081   `,control_list$`3083   `
                      ,control_list$`3088   `,control_list$`3090   `,control_list$`3092   `,control_list$`3105   `,control_list$`3131   `,control_list$`3141   `
                      ,control_list$`3152   `,control_list$`3163   `,control_list$`3189   `,control_list$`3205   `,control_list$`3211   `,control_list$`3217   `
                      ,control_list$`3221   `,control_list$`3224   `,control_list$`3227   `,control_list$`3231   `,control_list$`3234   `,control_list$`3236   `
                      ,control_list$`3260   `,control_list$`3264   `,control_list$`3265   `,control_list$`3289   `,control_list$`3293   `,control_list$`3299   `
                      ,control_list$`3317   `,control_list$`3323   `,control_list$`3324   `,control_list$`3346   `,control_list$`3362   `,control_list$`3363   `
                      ,control_list$`3374   `,control_list$`3376   `,control_list$`3380   `,control_list$`3406   `,control_list$`3413   `,control_list$`3428   `
                      ,control_list$`3443   `,control_list$`3450   `,control_list$`3454   `,control_list$`3455   `,control_list$`3479   `,control_list$`3481   `
                      ,control_list$`3484   `,control_list$`3490   `,control_list$`3491   `,control_list$`3498   `,control_list$`3504   `,control_list$`3508   `
                      ,control_list$`3514   `,control_list$`3515   `,control_list$`3522   `,control_list$`3526   `,control_list$`3527   `,control_list$`3529   `
                      ,control_list$`3532   `,control_list$`3533   `,control_list$`3541   `,control_list$`3545   `,control_list$`3546   `,control_list$`3548   `
                      ,control_list$`3552   `,control_list$`3556   `,control_list$`3558   `,control_list$`3563   `,control_list$`3576   `,control_list$`3587   `
                      ,control_list$`3596   `,control_list$`3611   `,control_list$`3617   `,control_list$`3624   `,control_list$`3645   `,control_list$`3653   `
                      ,control_list$`3661   `,control_list$`3665   `,control_list$`3673   `,control_list$`3689   `,control_list$`3691   `,control_list$`3698   `
                      ,control_list$`3702   `,control_list$`3706   `,control_list$`3707   `,control_list$`3708   `,control_list$`3711   `,control_list$`4105   `
                      ,control_list$`4107   `,control_list$`4114   `,control_list$`4119   `,control_list$`4120   `,control_list$`4123   `,control_list$`4126   `
                      ,control_list$`4128   `,control_list$`4129   `,control_list$`4130   `,control_list$`4137   `,control_list$`4147   `,control_list$`4148   `
                      ,control_list$`4157   `,control_list$`4162   `,control_list$`4163   `,control_list$`4167   `,control_list$`4190   `,control_list$`4426   `
                      ,control_list$`4528   `,control_list$`4532   `,control_list$`4536   `,control_list$`4551   `,control_list$`4552   `,control_list$`4721   `
                      ,control_list$`4725   `,control_list$`4728   `,control_list$`4736   `,control_list$`4739   `,control_list$`4743   `,control_list$`4746   `
                      ,control_list$`4763   `,control_list$`4803   `,control_list$`4804   `,control_list$`4903   `,control_list$`4904   `,control_list$`4906   `
                      ,control_list$`4912   `,control_list$`4915   `,control_list$`4919   `,control_list$`4927   `,control_list$`4935   `,control_list$`4938   `
                      ,control_list$`4943   `,control_list$`4947   `,control_list$`4953   `,control_list$`4958   `,control_list$`4960   `,control_list$`4961   `
                      ,control_list$`4966   `,control_list$`4971   `,control_list$`4974   `,control_list$`4977   `,control_list$`4979   `,control_list$`4991   `,control_list$`0050   `
                      ,control_list$`5009   `,control_list$`5014   `,control_list$`5234   `,control_list$`5243   `,control_list$`5245   `,control_list$`5263   `
                      ,control_list$`5264   `,control_list$`5269   `,control_list$`5274   `,control_list$`5287   `,control_list$`5288   `,control_list$`5289   `
                      ,control_list$`5299   `,control_list$`5305   `,control_list$`5306   `,control_list$`5315   `,control_list$`5317   `,control_list$`5347   `
                      ,control_list$`5349   `,control_list$`5356   `,control_list$`5371   `,control_list$`5388   `,control_list$`5392   `,control_list$`5425   `
                      ,control_list$`5434   `,control_list$`5439   `,control_list$`5443   `,control_list$`5457   `,control_list$`5469   `,control_list$`5478   `
                      ,control_list$`5483   `,control_list$`5490   `,control_list$`5512   `,control_list$`5530   `,control_list$`5534   `,control_list$`5536   `,control_list$`0056   `
                      ,control_list$`5871   `,control_list$`5876   `,control_list$`0061   `,control_list$`6104   `,control_list$`6105   `,control_list$`6107   `,control_list$`6111   `
                      ,control_list$`6116   `,control_list$`6120   `,control_list$`6121   `,control_list$`6138   `,control_list$`6139   `,control_list$`6143   `
                      ,control_list$`6146   `,control_list$`6147   `,control_list$`6150   `,control_list$`6153   `,control_list$`6158   `,control_list$`6173   `
                      ,control_list$`6175   `,control_list$`6176   `,control_list$`6180   `,control_list$`6182   `,control_list$`6187   `,control_list$`6188   `
                      ,control_list$`6196   `,control_list$`6202   `,control_list$`6205   `,control_list$`6206   `,control_list$`6207   `,control_list$`6209   `
                      ,control_list$`6213   `,control_list$`6214   `,control_list$`6220   `,control_list$`6223   `,control_list$`6230   `,control_list$`6238   `
                      ,control_list$`6239   `,control_list$`6244   `,control_list$`6245   `,control_list$`6257   `,control_list$`6261   `,control_list$`6269   `
                      ,control_list$`6271   `,control_list$`6274   `,control_list$`6278   `,control_list$`6279   `,control_list$`6282   `,control_list$`6283   `
                      ,control_list$`6284   `,control_list$`6285   `,control_list$`6290   `,control_list$`00636  `,control_list$`00639  `,control_list$`6411   `,control_list$`6412   `,control_list$`6414   `
                      ,control_list$`6415   `,control_list$`6426   `,control_list$`00643  `,control_list$`6435   `,control_list$`6438   `,control_list$`6451   `,control_list$`6452   `
                      ,control_list$`6456   `,control_list$`6462   `,control_list$`6472   `,control_list$`6488   `,control_list$`6496   `,control_list$`6509   `,control_list$`6510   `
                      ,control_list$`6523   `,control_list$`6525   `,control_list$`6548   `,control_list$`6552   `,control_list$`6554   `,control_list$`6561   `
                      ,control_list$`6568   `,control_list$`6569   `,control_list$`6613   `,control_list$`6643   `,control_list$`6666   `,control_list$`6670   `,control_list$`00668  `
                      ,control_list$`8016   `,control_list$`8027   `,control_list$`8038   `,control_list$`8039   `,control_list$`8042   `,control_list$`8043   `
                      ,control_list$`8044   `,control_list$`8046   `,control_list$`8050   `,control_list$`8064   `,control_list$`8066   `,control_list$`8069   `
                      ,control_list$`8070   `,control_list$`8076   `,control_list$`8091   `,control_list$`8105   `,control_list$`8110   `,control_list$`8112   `
                      ,control_list$`8114   `,control_list$`8150   `,control_list$`8155   `,control_list$`8163   `,control_list$`8210   `,control_list$`8213   `
                      ,control_list$`8215   `,control_list$`8234   `,control_list$`8255   `,control_list$`8299   `,control_list$`8341   `,control_list$`8349   `
                      ,control_list$`8358   `,control_list$`8383   `,control_list$`8404   `,control_list$`8406   `,control_list$`8415   `,control_list$`8416   `
                      ,control_list$`8433   `,control_list$`8436   `,control_list$`8437   `,control_list$`8446   `,control_list$`8450   `,control_list$`8462   `
                      ,control_list$`8464   `,control_list$`8489   `,control_list$`8916   `,control_list$`8924   `,control_list$`8926   `,control_list$`8936   `
                      ,control_list$`8938   `,control_list$`8942   `,control_list$`9136   `,control_list$`9802   `,control_list$`9904   `,control_list$`9911   `
                      ,control_list$`9914   `,control_list$`9921   `,control_list$`9934   `,control_list$`9938   `,control_list$`9939   `,control_list$`9941   `
                      ,control_list$`9951   `,control_list$`9958   `,control_list$`M2300  `,control_list$`M2800  `,control_list$`Y9999  `)

names(control_list1) <- y[c(1:nrow(y)),]

newdata <- NULL
for (i in 1:length(put_list)) {#把控制變數的資料丟進data
  
  cat(i, "/", length(put_list), "\n")
  
  temcontrol <- control_list1[[i]]
  
  temcontrol$MDATE <- as.Date(temcontrol$MDATE)
  
  temdata <- put_list[[i]]
  
  temlist <- split(temdata, f= temdata$COID)
  
  for (j in 1:length(temlist)) {
    
    jlist <- temlist[[j]]
    
    jlist$MDATE <- as.Date(jlist$MDATE)
    
    data_x <- merge(jlist, temcontrol, by = "MDATE", all = F)
    
    newdata <- rbind(newdata, data_x)
  }
}

newdata1 <- newdata[,-10]
colnames(newdata1[,2]) <- "COID"

write.csv(newdata1, file = "put_model_data.csv", sep = ",", col.names = TRUE)

put_pooling_model <- plm(newdata1$t1_return~newdata1$avg.moneyness+newdata1$STOCK_ROI+newdata1$prior_day_return+newdata1$IV+newdata1$MONTHRET+newdata1$MONTHTURNOVER+newdata1$ILLIQUIDITY+newdata1$ME, data = newdata1,
                         index = c("MDATE", "COID.x"), model = "pooling")
put_fix_model <- plm(newdata1$t1_return~newdata1$avg.moneyness+newdata1$STOCK_ROI+newdata1$prior_day_return+newdata1$IV+newdata1$MONTHRET+newdata1$MONTHTURNOVER+newdata1$ILLIQUIDITY+newdata1$ME, data = newdata1,
                     index = c("MDATE", "COID.x"), model = "within", effect = "individual")
put_fix_model1 <- plm(newdata1$t1_return~newdata1$avg.moneyness+newdata1$STOCK_ROI+newdata1$prior_day_return+newdata1$IV+newdata1$MONTHRET+newdata1$MONTHTURNOVER+newdata1$ILLIQUIDITY+newdata1$ME, data = newdata1,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")
put_random_model <- plm(newdata1$t1_return~newdata1$avg.moneyness+newdata1$STOCK_ROI+newdata1$prior_day_return+newdata1$IV+newdata1$MONTHRET+newdata1$MONTHTURNOVER+newdata1$ILLIQUIDITY+newdata1$ME, data = newdata1,
                        index = c("MDATE", "COID.x"), model = "random")

summary(put_pooling_model)
summary(put_fix_model)
summary(put_fix_model1)
summary(put_random_model)

if(!require(stargazer))install.packages("stargazer")
sink("put_model(30-90).txt")
bind_model <- stargazer(put_pooling_model, put_fix_model, put_fix_model1, put_random_model, title="Results", digits = 5,  header = TRUE, type = "text", report = "vc*p",
                        column.labels = c("pooling", "FE-individuals", "FE", "RE"), model.names = TRUE)
sink()


put_pooling_model_data <- plm(newdata1$data_t1_return~newdata1$avg.moneyness+newdata1$STOCK_ROI+newdata1$prior_day_return+newdata1$IV+newdata1$MONTHRET+newdata1$MONTHTURNOVER+newdata1$ILLIQUIDITY+newdata1$ME, data = newdata1,
                              index = c("MDATE", "COID.x"), model = "pooling")
put_fix_model_data <- plm(newdata1$data_t1_return~newdata1$avg.moneyness+newdata1$STOCK_ROI+newdata1$prior_day_return+newdata1$IV+newdata1$MONTHRET+newdata1$MONTHTURNOVER+newdata1$ILLIQUIDITY+newdata1$ME, data = newdata1,
                          index = c("MDATE", "COID.x"), model = "within", effect = "individual")
put_fix_model1_data <- plm(newdata1$data_t1_return~newdata1$avg.moneyness+newdata1$STOCK_ROI+newdata1$prior_day_return+newdata1$IV+newdata1$MONTHRET+newdata1$MONTHTURNOVER+newdata1$ILLIQUIDITY+newdata1$ME, data = newdata1,
                           index = c("MDATE", "COID.x"), model = "within", effects = "twoways")
put_random_model_data <- plm(newdata1$data_t1_return~newdata1$avg.moneyness+newdata1$STOCK_ROI+newdata1$prior_day_return+newdata1$IV+newdata1$MONTHRET+newdata1$MONTHTURNOVER+newdata1$ILLIQUIDITY+newdata1$ME, data = newdata1,
                             index = c("MDATE", "COID.x"), model = "random")

sink("put_model(30-90)data return.txt")
bind_model <- stargazer(put_pooling_model_data, put_fix_model_data, put_fix_model1_data, put_random_model_data, title="Results", digits = 5,  header = TRUE, type = "text", report = "vc*p",
                        column.labels = c("pooling", "FE-individuals", "FE", "RE"), model.names = TRUE)
sink()

pFtest(put_fix_model1, put_pooling_model)
pFtest(put_fix_model1_data, put_pooling_model_data)
'混合回歸模型和固定效果模型的檢定，
p<0.05，顯著，因此拒絕虛無假説，
使用固定效果模型較爲適當
p-value is < 0.05 then the fixed effects model is a better choice'

phtest(put_random_model, put_fix_model1)
phtest(put_random_model_data, put_fix_model1_data)
'隨即效果模型和固定效果模型的檢定，
p<0.05，不顯著，因此不拒絕虛無假説，
使用固定效果模型較爲適當'

##################################################################################################################################################
###############################################################     all DATA    ##################################################################
##################################################################################################################################################

##################################################################
#
#
#     全部data跑回歸
#
#
##################################################################
if(!require(plm))install.packages("plm")
library(data.table)
setwd("C:\\Users\\User\\Documents\\研究所\\論文")
allcalldata <- read.csv("call_model_data.csv", sep = ",", stringsAsFactors = F) #控制變數的data
allputdata <- read.csv("put_model_data.csv", sep = ",", stringsAsFactors = F) #控制變數的data

allcalldata <- allcalldata[,-1]
allputdata <- allputdata[,-1]
alldata <- rbind(allcalldata, allputdata)
put.avemoney <- allputdata$avg.moneyness
put.avemoney <- c(rep(NA,646732),put.avemoney)
put.avemoney <- as.data.frame(put.avemoney)
call.avemoney <- allcalldata$avg.moneyness
call.avemoney <-  c(call.avemoney, rep(NA, 155098))
call.avemoney <- as.data.frame(call.avemoney)
allcalldata1 <- allcalldata[,-6]
allputdata1 <- allputdata[,-6]
alldata <- cbind(alldata,call.avemoney,put.avemoney)

avemoneyIV <- as.data.frame(alldata$avg.moneyness*alldata$IV)
colnames(avemoneyIV) = "AveMoneyIV"
call.avemoneyIV <- as.data.frame(alldata$call.avemoney*alldata$IV)
colnames(call.avemoneyIV) = "CallAveMoneyIV"
put.avemoneyIV <- as.data.frame(alldata$put.avemoney*alldata$IV)
colnames(put.avemoneyIV) = "PutAveMoneyIV"
alldata <- cbind(alldata, avemoneyIV, call.avemoneyIV, put.avemoneyIV)

write.csv(alldata, file = "alldata1218.csv", sep = ",", col.names = TRUE)
alldata1 <- read.csv("alldata1218.csv", sep = ",", stringsAsFactors = F) #控制變數的data
alldata <- alldata1[,-1]
alldata[is.na(alldata)] <- 0
if(!require(plm))install.packages("plm")

all_fix_model1 <- plm(alldata$data_t1_return~alldata$avg.moneyness+
                        alldata$STOCK_ROI+alldata$IV+alldata$MONTHRET+alldata$MONTHTURNOVER+alldata$ILLIQUIDITY+alldata$ME, data = alldata,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

all_fix_model2 <- plm(alldata$data_t1_return~alldata$avg.moneyness+alldata$AveMoneyIV+
                        alldata$STOCK_ROI+alldata$IV+alldata$MONTHRET+alldata$MONTHTURNOVER+alldata$ILLIQUIDITY+alldata$ME, data = alldata,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

all_fix_model3 <- plm(alldata$data_t1_return~alldata$call.avemoney+
                        alldata$STOCK_ROI+alldata$IV+alldata$MONTHRET+alldata$MONTHTURNOVER+alldata$ILLIQUIDITY+alldata$ME, data = alldata,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

all_fix_model4 <- plm(alldata$data_t1_return~alldata$call.avemoney+alldata$CallAveMoneyIV+
                        alldata$STOCK_ROI+alldata$IV+alldata$MONTHRET+alldata$MONTHTURNOVER+alldata$ILLIQUIDITY+alldata$ME, data = alldata,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

all_fix_model5 <- plm(alldata$data_t1_return~alldata$put.avemoney+
                        alldata$STOCK_ROI+alldata$IV+alldata$MONTHRET+alldata$MONTHTURNOVER+alldata$ILLIQUIDITY+alldata$ME, data = alldata,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

all_fix_model6 <- plm(alldata$data_t1_return~alldata$put.avemoney+alldata$PutAveMoneyIV+
                        alldata$STOCK_ROI+alldata$IV+alldata$MONTHRET+alldata$MONTHTURNOVER+alldata$ILLIQUIDITY+alldata$ME, data = alldata,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

all_fix_model7 <- plm(alldata$data_t1_return~alldata$call.avemoney+alldata$put.avemoney+
                        alldata$STOCK_ROI+alldata$IV+alldata$MONTHRET+alldata$MONTHTURNOVER+alldata$ILLIQUIDITY+alldata$ME, data = alldata,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

all_fix_model8 <- plm(alldata$data_t1_return~alldata$call.avemoney+alldata$CallAveMoneyIV+alldata$put.avemoney+alldata$PutAveMoneyIV+
                        alldata$STOCK_ROI+alldata$IV+alldata$MONTHRET+alldata$MONTHTURNOVER+alldata$ILLIQUIDITY+alldata$ME, data = alldata,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")


sink("regression_model(30-90)days data return .txt")
bind_model <- stargazer(all_fix_model1, all_fix_model2, all_fix_model3, all_fix_model4, all_fix_model5, all_fix_model6,  all_fix_model7, all_fix_model8,
                        title="Results", digits = 5,  header = TRUE, type = "text", report = "vc*p",
                         model.names = TRUE)
sink()

##################################################################
#
#
#     分開大市值，中小市值，和ETF做三種模型
#
#
##################################################################
#處理資料
library(data.table)

all_list <- split(alldata, f = alldata$STOCK_CODE)
etf_data <- list()
for (i in 1:23) {
  etf_data <- rbind(etf_data, all_list[[i]])
}
etf_data <- rbind(etf_data, all_list$`636`, all_list$`639`, all_list$`643`, all_list$`668`, all_list$`50`, all_list$`56`, all_list$`61`, all_list$`M2300 `,
                  all_list$`M2800 `, all_list$`Y9999 `)
etf_list <- split(etf_data, f = etf_data$STOCK_CODE)

stock_data <- list()
for (i in 24:length(all_list)) {
  
  cat(i, "/", length(all_list), "\n")
  
  stock_data <- rbind(stock_data, all_list[[i]])
  
}
stock_list <- split(stock_data, f = stock_data$STOCK_CODE)
stock_list <- list(all_list$`1101`,all_list$`1102`,all_list$`1201`,all_list$`1210`,all_list$`1215`
                   ,all_list$`1216`,all_list$`1218`,all_list$`1262`,all_list$`1301`,all_list$`1303`,all_list$`1304`,all_list$`1305`
                   ,all_list$`1310`,all_list$`1312`,all_list$`1313`,all_list$`1314`,all_list$`1319`,all_list$`1326`
                   ,all_list$`1338`,all_list$`1402`,all_list$`1409`,all_list$`1444`,all_list$`1455`,all_list$`1464`
                   ,all_list$`1476`,all_list$`1477`,all_list$`1504`,all_list$`1513`,all_list$`1515`,all_list$`1522`
                   ,all_list$`1525`,all_list$`1527`,all_list$`1532`,all_list$`1536`,all_list$`1537`,all_list$`1560`
                   ,all_list$`1565`,all_list$`1569`,all_list$`1580`,all_list$`1582`,all_list$`1586`,all_list$`1589`
                   ,all_list$`1590`,all_list$`1597`,all_list$`1605`,all_list$`1608`,all_list$`1609`,all_list$`1704`
                   ,all_list$`1707`,all_list$`1710`,all_list$`1714`,all_list$`1718`,all_list$`1722`,all_list$`1723`
                   ,all_list$`1752`,all_list$`1760`,all_list$`1795`,all_list$`1802`,all_list$`1808`,all_list$`1815`
                   ,all_list$`1904`,all_list$`1905`,all_list$`1907`,all_list$`1909`,all_list$`2002`,all_list$`2014`
                   ,all_list$`2023`,all_list$`2027`,all_list$`2029`,all_list$`2031`,all_list$`2034`,all_list$`2049`
                   ,all_list$`2103`,all_list$`2104`,all_list$`2105`,all_list$`2208`,all_list$`2228`,all_list$`2231`
                   ,all_list$`2233`,all_list$`2239`,all_list$`2301`,all_list$`2303`,all_list$`2308`,all_list$`2311`
                   ,all_list$`2312`,all_list$`2313`,all_list$`2314`,all_list$`2316`,all_list$`2317`,all_list$`2323`
                   ,all_list$`2324`,all_list$`2327`,all_list$`2328`,all_list$`2330`,all_list$`2331`,all_list$`2332`
                   ,all_list$`2337`,all_list$`2338`,all_list$`2340`,all_list$`2344`,all_list$`2345`,all_list$`2347`
                   ,all_list$`2351`,all_list$`2352`,all_list$`2353`,all_list$`2354`,all_list$`2355`,all_list$`2356`
                   ,all_list$`2360`,all_list$`2363`,all_list$`2367`,all_list$`2368`,all_list$`2371`,all_list$`2375`
                   ,all_list$`2376`,all_list$`2377`,all_list$`2379`,all_list$`2382`,all_list$`2383`,all_list$`2392`
                   ,all_list$`2393`,all_list$`2401`,all_list$`2404`,all_list$`2406`,all_list$`2408`,all_list$`2409`
                   ,all_list$`2412`,all_list$`2421`,all_list$`2426`,all_list$`2428`,all_list$`2439`,all_list$`2448`
                   ,all_list$`2449`,all_list$`2451`,all_list$`2454`,all_list$`2455`,all_list$`2456`,all_list$`2458`
                   ,all_list$`2464`,all_list$`2472`,all_list$`2474`,all_list$`2478`,all_list$`2481`,all_list$`2485`
                   ,all_list$`2489`,all_list$`2492`,all_list$`2498`,all_list$`2515`,all_list$`2520`,all_list$`2534`
                   ,all_list$`2542`,all_list$`2545`,all_list$`2548`,all_list$`2601`,all_list$`2603`,all_list$`2605`
                   ,all_list$`2606`,all_list$`2609`,all_list$`2610`,all_list$`2614`,all_list$`2615`,all_list$`2617`
                   ,all_list$`2618`,all_list$`2633`,all_list$`2723`,all_list$`2726`,all_list$`2727`,all_list$`2732`
                   ,all_list$`2801`,all_list$`2823`,all_list$`2834`,all_list$`2880`,all_list$`2881`,all_list$`2882`
                   ,all_list$`2883`,all_list$`2884`,all_list$`2885`,all_list$`2886`,all_list$`2887`,all_list$`2888`
                   ,all_list$`2890`,all_list$`2891`,all_list$`2892`,all_list$`2913`,all_list$`2915`,all_list$`2928`
                   ,all_list$`2929`,all_list$`3003`,all_list$`3005`,all_list$`3006`,all_list$`3008`,all_list$`3016`
                   ,all_list$`3017`,all_list$`3019`,all_list$`3023`,all_list$`3026`,all_list$`3030`,all_list$`3034`
                   ,all_list$`3035`,all_list$`3036`,all_list$`3037`,all_list$`3042`,all_list$`3044`,all_list$`3045`
                   ,all_list$`3049`,all_list$`3059`,all_list$`3068`,all_list$`3078`,all_list$`3081`,all_list$`3083`
                   ,all_list$`3088`,all_list$`3090`,all_list$`3092`,all_list$`3105`,all_list$`3131`,all_list$`3141`
                   ,all_list$`3152`,all_list$`3163`,all_list$`3189`,all_list$`3205`,all_list$`3211`,all_list$`3217`
                   ,all_list$`3221`,all_list$`3224`,all_list$`3227`,all_list$`3231`,all_list$`3234`,all_list$`3236`
                   ,all_list$`3260`,all_list$`3264`,all_list$`3265`,all_list$`3289`,all_list$`3293`,all_list$`3299`
                   ,all_list$`3317`,all_list$`3323`,all_list$`3324`,all_list$`3346`,all_list$`3362`,all_list$`3363`
                   ,all_list$`3374`,all_list$`3376`,all_list$`3380`,all_list$`3406`,all_list$`3413`,all_list$`3428`
                   ,all_list$`3443`,all_list$`3450`,all_list$`3454`,all_list$`3455`,all_list$`3479`,all_list$`3481`
                   ,all_list$`3484`,all_list$`3490`,all_list$`3491`,all_list$`3498`,all_list$`3504`,all_list$`3508`
                   ,all_list$`3514`,all_list$`3515`,all_list$`3522`,all_list$`3526`,all_list$`3527`,all_list$`3529`
                   ,all_list$`3532`,all_list$`3533`,all_list$`3541`,all_list$`3545`,all_list$`3546`,all_list$`3548`
                   ,all_list$`3552`,all_list$`3556`,all_list$`3558`,all_list$`3563`,all_list$`3576`,all_list$`3587`
                   ,all_list$`3596`,all_list$`3611`,all_list$`3617`,all_list$`3624`,all_list$`3645`,all_list$`3653`
                   ,all_list$`3661`,all_list$`3665`,all_list$`3673`,all_list$`3689`,all_list$`3691`,all_list$`3698`
                   ,all_list$`3702`,all_list$`3706`,all_list$`3707`,all_list$`3708`,all_list$`3711`,all_list$`4105`
                   ,all_list$`4107`,all_list$`4114`,all_list$`4119`,all_list$`4120`,all_list$`4123`,all_list$`4126`
                   ,all_list$`4128`,all_list$`4129`,all_list$`4130`,all_list$`4137`,all_list$`4147`,all_list$`4148`
                   ,all_list$`4157`,all_list$`4162`,all_list$`4163`,all_list$`4167`,all_list$`4190`,all_list$`4426`
                   ,all_list$`4528`,all_list$`4532`,all_list$`4536`,all_list$`4551`,all_list$`4552`,all_list$`4721`
                   ,all_list$`4725`,all_list$`4728`,all_list$`4736`,all_list$`4739`,all_list$`4743`,all_list$`4746`
                   ,all_list$`4763`,all_list$`4803`,all_list$`4804`,all_list$`4903`,all_list$`4904`,all_list$`4906`
                   ,all_list$`4912`,all_list$`4915`,all_list$`4919`,all_list$`4927`,all_list$`4935`,all_list$`4938`
                   ,all_list$`4943`,all_list$`4947`,all_list$`4953`,all_list$`4958`,all_list$`4960`,all_list$`4961`
                   ,all_list$`4966`,all_list$`4971`,all_list$`4974`,all_list$`4977`,all_list$`4979`,all_list$`4991`
                   ,all_list$`5009`,all_list$`5014`,all_list$`5234`,all_list$`5243`,all_list$`5245`,all_list$`5263`
                   ,all_list$`5264`,all_list$`5269`,all_list$`5274`,all_list$`5287`,all_list$`5288`,all_list$`5289`
                   ,all_list$`5299`,all_list$`5305`,all_list$`5306`,all_list$`5315`,all_list$`5317`,all_list$`5347`
                   ,all_list$`5349`,all_list$`5356`,all_list$`5371`,all_list$`5388`,all_list$`5392`,all_list$`5425`
                   ,all_list$`5434`,all_list$`5439`,all_list$`5443`,all_list$`5457`,all_list$`5469`,all_list$`5478`
                   ,all_list$`5483`,all_list$`5490`,all_list$`5512`,all_list$`5530`,all_list$`5534`,all_list$`5536`
                   ,all_list$`5871`,all_list$`5876`,all_list$`6104`,all_list$`6105`,all_list$`6107`,all_list$`6111`
                   ,all_list$`6116`,all_list$`6120`,all_list$`6121`,all_list$`6138`,all_list$`6139`,all_list$`6143`
                   ,all_list$`6146`,all_list$`6147`,all_list$`6150`,all_list$`6153`,all_list$`6158`,all_list$`6173`
                   ,all_list$`6175`,all_list$`6176`,all_list$`6180`,all_list$`6182`,all_list$`6187`,all_list$`6188`
                   ,all_list$`6196`,all_list$`6202`,all_list$`6205`,all_list$`6206`,all_list$`6207`,all_list$`6209`
                   ,all_list$`6213`,all_list$`6214`,all_list$`6220`,all_list$`6223`,all_list$`6230`,all_list$`6238`
                   ,all_list$`6239`,all_list$`6244`,all_list$`6245`,all_list$`6257`,all_list$`6261`,all_list$`6269`
                   ,all_list$`6271`,all_list$`6274`,all_list$`6278`,all_list$`6279`,all_list$`6282`,all_list$`6283`
                   ,all_list$`6284`,all_list$`6285`,all_list$`6290`,all_list$`6411`,all_list$`6412`,all_list$`6414`
                   ,all_list$`6415`,all_list$`6426`,all_list$`6435`,all_list$`6438`,all_list$`6451`,all_list$`6452`
                   ,all_list$`6456`,all_list$`6462`,all_list$`6472`,all_list$`6488`,all_list$`6496`,all_list$`6509`,all_list$`6510`
                   ,all_list$`6523`,all_list$`6525`,all_list$`6548`,all_list$`6552`,all_list$`6554`,all_list$`6561`
                   ,all_list$`6568`,all_list$`6569`,all_list$`6613`,all_list$`6643`,all_list$`6666`,all_list$`6670`
                   ,all_list$`8016`,all_list$`8027`,all_list$`8038`,all_list$`8039`,all_list$`8042`,all_list$`8043`
                   ,all_list$`8044`,all_list$`8046`,all_list$`8050`,all_list$`8064`,all_list$`8066`,all_list$`8069`
                   ,all_list$`8070`,all_list$`8076`,all_list$`8091`,all_list$`8105`,all_list$`8110`,all_list$`8112`
                   ,all_list$`8114`,all_list$`8150`,all_list$`8155`,all_list$`8163`,all_list$`8210`,all_list$`8213`
                   ,all_list$`8215`,all_list$`8234`,all_list$`8255`,all_list$`8299`,all_list$`8341`,all_list$`8349`
                   ,all_list$`8358`,all_list$`8383`,all_list$`8404`,all_list$`8406`,all_list$`8415`,all_list$`8416`
                   ,all_list$`8433`,all_list$`8436`,all_list$`8437`,all_list$`8446`,all_list$`8450`,all_list$`8462`
                   ,all_list$`8464`,all_list$`8489`,all_list$`8916`,all_list$`8924`,all_list$`8926`,all_list$`8936`
                   ,all_list$`8938`,all_list$`8942`,all_list$`9136`,all_list$`9802`,all_list$`9904`,all_list$`9911`
                   ,all_list$`9914`,all_list$`9921`,all_list$`9934`,all_list$`9938`,all_list$`9939`,all_list$`9941`
                   ,all_list$`9951`,all_list$`9958`)
stock_data <- rbindlist(stock_list)
stock_list <- split(stock_data, f = stock_data$STOCK_CODE)

me_data <- stock_data[,c(1,3,9)]
me_list <- split(me_data, f = me_data$STOCK_CODE)

me_data1 <- NULL
for (i in 1:length(me_list)) {
  
  cat(i, "/", length(me_list), "\n")
  
  tem <- me_list[[i]]
  
  me <- as.numeric(round(mean(tem$ME),3))
  
  temcode <- unique(tem$STOCK_CODE)
  
  temdata <- cbind(temcode, me)
  
  me_data1 <- rbind(me_data1, temdata)
  
}
me_data1 <- as.data.frame(me_data1)
colnames(me_data1) <- c("STOCK_CODE", "ME")
me_data1 <- me_data1[order(me_data1[,2], decreasing = F),]

big_stock <- subset(me_data1[c(104:153),])
small_stock <- subset(me_data1[-c(104:153),])
big_stock <- as.vector(big_stock$STOCK_CODE)
small_stock <- as.vector(small_stock$STOCK_CODE)

big_data <- NULL
for (i in 1:length(big_stock)) {
  
  cat(i, "/", length(big_stock), "\n")
  
  tem <- big_stock[i]
  
  big_data <- rbind(big_data, stock_list[[tem]])
  
}  

small_data <- NULL  
for (i in 1:length(small_stock)) {
  
  cat(i, "/", length(small_stock), "\n")
  
  tem <- small_stock[i]
  
  small_data <- rbind(small_data, stock_list[[tem]])
  
}  

save(etf_data,small_data, big_data, file = "regression data.RData")
##################################################################
#
#
#     ETF模型
#
#
##################################################################

if(!require(plm))install.packages("plm")
if(!require(stargazer))install.packages("stargazer")

etf_fix_model1 <- plm(etf_data$data_t1_return~etf_data$avg.moneyness+
                        etf_data$STOCK_ROI+etf_data$prior_day_return+etf_data$IV+etf_data$MONTHRET+etf_data$MONTHTURNOVER+etf_data$ILLIQUIDITY+etf_data$ME, data = etf_data,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

etf_fix_model2 <- plm(etf_data$data_t1_return~etf_data$avg.moneyness+etf_data$AveMoneyIV+
                        etf_data$STOCK_ROI+etf_data$prior_day_return+etf_data$IV+etf_data$MONTHRET+etf_data$MONTHTURNOVER+etf_data$ILLIQUIDITY+etf_data$ME, data = etf_data,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

etf_fix_model3 <- plm(etf_data$data_t1_return~etf_data$call.avemoney+
                        etf_data$STOCK_ROI+etf_data$prior_day_return+etf_data$IV+etf_data$MONTHRET+etf_data$MONTHTURNOVER+etf_data$ILLIQUIDITY+etf_data$ME, data = etf_data,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

etf_fix_model4 <- plm(etf_data$data_t1_return~etf_data$call.avemoney+etf_data$CallAveMoneyIV+
                        etf_data$STOCK_ROI+etf_data$prior_day_return+etf_data$IV+etf_data$MONTHRET+etf_data$MONTHTURNOVER+etf_data$ILLIQUIDITY+etf_data$ME, data = etf_data,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

etf_fix_model5 <- plm(etf_data$data_t1_return~etf_data$put.avemoney+
                        etf_data$STOCK_ROI+etf_data$prior_day_return+etf_data$IV+etf_data$MONTHRET+etf_data$MONTHTURNOVER+etf_data$ILLIQUIDITY+etf_data$ME, data = etf_data,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

etf_fix_model6 <- plm(etf_data$data_t1_return~etf_data$put.avemoney+etf_data$PutAveMoneyIV+
                        etf_data$STOCK_ROI+etf_data$prior_day_return+etf_data$IV+etf_data$MONTHRET+etf_data$MONTHTURNOVER+etf_data$ILLIQUIDITY+etf_data$ME, data = etf_data,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

etf_fix_model7 <- plm(etf_data$data_t1_return~etf_data$call.avemoney+etf_data$put.avemoney+
                        etf_data$STOCK_ROI+etf_data$prior_day_return+etf_data$IV+etf_data$MONTHRET+etf_data$MONTHTURNOVER+etf_data$ILLIQUIDITY+etf_data$ME, data = etf_data,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

etf_fix_model8 <- plm(etf_data$data_t1_return~etf_data$call.avemoney+etf_data$CallAveMoneyIV+etf_data$put.avemoney+etf_data$PutAveMoneyIV+
                        etf_data$STOCK_ROI+etf_data$prior_day_return+etf_data$IV+etf_data$MONTHRET+etf_data$MONTHTURNOVER+etf_data$ILLIQUIDITY+etf_data$ME, data = etf_data,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")


sink("ETF_model(30-90)days return .txt")
bind_model <- stargazer(etf_fix_model1, etf_fix_model2, etf_fix_model3, etf_fix_model4, etf_fix_model5, etf_fix_model6,  etf_fix_model7, etf_fix_model8,
                        title="Results", digits = 5,  header = TRUE, type = "text", report = "vc*p",
                        model.names = TRUE)
sink()
##################################################################
#
#
#     大市值m模型
#
#
##################################################################


big_fix_model1 <- plm(big_data$data_t1_return~big_data$avg.moneyness+
                        big_data$STOCK_ROI+big_data$prior_day_return+big_data$IV+big_data$MONTHRET+big_data$MONTHTURNOVER+big_data$ILLIQUIDITY+big_data$ME, data = big_data,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

big_fix_model2 <- plm(big_data$data_t1_return~big_data$avg.moneyness+big_data$AveMoneyIV+
                        big_data$STOCK_ROI+big_data$prior_day_return+big_data$IV+big_data$MONTHRET+big_data$MONTHTURNOVER+big_data$ILLIQUIDITY+big_data$ME, data = big_data,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

big_fix_model3 <- plm(big_data$data_t1_return~big_data$call.avemoney+
                        big_data$STOCK_ROI+big_data$prior_day_return+big_data$IV+big_data$MONTHRET+big_data$MONTHTURNOVER+big_data$ILLIQUIDITY+big_data$ME, data = big_data,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

big_fix_model4 <- plm(big_data$data_t1_return~big_data$call.avemoney+big_data$CallAveMoneyIV+
                        big_data$STOCK_ROI+big_data$prior_day_return+big_data$IV+big_data$MONTHRET+big_data$MONTHTURNOVER+big_data$ILLIQUIDITY+big_data$ME, data = big_data,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

big_fix_model5 <- plm(big_data$data_t1_return~big_data$put.avemoney+
                        big_data$STOCK_ROI+big_data$prior_day_return+big_data$IV+big_data$MONTHRET+big_data$MONTHTURNOVER+big_data$ILLIQUIDITY+big_data$ME, data = big_data,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

big_fix_model6 <- plm(big_data$data_t1_return~big_data$put.avemoney+big_data$PutAveMoneyIV+
                        big_data$STOCK_ROI+big_data$prior_day_return+big_data$IV+big_data$MONTHRET+big_data$MONTHTURNOVER+big_data$ILLIQUIDITY+big_data$ME, data = big_data,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

big_fix_model7 <- plm(big_data$data_t1_return~big_data$call.avemoney+big_data$put.avemoney+
                        big_data$STOCK_ROI+big_data$prior_day_return+big_data$IV+big_data$MONTHRET+big_data$MONTHTURNOVER+big_data$ILLIQUIDITY+big_data$ME, data = big_data,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

big_fix_model8 <- plm(big_data$data_t1_return~big_data$call.avemoney+big_data$CallAveMoneyIV+big_data$put.avemoney+big_data$PutAveMoneyIV+
                        big_data$STOCK_ROI+big_data$prior_day_return+big_data$IV+big_data$MONTHRET+big_data$MONTHTURNOVER+big_data$ILLIQUIDITY+big_data$ME, data = big_data,
                      index = c("MDATE", "COID.x"), model = "within", effects = "twoways")


sink("big_model(30-90)days return .txt")
bind_model <- stargazer(big_fix_model1, big_fix_model2, big_fix_model3, big_fix_model4, big_fix_model5, big_fix_model6,  big_fix_model7, big_fix_model8,
                        title="Results", digits = 5,  header = TRUE, type = "text", report = "vc*p",
                        model.names = TRUE)
sink()


##################################################################
#
#
#     中小市值模型
#
#
##################################################################

small_fix_model1 <- plm(small_data$data_t1_return~small_data$avg.moneyness+
                          small_data$STOCK_ROI+small_data$prior_day_return+small_data$IV+small_data$MONTHRET+small_data$MONTHTURNOVER+small_data$ILLIQUIDITY+small_data$ME, data = small_data,
                        index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

small_fix_model2 <- plm(small_data$data_t1_return~small_data$avg.moneyness+small_data$AveMoneyIV+
                          small_data$STOCK_ROI+small_data$prior_day_return+small_data$IV+small_data$MONTHRET+small_data$MONTHTURNOVER+small_data$ILLIQUIDITY+small_data$ME, data = small_data,
                        index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

small_fix_model3 <- plm(small_data$data_t1_return~small_data$call.avemoney+
                          small_data$STOCK_ROI+small_data$prior_day_return+small_data$IV+small_data$MONTHRET+small_data$MONTHTURNOVER+small_data$ILLIQUIDITY+small_data$ME, data = small_data,
                        index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

small_fix_model4 <- plm(small_data$data_t1_return~small_data$call.avemoney+small_data$CallAveMoneyIV+
                          small_data$STOCK_ROI+small_data$prior_day_return+small_data$IV+small_data$MONTHRET+small_data$MONTHTURNOVER+small_data$ILLIQUIDITY+small_data$ME, data = small_data,
                        index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

small_fix_model5 <- plm(small_data$data_t1_return~small_data$put.avemoney+
                          small_data$STOCK_ROI+small_data$prior_day_return+small_data$IV+small_data$MONTHRET+small_data$MONTHTURNOVER+small_data$ILLIQUIDITY+small_data$ME, data = small_data,
                        index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

small_fix_model6 <- plm(small_data$data_t1_return~small_data$put.avemoney+small_data$PutAveMoneyIV+
                          small_data$STOCK_ROI+small_data$prior_day_return+small_data$IV+small_data$MONTHRET+small_data$MONTHTURNOVER+small_data$ILLIQUIDITY+small_data$ME, data = small_data,
                        index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

small_fix_model7 <- plm(small_data$data_t1_return~small_data$call.avemoney+small_data$put.avemoney+
                          small_data$STOCK_ROI+small_data$prior_day_return+small_data$IV+small_data$MONTHRET+small_data$MONTHTURNOVER+small_data$ILLIQUIDITY+small_data$ME, data = small_data,
                        index = c("MDATE", "COID.x"), model = "within", effects = "twoways")

small_fix_model8 <- plm(small_data$data_t1_return~small_data$call.avemoney+small_data$CallAveMoneyIV+small_data$put.avemoney+small_data$PutAveMoneyIV+
                          small_data$STOCK_ROI+small_data$prior_day_return+small_data$IV+small_data$MONTHRET+small_data$MONTHTURNOVER+small_data$ILLIQUIDITY+small_data$ME, data = small_data,
                        index = c("MDATE", "COID.x"), model = "within", effects = "twoways")


sink("small_model(30-90)days return .txt")
bind_model <- stargazer(small_fix_model1, small_fix_model2, small_fix_model3, small_fix_model4, small_fix_model5, small_fix_model6,  small_fix_model7, small_fix_model8,
                        title="Results", digits = 5,  header = TRUE, type = "text", report = "vc*p",
                        model.names = TRUE)
sink()



















