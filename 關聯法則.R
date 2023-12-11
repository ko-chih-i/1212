library(dplyr)

table<-read.csv("C:\\Users\\88698\\OneDrive\\桌面\\學習單(共).csv")

is.na(table)
sum(is.na(table))
#填補遺失值
mean.國文學科能力測驗<-mean(table[,1],na.rm=T)
na.rows<-is.na(table[,1])
table[na.rows,1]<-mean.國文學科能力測驗

mean.過去模擬考中各科通常得到幾級分<-mean(table[,2],na.rm=T)
na.rows<-is.na(table[,2])
table[na.rows,2]<-mean.過去模擬考中各科通常得到幾級分

table(table$擔心國文考不好)
na.rows<-is.na(table[,3])
table[na.rows,3]<-"3"
sum(is.na(table))

table(table$國文科考試時緊張的程度)
na.rows<-is.na(table[,4])
table[na.rows,4]<-"3"
sum(is.na(table))

table(table$我擔心考不好會被父母責怪)
na.rows<-is.na(table[,5])
table[na.rows,5]<-"2"


table(table$我覺得自己準備得很充分)
na.rows<-is.na(table[,6])
table[na.rows,6]<-"從沒有或很少有"
sum(is.na(table))

table(table$我覺得考不好會比別人矮一截)
na.rows<-is.na(table[,7])
table[na.rows,7]<-"常有"
sum(is.na(table))

table <- table %>%
  mutate(
    國文科考試時緊張的程度 = recode(國文科考試時緊張的程度,
                         "非常緊張" = 5,
                         "完全不緊張" = 1,
                         "2" = 2,
                         "3" = 3,
                         "4" = 4
    ),
    
    我覺得自己準備得很充分 = recode(我覺得自己準備得很充分,
                         "從沒有或很少有" = 1,
                         "偶爾" = 2,
                         "常有" = 3,
                         "大半時間或經常如此" = 4
    ),
    
    我覺得考不好會比別人矮一截 = recode(我覺得考不好會比別人矮一截,
                           "從沒有或很少有" = 1,
                           "偶爾" = 2,
                           "常有" = 3,
                           "大半時間或經常如此" = 4
    ),
  )


#轉類別變數 / 連續型變數


table$擔心國文考不好 <- as.factor(table$擔心國文考不好)
table$我擔心考不好會被父母責怪 <- as.factor(table$我擔心考不好會被父母責怪)
table$國文科考試時緊張的程度 <- as.factor(table$國文科考試時緊張的程度)
table$我覺得自己準備得很充分 <- as.factor(table$我覺得自己準備得很充分)
table$我覺得考不好會比別人矮一截 <- as.factor(table$我覺得考不好會比別人矮一截)



require(arules)
rule<-apriori(table,parameter=list(supp=0.02,conf=0.5),
              appearance = list(rhs=c("國文科考試時緊張的程度=1","國文科考試時緊張的程度=2","國文科考試時緊張的程度=3","國文科考試時緊張的程度=4","國文科考試時緊張的程度=5")))
sort.rule<-sort(rule,by="support")
subset.matrix<-as.matrix(is.subset(x=sort.rule,y=sort.rule))
subset.matrix[lower.tri(subset.matrix,diag =T)]<-NA

redundant<-colSums(subset.matrix, na.rm=T)>=1#重複的
sort.rule<-sort.rule[!redundant]#拿掉
sort.rule<-as(sort.rule,"data.frame")
write.csv(sort.rule,"關聯法則機器.csv",fileEncoding="Big5")

