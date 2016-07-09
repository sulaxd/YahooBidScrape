# migration
library(mongolite)
library(plotly)
library(jiebaR)
library(wordcloud)
library(plyr)

# 從MongoDB匯入資料為DataFrame: rawdata
m <- mongo(collection = "iphone6", url="mongodb://104.199.134.206")
rawdata<-m$find()

# 變數選擇＆型別轉換
rawdata$Date <- as.Date(rawdata$srp.pdtime)
rawdata$Rating <- strtoi(substr(rawdata$srp.rating, 4, nchar(rawdata$srp.rating)))
rawdata$Place <- substr(rawdata$srp.place, 3, nchar(rawdata$srp.place))
rawdata$Seller <- substr(rawdata$srp.seller.yui3.g, 5, nchar(rawdata$srp.seller.yui3.g))
iphone6s <- rawdata[c('Date', 'Place','Seller', 'Rating','srp.pdtitle', 'srp.pdprice.yui3.g')]

# 交叉表
# View(table(iphone6s$Date))
# View(table(iphone6s$Seller))
# View(table(iphone6s$Place))

# 未去除離群值之前的統計圖
t1 <- aggregate(srp.pdprice.yui3.g~Date, iphone6s, mean)
plot_ly(t1, x = Date, y = srp.pdprice.yui3.g) %>% layout(yaxis = list(title='價格'), xaxis = list(title='時間')) # 依時間變化之價格趨勢(線圖)
plot_ly(iphone6s, x = Date, y = srp.pdprice.yui3.g, type = 'box') %>% layout(yaxis = list(title='價格'), xaxis = list(title='時間')) # 依時間變化之價格趨勢(盒型圖)

# 以 coef = 1.5去除離群值後之DF
boxplot.stats(iphone6s$srp.pdprice.yui3.g, coef = 1.5)
# 得出extreme of the lower whisker = 10500, extreme of the upper whisker = 15800
newdata = subset(iphone6s, srp.pdprice.yui3.g <= 15800 & srp.pdprice.yui3.g >=10500)

#文字雲
cc <- worker()
s <- cc[newdata$srp.pdtitle]
s <- gsub("[0-9]+?","",s)
tableWord<-count(s)
par(family=("Heiti TC Light"))
wordcloud(tableWord[,1],tableWord[,2], scale=c(10,0.8))
# 交叉表
#View(table(newdata$Date))
#View(table(newdata$Seller))
#View(table(newdata$Place))

# 去除離群值之後的統計圖
plot_ly(newdata, x = Date, y = srp.pdprice.yui3.g, type = 'box')%>% layout(yaxis = list(title='價格'), xaxis = list(title='時間')) # 依時間變化之價格趨勢(盒型圖)
t2 <- aggregate(srp.pdprice.yui3.g~Date, newdata, mean)
fit <- lm(t2$srp.pdprice.yui3.g~t2$Date)
plot_ly(t2, x = Date, y = srp.pdprice.yui3.g) %>% add_trace(x = Date, y= fitted(fit), mode="lines") %>% layout(yaxis = list(title='價格'), xaxis = list(title='時間')) # 依時間變化之價格趨勢(線圖)
t3 <- aggregate(srp.pdprice.yui3.g~Place, newdata, mean)
plot_ly(t3, x = Place, y = srp.pdprice.yui3.g, type='bar') %>% layout(yaxis = list(title='價格'), xaxis = list(title='地點')) # 不同縣市之平均價格

