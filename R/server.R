library(shiny)
library(ggplot2)
library(leaflet)
library(mongolite)  
library(plotly)
library(magrittr)

shinyServer(function(input, output) {
  
  output$mytable = renderDataTable({
    newdata <- loadDataMongo()
  })
  
  output$text1 <- renderText({ 
    paste("You have typed", input$productKeyword)
  })
  
  
  output$locationPlot <- renderPlot({
    newdata <- loadDataMongo()
    p <- ggplot(newdata, aes(x=newdata$Place, y=newdata$srp.pdprice.yui3)) + geom_point()
    print(p)
    
  }, height=700)
  
  output$markPlot <- renderPlotly({
    p <- loadDataVisual()
    print(p)
  })
  
  
  keyToSearch <- reactive({
    input$productKeyword
  })
  keyToRemove <- reactive({
    input$removeKeyword
  })
  keyToInclude <- reactive({
    input$includeKeyword
  })



})



loadDataVisual <- function(){
  
  
  m <- mongo(collection = "iphone6", url="mongodb://104.199.134.206") 
  m <- m$find()
  
  price <- m$srp.pdprice.yui3.g
  date <- as.Date(m$srp.pdtime)
  t1 <- aggregate(price~date, m, mean)
  
  fit <- lm(t1$price~t1$date)
  
  plot_ly(t1, x = date, y = price) %>% add_trace(x = date, y= fitted(fit), mode="lines")
  plot_ly(m, x = date, y = price, type = 'box')
  price <- m$srp.pdprice.yui3.g
  date <- as.Date(m$srp.pdtime)
  t1 <- aggregate(price~date, m, mean)
  
  fit <- lm(t1$price~t1$date)
  
  p <- plot_ly(t1, x = date, y = price) %>% add_trace(x = date, y= fitted(fit), mode="lines")
  return(p)
}


loadDataMongo <- function(){
  m <- mongo(collection = "iphone6", url="mongodb://104.199.134.206")
  rawdata<-m$find()
  # 變數選擇＆型別轉換
  rawdata$Date <- as.Date(rawdata$srp.pdtime)
  rawdata$Rating <- strtoi(substr(rawdata$srp.rating, 4, nchar(rawdata$srp.rating)))
  rawdata$Place <- substr(rawdata$srp.place, 3, nchar(rawdata$srp.place))
  rawdata$Seller <- substr(rawdata$srp.seller.yui3.g, 5, nchar(rawdata$srp.seller.yui3.g))
  iphone6s <- rawdata[c('Date', 'Place','Seller', 'Rating','srp.pdtitle', 'srp.pdprice.yui3.g')]
  # 未去除離群值之前的統計圖
  t1 <- aggregate(srp.pdprice.yui3.g~Date, iphone6s, mean)
  plot_ly(t1, x = Date, y = srp.pdprice.yui3.g) # 依時間變化之價格趨勢(線圖)
  plot_ly(iphone6s, x = Date, y = srp.pdprice.yui3.g, type = 'box') # 依時間變化之價格趨勢(盒型圖)
  
  # 以 coef = 1.5去除離群值後之DF
  boxplot.stats(iphone6s$srp.pdprice.yui3.g, coef = 1.5)
  # 得出extreme of the lower whisker = 10500, extreme of the upper whisker = 15800
  newdata = subset(iphone6s, srp.pdprice.yui3.g <= 15800 & srp.pdprice.yui3.g >=10500)
  return(newdata)
}

scrape_yahoo <- function(main.keywords, filter.words.remove, filter.words.keep){
  require(plyr)
  require(rvest)
  require(magrittr)
  
  result <- list()
  page=999
  i=1
  options(stringsAsFactors = F)
  
  url <- sprintf("https://tw.search.bid.yahoo.com/search/auction/product;_ylt=Ag3dtVJIWUOh2aOrJrw9RWFyFbN8;_ylv=3?kw=iphone6&p=iphone6&property=auction&sub_property=auction&srch=product&aoffset=0&poffset=0&pg=1&sort=-ptime&nst=1&act=srp&rescheck=1&pptf=3&cid=23960&clv=2",
                 main.keywords, main.keywords, (i-1)*60, i)
  dta <- read_html(url)
  
  if(page==999){
    options(warn=-1)
    pageUrls <- c()
    tmp <- dta %>% html_nodes(xpath = "//div[@class='srp_pagination srp_pjax ']") %>% .[[1]] %>%
      html_children() %>% .[[1]] %>% html_children()
    pageUrl <- html_children(tmp) %>% html_attr("href")
    pages <- dta %>% html_nodes(xpath = "//div[@class='srp_pagination srp_pjax ']") %>% .[[1]] %>%
      html_children() %>% .[[1]] %>% html_children() %>% html_text()
    page <- pages %>% as.integer() %>% .[!is.na(.)] %>% max()
    
    while(any(grepl("下十頁",pages))){
      tmp <- paste0("https://tw.search.bid.yahoo.com/search/auction/", pageUrl[which.max(as.integer(sapply(strsplit(pageUrl, "&|="), function(u){u[grep("pg",u)+1]})))])
      print(tmp)
      dta <- read_html(tmp)
      pages <- dta %>% html_nodes(xpath = "//div[@class='srp_pagination srp_pjax ']") %>% .[[1]] %>%
        html_children() %>% .[[1]] %>% html_children() %>% html_text()
      page <- pages %>% as.integer() %>% .[!is.na(.)] %>% max()
      
      # page url
      tmp <- dta %>% html_nodes(xpath = "//div[@class='srp_pagination srp_pjax ']") %>% .[[1]] %>%
        html_children() %>% .[[1]] %>% html_children()
      pageUrl <- html_children(tmp) %>% html_attr("href")
      pages <- dta %>% html_nodes(xpath = "//div[@class='srp_pagination srp_pjax ']") %>% .[[1]] %>%
        html_children() %>% .[[1]] %>% html_children() %>% html_text()
      page <- pages %>% as.integer() %>% .[!is.na(.)] %>% max()
      pageUrls <- c(pageUrls, pageUrl)
    }
    pageUrls <- unique(pageUrls)
    options(warn=0)
  }
  
  for(i in 1:length(pageUrls)){
    url <- sprintf("https://tw.search.bid.yahoo.com/search/auction/%s",pageUrls[i])
    dta <- read_html(url)
    cat("正在擷取第",i,"頁，共",length(pageUrls),"頁 \n")
    item_list <- (dta %>% html_nodes(xpath = "//div[@class='list-type yui3-g']"))[[1]] %>% html_children()
    
    dta_collection <- lapply(item_list, function(u){
      result_tmp <- list()
      tmp <- html_children(u)[[grep("wrap",html_children(u))]] %>% html_children()
      result_tmp[[1]] <- tmp[[1]] %>% html_children() %>% .[[1]] %>% html_children() %>% html_attr('src') %>% set_names("img_link")
      result_tmp[[2]] <- tmp[[2]] %>% html_children() %>% .[[1]] %>% html_children() %>% .[1] %>% html_children() %>% html_attr('href') %>% set_names("link")
      result_tmp[[3]] <- tmp[[2]] %>% html_children() %>% .[[1]] %>% html_children() %>% {
        name_tmp <- sapply(.,function(u)html_attr(u,'class'))
        html_text(.) %>% set_names(name_tmp)
      }
      result_tmp[[4]] <- tmp[[2]] %>% html_children() %>% .[[2]] %>% html_children() %>% {
        name_tmp <- sapply(.,function(u)html_attr(u,'class'))
        html_text(.) %>% set_names(name_tmp)
      }
      result_tmp[[5]] <- tmp[[3]] %>% html_children() %>% .[[1]] %>% html_children() %>% {
        name_tmp <- sapply(.,function(u)html_attr(u,'class'))
        html_text(.) %>% set_names(name_tmp)
      }
      unlist(result_tmp)
    })
    
    dta_collection <- lapply(dta_collection,function(u){
      gsub("\\n | $","",u) %>% gsub(" {2,}"," ",.) %>% as.list() %>% as.data.frame()
    })
    
    result[[i]] <- do.call("rbind.fill", dta_collection)
  }
  result_bind <- ldply(result, rbind)
  result_bind <- result_bind[!duplicated(result_bind$link),]
  result_bind$srp.pdtitle %<>% tolower()
  
  bid.index <- result_bind$srp.pdprice.yui3.g %>% grep("次出價", .)
  result_bind$bid.times <- 0
  result_bind$bid.times[bid.index] <- result_bind$srp.pdprice.yui3.g[bid.index] %>% {sapply(strsplit(., "/"), "[[", 2)} %>% 
    gsub(" 次出價","", .) %>% as.integer()
  
  result_bind <- result_bind[result_bind$bid.times == 0, ]
  
  result_bind$srp.pdprice.yui3.g <- sapply(strsplit(result_bind$srp.pdprice.yui3.g, "/"), "[[", 1) %>% gsub("\\$|,","",.) %>% as.integer()
  if(is.null(filter.words.remove)){
    return(result_bind)
  }
  result_bind.filterRemove <- result_bind[-grep(tolower(filter.words.remove),result_bind$srp.pdtitle),]
  result_bind.filterKeep <- result_bind.filterRemove[grep(filter.words.keep,result_bind.filterRemove$srp.pdtitle),]
  result_bind.filterKeep
}

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y <- y[x >= (qnt[1] - H)]
  y <- y[y <= (qnt[2] + H)]
  y
}

