require(plyr)
require(rvest)
require(magrittr)
require(plotly)
require(jiebaR)
require(wordcloud)
require(shiny)
scrape_yahoo <- function(main.keywords="iphone6",
                         filter.words.remove=c("6s","plus","32g","64g","128g"),
                         filter.words.keep="16g",
                         page.limit){
  # filter.words.remove : 文字向量
  # main.keywords, filter.words.keep : 文字

  filter.words.remove <- paste(filter.words.remove, collapse="|")
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
    if(!is.null(page.limit)){ if(i>page.limit){break}}
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
  rawdata <- result_bind.filterRemove[grep(filter.words.keep,result_bind.filterRemove$srp.pdtitle),]

  # 變數選擇＆型別轉換
  rawdata$Date <- as.Date(rawdata$srp.pdtime)
  rawdata$Rating <- strtoi(substr(rawdata$srp.rating, 4, nchar(rawdata$srp.rating)))
  rawdata$Place <- substr(rawdata$srp.place, 3, nchar(rawdata$srp.place))
  rawdata$Seller <- substr(rawdata$srp.seller.yui3.g, 5, nchar(rawdata$srp.seller.yui3.g))
  rawdata <- rawdata[c('Date', 'Place','Seller', 'Rating','srp.pdtitle', 'srp.pdprice.yui3.g')]
  class(rawdata) <- c("ybid", "data.frame")
  rawdata
}


get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

#文字雲
wordcloud.bid <- function(df){
  cc <- worker()
  s <- cc[df$srp.pdtitle]
  s <- gsub("[0-9]+?","",s)
  tableWord<-count(s)
  Sys.info()
  if(get_os()=="osx"){
    par(family=("Heiti TC Light"))
  }
  wordcloud(tableWord[,1],tableWord[,2], scale=c(8,0.8),
            random.order=F)
}


plot.ybid <- function(df, plot.type = 1, rm.outlier=T){
  # rm.outlier=T:以 coef = 1.5去除離群值
  # plot.type=1:依時間變化之平均價格趨勢(box chart)
  # plot.type=2:依時間變化之平均價格趨勢(line chart)
  # plot.type=3:不同縣市之平均價格(bar chart)
  # plot.type=4:標題斷詞後之文字雲
  if(rm.outlier){
    boxplot.stats(df$srp.pdprice.yui3.g, coef = 1.5)
    df <- subset(df, df$srp.pdprice.yui3.g <= 15800 & df$srp.pdprice.yui3.g >=10500)
  }

    switch (plot.type,
            plot_ly(df, x = df$Date, y = srp.pdprice.yui3.g, type = 'box')%>% layout(yaxis = list(title='價格'), xaxis = list(title='時間')) # 依時間變化之價格趨勢(盒型圖)
            ,
            {fit <- lm(df$srp.pdprice.yui3.g~df$Date)
            aggregate(srp.pdprice.yui3.g~Date, df, median) %>% plot_ly(x = Date, y = srp.pdprice.yui3.g) %>% add_trace(x = Date, y= fitted(fit), mode="lines") %>% layout(yaxis = list(title='價格'), xaxis = list(title='時間'))} # 依時間變化之價格趨勢(線圖)
            ,
            aggregate(srp.pdprice.yui3.g~Place, df, mean) %>% plot_ly(x = Place, y = srp.pdprice.yui3.g, type='bar') %>% layout(yaxis = list(title='價格'), xaxis = list(title='地點')) # 不同縣市之平均價格
            ,
            wordcloud.bid(df)
    )
  }


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

app <- shinyApp(
  ui = navbarPage(

    title = '價格趨勢圖',
    tabPanel('關鍵字設定',
             h1('Search'),
             fluidPage(
               fluidRow(
                 column(3,
                        textInput("productKeyword",
                                  label = h3("請輸入您要搜尋的商品關鍵字"),
                                  value = "iPhone6")
                 ),

                 column(3,
                        textInput("includeKeyword",
                                  label = h3("請輸入一定要出現的關鍵字"),
                                  value = "16g")
                 ) ,
                 column(3,
                        textInput("removeKeyword",
                                  label = h3("請輸入您要去除的的關鍵字"),
                                  value = "通訊行|全新")
                 )

               ),
               fluidRow(

                 column(3,
                        textInput("lowerPrice",
                                  label = h3("價格區間"),
                                  value = "10000"),
                        textInput("upperPrice",
                                  label = h3("價格區間"),
                                  value = "30000")


                 )
               ),
               fluidRow(
                 column(3,
                        # actionButton("action", label = "Action"),
                        submitButton("搜尋"))
               )
             ), #fluidPage End
             mainPanel(
               dataTableOutput("mytable"),
               textOutput("text1")  #顯示key的關鍵字

             )
    ),#Tab Panel End
    navbarMenu("資料視覺化",
               tabPanel("價格",
                        mainPanel(
                          plotlyOutput('markPlot')
                        )
               ),
               tabPanel("地點",
                        h1('地點'),
                        mainPanel(
                          plotOutput('locationPlot')
                        ))
    )
  ),
  server = function(input, output) {
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
  }
)

# runApp(appDir = paste0(path.package("YahooScrape"),'/R'))

