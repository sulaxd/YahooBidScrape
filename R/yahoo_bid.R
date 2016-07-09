

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(c("plyr","rvest","magrittr","plotly","jiebaR","wordcloud",
       "shiny","mongolite"))

require(plyr)
require(rvest)
require(magrittr)
require(plotly)
require(jiebaR)
require(wordcloud)
require(shiny)
require(mongolite)
#### Search Function ####
ybid.scrape <- function(main.keywords="iphone6",
                        filter.words.remove=c("6s|plus|32g|64g|128g"),
                        filter.words.keep="16g",
                        page.limit=NULL){
  # filter.words.remove : 關鍵字以|間隔
  # main.keywords, filter.words.keep : 文字
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
    # paste0("正在擷取第",i,"頁，共",length(pageUrls),"頁 \n")
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
  if(get_os()=="osx"){
    par(family=("Heiti TC Light"))
  }
  wordcloud(tableWord[,1],tableWord[,2], scale=c(12,0.8),
            random.order=F,colors=brewer.pal(8, "Dark2"))
}

#### Plot Function ####
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

  median.value <- aggregate(srp.pdprice.yui3.g~Date, df, median)
  mean.value <- aggregate(srp.pdprice.yui3.g~Date, df, mean)
  min.value <- aggregate(srp.pdprice.yui3.g~Date, df, min)
  max.value <- aggregate(srp.pdprice.yui3.g~Date, df, max)



  switch (plot.type,
          plot_ly(df, x = df$Date, y = srp.pdprice.yui3.g, type = 'box')%>% layout(yaxis = list(title='價格'), xaxis = list(title='時間')) # 依時間變化之價格趨勢(盒型圖)
          ,
          {fit <- lm(df$srp.pdprice.yui3.g~df$Date)
          plot_ly(mean.value, x = Date, y = srp.pdprice.yui3.g, mode="lines",symbol = "平均值") %>%
            add_trace(x = Date, y= fitted(fit), mode="lines",symbol = "迴歸線") %>%
            add_trace(x = Date, y= median.value$srp.pdprice.yui3.g, mode="lines", symbol = "中位數") %>%
            add_trace(x = Date, y= min.value$srp.pdprice.yui3.g, mode="lines", symbol = "最小值", size=0.05) %>%
            add_trace(x = Date, y= max.value$srp.pdprice.yui3.g, mode="lines", symbol = "最大值", size=0.05) %>%
            layout(yaxis = list(title='價格'), xaxis = list(title='時間'))} # 依時間變化之價格趨勢(線圖)
          ,
          # aggregate(srp.pdprice.yui3.g~Place, df, mean) %>% plot_ly(x = Place, y = srp.pdprice.yui3.g, type='bar') %>% layout(yaxis = list(title='價格'), xaxis = list(title='地點')) # 不同縣市之平均價格
          {mean.byLocation <- aggregate(srp.pdprice.yui3.g~Place, df, mean)
          df.location <- cbind(mean.byLocation,table(df$Place))
            plot_ly(df.location, x = Place, y = srp.pdprice.yui3.g, size = Freq, mode = "markers") %>% layout(yaxis = list(title='價格'), xaxis = list(title='地點')) # 不同縣市之平均價格
            }
          ,
          wordcloud.bid(df)
  )
}


loadDataFromDB <- function(){
  m <- mongo(collection = "iphone6", url="mongodb://104.199.134.206")
  df <- m$find()
  return(df)
}

ybid.shiny <- function(){
  app <- shinyApp(
    ui = navbarPage(

      title = '價格趨勢圖',
      # tabPanel('關鍵字設定',
      #          h1('Search'),
      #          fluidPage(
      #            fluidRow(
                   # column(3,
                   #        textInput("productKeyword",
                   #                  label = "商品關鍵字",
                   #                  value = "iPhone6"),
                   #        textInput("includeKeyword",
                   #                  label = "必要關鍵字",
                   #                  value = "16g"),
                   #        textInput("removeKeyword",
                   #                  label = "篩除關鍵字",
                   #                  value = "通訊行|全新"),
                   #        # textInput("lowerPrice",
                   #        #           label = h3("價格區間"),
                   #        #           value = "10000"),
                   #        # textInput("upperPrice",
                   #        #           label = h3("價格區間"),
                   #        #           value = "30000"),
                   #        verbatimTextOutput("urlText"),
                   #        actionButton("search", "搜尋")
                   # ),
                   # column(10,
                   #        dataTableOutput("mytable"),
                   #        textOutput("text1")  #顯示key的關鍵字
                   # ))
               # ) #fluidPage End

      # ),#Tab Panel End
      # navbarMenu("資料視覺化",
      tabPanel("價格趨勢",
               h1('價格'),
               mainPanel(
                 plotlyOutput('linePlot')
               )
      ),
      tabPanel("價格變異",
               h1('價格盒鬚圖'),
               mainPanel(
                 plotlyOutput('boxPlot')
               )),
      tabPanel("標題探勘",
               h1('標題文字雲'),
               mainPanel(
                 plotOutput('wordPlot')
               )),
      tabPanel("地點散佈",
               h1('地點氣泡圖'),
               mainPanel(
                 plotlyOutput('locationPlot',width = "800px")
               ))
      # )
    ),
    server = function(input, output) {

      df <- loadDataFromDB()

      output$text1 <- renderText({
        paste("You have typed", input$productKeyword)
      })

      #### Search 暫不開放####
      observeEvent(input$search, {
        ybid.scrape()
      })


      output$locationPlot <- renderPlotly({
        print(plot.ybid(df,3))
      })

      output$boxPlot <- renderPlotly({
        print(plot.ybid(df,1))
      })

      output$wordPlot <- renderPlot({
        withProgress({
          setProgress(message = "Processing corpus...")
          print(plot.ybid(df,4))
        })
      })

      output$linePlot <- renderPlotly({
        print(plot.ybid(df,2))
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
  runApp(app)
}
