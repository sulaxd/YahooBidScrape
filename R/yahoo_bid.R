# main.keywords <- "iphone6"
# filter.words.remove <- c("6s","plus","32g","64g","128g")
# filter.words.keep <- "16g"

scrape_yahoo <- function(main.keywords, filter.words.remove, filter.words.keep){
  # filter.words.remove : 文字向量
  # main.keywords, filter.words.keep : 文字

  filter.words.remove <- paste(filter.words.remove, collapse="|")
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
