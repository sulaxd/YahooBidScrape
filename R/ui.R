library(plotly)
shinyUI(
  navbarPage(

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
                         print("~"),
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
  )
)


