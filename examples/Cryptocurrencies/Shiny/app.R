library(shiny)
library(shinymaterial)
library(curl)
library(jsonlite)
library(data.table)
library(highcharter)
library(lubridate)

ui <- material_page(
  
  title = "Cryptocurrencies",
  nav_bar_fixed = T,
  include_fonts = T,
  nav_bar_color = "teal lighten-1",
  
  material_side_nav(

    material_row(
      material_column(
        width = 4,
        uiOutput("coinsUI")
      ),
      material_column(
        width = 8,
        material_radio_button(
          input_id = "coins_order", label = "Order by",
          choices = c("24h-Volume",
                      "Alphabetic"),
          selected = "24h-Volume"
        )
      )
    )
  ),
  
  material_row(
    material_column(
      width = 2, #offset = 2,
      htmlOutput("coin_logo")
    ),
    material_column(
      width = 2, #offset = 2,
      uiOutput("pct_chg_24h")
    ),
    material_column(
      width = 2, #offset = 2,
      uiOutput("pct_chg_7d")
    ),
    material_column(
      width = 2, #offset = 2,
      uiOutput("pct_chg_1m")
    ),
    material_column(
      width = 2, #offset = 2,
      uiOutput("pct_chg_6m")
    ),
    material_column(
      width = 2, #offset = 2,
      uiOutput("pct_chg_1y")
    )
    
  ),
  material_row(
      material_column(
          width = 10, offset = 2,
          material_card(
                tags$h5('Time Frame'),
                material_radio_button(
                    "chart_frame", "",
                    choices = c("24H", "7d", "1m", "6m", "1Y", "ALL"),
                    selected = "24H", with_gap = TRUE
                ),
                tagList(
                    tags$head(
                        tags$style(
                            "#chart_frame {display:flex;}
                            input[type=radio] {margin-right: 42px;}"
                        )
                    )
                ),

                tags$h5('Scale'),
                material_switch("log_scale", 
                        on_label = "Log",
                        initial_value = FALSE,
                        color = "teal lighten-1"),

              highchartOutput("crypto_graph"),
              depth = 3
          )
      )
  )
  
  
)

server <- function(input, output, session){
  
    apikey <- fread(paste0(examples_path, "/Cryptocurrencies/Shiny/api_key.txt"))
    apikey <- apikey[,key]
    
    h <- new_handle()
    handle_setheaders(h, "Accepts" = "application/json",
                        "X-CMC_PRO_API_KEY" = apikey)
    
    coins_dim <- curl_fetch_memory("https://min-api.cryptocompare.com/data/top/totaltoptiervolfull?limit=100&tsym=USD", handle = h)
    coins_dim <- fromJSON(rawToChar(coins_dim$content), flatten = T)
  #all_coins <- coins_dim$Data$CoinInfo.Internal
  
  images <- reactive({
    req(input$coins_order)
    imgs <- data.table(
        img = coins_dim$Data$CoinInfo.ImageUrl,
        coin = coins_dim$Data$CoinInfo.Internal
    )
    if (input$coins_order == "Alphabetic"){
      imgs <- imgs[order(coin)]
    }
    return(imgs)
  })
  
  all_coins <- reactive({
    req(input$coins_order)
    t <- coins_dim$Data$CoinInfo.Internal
    if (input$coins_order == "Alphabetic"){
      t <- t[order(t)]
    }
    t
  })
  
  output$coin_logo <- renderText({
      req(input$selected_coin)
      c <- input$selected_coin
      t <- images()[coin == c, img]
      src <- paste0("https://www.cryptocompare.com", t)
      c('<img src="',src,'">')
  }) #, deleteFile = F
  
  output$coinsUI <- renderUI({
  
    t <- material_radio_button("selected_coin", "Select Coin",
                       choices = all_coins(),
                       selected = all_coins()[1]
    )

    t
  })
  
  
  coin_data <- reactive({
    
    req(input$selected_coin)
    coin <- input$selected_coin
    
    h <- new_handle()
    handle_setheaders(h, "Accepts" = "application/json",
                      "X-CMC_PRO_API_KEY" = apikey)
    
    daily <- curl_fetch_memory(
      paste0("https://min-api.cryptocompare.com/data/v2/histoday?fsym=",coin,"&tsym=USD&limit=2000")
      , handle = h)
    daily <- data.table(fromJSON(rawToChar(daily$content), flatten = T)$Data$Data)
    
    h <- new_handle()
    handle_setheaders(h, "Accepts" = "application/json",
                      "X-CMC_PRO_API_KEY" = apikey)
    hourly <- curl_fetch_memory(
      paste0("https://min-api.cryptocompare.com/data/v2/histohour?fsym=",coin,"&tsym=USD&limit=2000")
      , handle = h)
    hourly <- data.table(fromJSON(rawToChar(hourly$content), flatten = T)$Data$Data)
    
    
    h <- new_handle()
    handle_setheaders(h, "Accepts" = "application/json",
                      "X-CMC_PRO_API_KEY" = apikey)
    minute <- curl_fetch_memory(
      paste0("https://min-api.cryptocompare.com/data/v2/histominute?fsym=",coin,"&tsym=USD&limit=2000")
      , handle = h)
    minute <- data.table(fromJSON(rawToChar(minute$content), flatten = T)$Data$Data)
    
    
    daily[,date := as_datetime(time)]
    hourly[,date := as_datetime(time)]
    minute[,date := as_datetime(time)]
    
    list(daily=daily,
         hourly=hourly,
         minute=minute)
  })
  
  

  output$crypto_graph <- renderHighchart({

    req(input$chart_frame)

    material_spinner_show(session, "crypto_graph")
    Sys.sleep(time = 2)
    material_spinner_hide(session, "crypto_graph")
    if (input$chart_frame == "24H"){
        dt <- coin_data()$minute[-c(1:560)]
    } else if (input$chart_frame == "7d"){
        dt <- coin_data()$hourly[-c(1:1832)]
    } else if (input$chart_frame == "1m"){
        dt <- coin_data()$hourly
        dt <- dt[date >= dt$date[2001] + months(-1)]
    } else if (input$chart_frame == "6m"){
        dt <- coin_data()$daily
        dt <- dt[date >= dt$date[2001] + months(-6)]
    } else if (input$chart_frame == "1Y"){
        dt <- coin_data()$daily
        dt <- dt[date >= dt$date[2001] + months(-12)]
    } else {
        dt <- coin_data()$daily
    }
    
    

    if (input$log_scale) {
        t <- hchart(dt, "line", hcaes(x = highcharter::datetime_to_timestamp(date), y = log(close)))
    } else {
        t <- hchart(dt, "line", hcaes(x = highcharter::datetime_to_timestamp(date), y = close))
    }

    

    t <- t %>%
        hc_xAxis(type = 'datetime',
            title = list(text = "Datetime")) %>%
        hc_yAxis(title = list(text = "Price"))

  })
  
  
  output$pct_chg_24h <- renderUI({
    
    t <- coin_data()$minute[-c(1:560)]
    s <- t[.N,close]/t[1,close] - 1
    c <- round((s)*100, 2)
    sign <- ifelse(s >= 0, "+", "")
    color_s <- ifelse(s >= 0, "#03CE0C", "#CE0303")
    
    material_column(
      width = 12,
      material_card(
        title = HTML(
          paste0(
            "<span style='font-weight:bold; color:", "#000000", "'>", "24h%", "</span>&nbsp;&nbsp;"
          )
      ), 
      depth = 3,
      HTML(
        paste0(
          "<div class='text-right'><span style='font-size:28px; color:",color_s,"'>",
          sign, c,
        "</span></div>"
      )
      )
    ))
  })
  
  
  output$pct_chg_7d <- renderUI({
    
    t <- coin_data()$hourly[-c(1:1832)]
    s <- t[.N,close]/t[1,close] - 1
    c <- round((s)*100, 2)
    sign <- ifelse(s >= 0, "+", "")
    color_s <- ifelse(s >= 0, "#03CE0C", "#CE0303")
    
    material_column(
      width = 12,
      material_card(
        title = HTML(
          paste0(
            "<span style='font-weight:bold; color:", "#000000", "'>", "7d%", "</span>&nbsp;&nbsp;"
          )
        ), 
        depth = 3,
        HTML(
          paste0(
            "<div class='text-right'><span style='font-size:28px; color:",color_s,"'>",
            sign, c,
            "</span></div>"
          )
        )
      ))
  })
  
  
  output$pct_chg_1m <- renderUI({
    
    t <- coin_data()$hourly
    t <- t[date >= t$date[2001] + months(-1)]
    s <- t[.N,close]/t[1,close] - 1
    c <- round((s)*100, 2)
    sign <- ifelse(s >= 0, "+", "")
    color_s <- ifelse(s >= 0, "#03CE0C", "#CE0303")
    
    material_column(
      width = 12,
      material_card(
        title = HTML(
          paste0(
            "<span style='font-weight:bold; color:", "#000000", "'>", "1m%", "</span>&nbsp;&nbsp;"
          )
        ), 
        depth = 3,
        HTML(
          paste0(
            "<div class='text-right'><span style='font-size:28px; color:",color_s,"'>",
            sign, c,
            "</span></div>"
          )
        )
      ))
  })

  output$pct_chg_6m <- renderUI({
    
    t <- coin_data()$daily
    t <- t[date >= t$date[2001] + months(-6)]
    s <- t[.N,close]/t[1,close] - 1
    c <- round((s)*100, 2)
    sign <- ifelse(s >= 0, "+", "")
    color_s <- ifelse(s >= 0, "#03CE0C", "#CE0303")
    
    material_column(
      width = 12,
      material_card(
        title = HTML(
          paste0(
            "<span style='font-weight:bold; color:", "#000000", "'>", "6m%", "</span>&nbsp;&nbsp;"
          )
        ), 
        depth = 3,
        HTML(
          paste0(
            "<div class='text-right'><span style='font-size:28px; color:",color_s,"'>",
            sign, c,
            "</span></div>"
          )
        )
      ))
  })
  
  output$pct_chg_1y <- renderUI({
    
    t <- coin_data()$daily
    t <- t[date >= t$date[2001] + months(-12)]
    s <- t[.N,close]/t[1,close] - 1
    c <- round((s)*100, 2)
    sign <- ifelse(s >= 0, "+", "")
    color_s <- ifelse(s >= 0, "#03CE0C", "#CE0303")
    
    material_column(
      width = 12,
      material_card(
        title = HTML(
          paste0(
            "<span style='font-weight:bold; color:", "#000000", "'>", "1Y%", "</span>&nbsp;&nbsp;"
          )
        ), 
        depth = 3,
        HTML(
          paste0(
            "<div class='text-right'><span style='font-size:28px; color:",color_s,"'>",
            sign, c,
            "</span></div>"
          )
        )
      ))
  })

}

shinyApp(ui,server)
