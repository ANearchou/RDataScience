
library(shiny)
library(data.table)
library(shinydashboard)
library(shinyWidgets)
library(plotly)

ui <- dashboardPage(
  
  dashboardHeader(title = "BRAZIL RETAIL"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Main", tabName = "main", icon = icon("home"))
    ),
    sidebarMenu(
      menuItem("Category", tabName = "category", icon = icon("cubes"))
    ),
    sidebarMenu(
      menuItem("Product", tabName = "product", icon = icon("sitemap"))
    ),
    sidebarMenu(
      menuItem("Seller", tabName = "seller", icon = icon("shipping-fast"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML( '
                              input {
                              width: 100px !important;
                              }
                               ' ))),
    tabItems(
      tabItem(tabName = "main"
                ,imageOutput("myImage")
              ),
      tabItem(tabName = "category"

              , fluidRow(
                column(8,
                  plotlyOutput("cat_sales_pl")
                ),
                column(4,
                  uiOutput("all_categories_ui")
                ))

              , fluidRow(
                column(8,
                plotlyOutput("cat_sharing")),
                column(4,
                  uiOutput("all_categories_periods_ui")
                ))
              
      ),
      tabItem(tabName = "product"
      ),
      tabItem(tabName = "seller" 
      )
     
    )
  )
)


server <- function(input, output, session) {
  options(shiny.maxRequestSize=1000*1024^2)
  
  outpath <- "C:/Users/user/Desktop/Andreas/R/RDataScience/kaggle/Brazil_ECommerce/Output"

  output$myImage <- renderImage({
    list(
      src = "C:/Users/user/Desktop/Andreas/R/RDataScience/kaggle/Brazil_ECommerce/ShinyApp/www/shutterstock_1772268194.jpg",
      width = 1000,
      height = 600
    )
  }, deleteFile = FALSE)


  cat_lvl <- fread(paste0(outpath, "/category_lvl.csv"))
  cat_lvl[,period := as.character(period)]
  all_categories <- unique(cat_lvl[,.(Category)])[order(Category)]
  all_categories_periods <- unique(cat_lvl[,.(period = as.character(period))])[order(period)]


  output$all_categories_ui <- renderUI({
    
    pickerInput("all_categories_ui_id", label = "Categories",
                 choices = all_categories[,Category], 
                 options = list(
                          `actions-box` = TRUE, 
                          size = 10,
                          `selected-text-format` = "count > 3"
                        )
                 , multiple = TRUE)
  })

  output$all_categories_periods_ui <- renderUI({
    
    pickerInput("all_categories_periods_id", label = "Periods",
                 choices = all_categories_periods[,period], 
                 options = list(
                          `actions-box` = TRUE, 
                          size = 10,
                          `selected-text-format` = "count > 3"
                        )
                 , multiple = TRUE)

  })


  ####################################################################################################################
  selected_categories <- reactive(input$all_categories_ui_id)
  selected_categories_periods <- reactive(input$all_categories_periods_id)
  categories_dt <- reactive(cat_lvl[period %in% selected_categories_periods() & Category %in% selected_categories()])
  ####################################################################################################################


  cat_shares <- reactive({

    t <- categories_dt()[,.(sales = sum(sales)), by = .(Category)]
    t[,total := sum(sales)]
    t[,share := sales/total]
    t

  })
  
  output$cat_sales_pl <- renderPlotly({
    
    req(input$all_categories_ui_id)
    req(input$all_categories_periods_id)

    p <- plot_ly()
    for(i in 1:length(selected_categories())){
      p <- add_trace(p, 
                    x = as.Date(categories_dt()[Category %in% selected_categories()[i] & period %in% selected_categories_periods(), period]),
                    y = categories_dt()[Category %in% selected_categories()[i], sales],
                    type='scatter', mode = "lines", name = selected_categories()[i] )
    }
    p

  })

  output$cat_sharing <- renderPlotly({
    
    req(input$all_categories_ui_id)
    req(input$all_categories_periods_id)

    p <- layout(
      plot_ly(cat_shares(), labels = ~Category, values = ~share, type = 'pie'),
      title = 'Category Shares', 
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis  = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
    )
    p

  })

}

shinyApp(ui, server)
