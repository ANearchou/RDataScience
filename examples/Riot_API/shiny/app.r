library(shiny)
library(shinydashboard)
library(data.table)
library(highcharted)
library(typedjs)

ui <- dashboardPage(
    dashboardHeader(

    ),
    dashboardSidebar(

    ),
    dashboardBody(
        fluidRow(
            typed(
                "<h1 style='font-size:500%;text-align:center;'>Hello! Welcome to PictureCY!</h1>.",
                contentType = "html"
            )
        )#,
        # fluidRow(
        #     uiOutput("champ_select_ui")
        # ),
        # fluidRow(
        #     highchartOutput("base_stats_hc")
        # )
    )
)

server <- function(input, output, session){

    champions_base_stats <- readRDS("C:/Users/user/Desktop/Andreas/R/RDataScience/examples/Riot_API/data/champions_base_stats.rds")

    output$champ_select_ui <- renderUI({
        pickerInput(
            "champ_select_id",
            "Choose 5 champions",
            choices = unique(champions_base_stats[,champion]),
            selected = NULL, multiple = TRUE,
            options = pickerOptions(maxOptions = 5)

        )
    })

    champions_selected <- reactive({
        req(input$champ_select_id)
        champions_base_stats[champion %in% input$champ_select_id]
    })

    base_stats <- reactive({
        t <- champions_selected()[,.(value = sum(value_std)), by = .(statistic)]
        t[,color := c(brewer.pal(8, "Accent"), brewer.pal(12, "Paired"), brewer.pal(8, "Set3"))[1:nrow(t)]]
    })

    output$base_stats_hc <- renderHighchart({
        highchart() %>%
            hc_title(text = "Team combined Statistics") %>%
            hc_chart(
                type = "column"
            ) %>%
            hc_xAxis(
                categories = base_stats()[,statistic]
            ) %>%
            hc_yAxis(
                visible = T
            ) %>%
            hc_tooltip(
                outside = T
            ) %>%
            hc_add_series(
                base_stats()[,.(name = statistic, y = value, color)],
                showInLegend = T
            )
    })
    

}

shinyApp(ui = ui, server = server)