    library(shiny)
    library(shinydashboard)
    library(data.table)
    library(highcharter)
    library(shinyWidgets)
    library(dplyr)
    library(terra)
    library(leaflet)
    library(DT)

    ui <- dashboardPage(
    dashboardHeader(
        title = "Picture CY"
    ),
    dashboardSidebar(
        sidebarMenu(
        menuItem("Home", tabName = "home_tab", icon = icon("house-user")),
        menuItem("Education", tabName = "education_tab", icon = icon("graduation-cap")),
        menuItem("Environment", tabName = "env_tab", icon = icon("envira")),
        menuItem("Energy", tabName = "eng_tab", icon = icon("charging-station"))
        )
    ),
    dashboardBody(
        tabItems(
        tabItem(
            tabName = "home_tab",
            imageOutput("myImage"), 
            
            tags$html(
                
                tags$body(
                    h1('Welcome to Picture CY'),
                    p('Source of all data: ', strong('https://www.data.gov.cy/')),
                )
            )
        ),
        tabItem(
            tabName = "education_tab",

            box(
            width = 12,
            tags$style(
                ".first-p {
                font-family: verdana;
                font-size: 200%;
                }
                "
            ),

            title =   p(class = "first-p", "Teachers"),

            fluidRow(
                column(
                width = 7, offset = 1,
                box(
                    width = NULL,
                    highchartOutput("teachers_total")
                )
                ),
                column(
                width = 2, #offset = 2,
                box(
                    width = NULL,
                    uiOutput("teachers_year_ui")
                )
                )
            ),

            fluidRow(
                column(
                width = 4,
                box(
                    width = NULL,
                    highchartOutput("teachers_Primary")
                )
                ),
                column(
                width = 4,
                box(
                    width = NULL,
                    highchartOutput("teachers_Pre_Primary")
                )
                ),
                column(
                width = 4,
                box(
                    width = NULL,
                    highchartOutput("teachers_Special")
                )
                )
            ),
            fluidRow(
                column(
                width = 6,
                box(
                    width = NULL,
                    highchartOutput("teachers_Generar_Secondary")
                )
                ),
                column(
                width = 6,
                box(
                    width = NULL,
                    highchartOutput("teachers_Technical_Secondary")
                )
                )
            )
            ),
            fluidRow()
        ),

        tabItem(tabName = "env_tab",
            
            box(
            tags$style(
                ".first-p {
                font-family: verdana;
                font-size: 200%;
                }
                "
            ),

            title =   p(class = "first-p", "Temperature and Relative Humitity"),

            fluidRow(
                column(
                width = 5, 
                box(width = NULL,
                    highchartOutput("temp_clmrng")
                )
                ),
                column(
                width = 4,
                box(width = NULL,
                    highchartOutput("hum_clmrng")
                )
                ),
                column(
                width = 2,
                box(width = NULL,
                    uiOutput("temp_hum_loc_ui")
                )
                ),
                column(
                width = 1,
                box(width = NULL,
                    uiOutput("temp_hum_year_ui")
                )
                )
            )
            #, background = 'light-blue'
            , width = 12
            ),

            box(
            tags$style(
                ".first-p {
                font-family: verdana;
                font-size: 200%;
                }
                "
            ),

            title =   p(class = "first-p", "Air Pollution"), 
            fluidRow(

                box(
                width = 3, offset = 2,
                uiOutput("pollutant_station_ui")
                ),
                box(
                width = 3, #offset = 2,
                uiOutput("pollutant_name_ui")
                ),
                box(
                width = 3, #offset = 2,
                uiOutput("pollutant_date_ui")
                )
            ),
            fluidRow(
                box(width = 12,
                highchartOutput("pollution_plot")
                )
            )

            #, background = 'light-blue'
            , width = 12
            ),
            #br(),
            # br(),
            # br(),
            # br(),
            # br(),
            # br(),
            # br(),
            # br(),
            box(
            tags$style(
                ".first-p {
                font-family: verdana;
                font-size: 200%;
                }
                "
            ),

            title =   p(class = "first-p", "Fires"),

            fluidRow(
                column(
                width = 8, 
                box(width = NULL,
                    leafletOutput("map", height = 500)
                )
                ),
                column(
                width = 4,
                box(width = NULL,
                    uiOutput("fires_from_to")
                )
                )
            )
            #, background = 'light-blue'
            , width = 12
            )

            ,fluidRow()
            
        ),

        tabItem(
            tabName = "eng_tab",
            box(
            width = 12,
            tags$style(
                ".first-p {
                font-family: verdana;
                font-size: 200%;
                }
                "
            ),

            title =   p(class = "first-p", "Vehicle Charging Stations"),

            fluidRow(
                column(
                width = 10, offset = 1,
                box(
                    width = NULL,
                    DTOutput("e_charging_dt")
                )
                )
            ),

            
            ),

            box(
            width = 12,
            tags$style(
                ".first-p {
                font-family: verdana;
                font-size: 200%;
                }
                "
            ),

            title =   p(class = "first-p", "Register of Specialized Experts of Energy Performance Certificates of Buildings"),

            fluidRow(
                column(
                width = 10, offset = 1,
                box(
                    width = NULL,
                    DTOutput("energy_efficiency_dt")
                )
                )
            ),

            
            ),
            fluidRow()
        )

        )
    )
    )

    server <- function(input, output, session) { 

    output$myImage <- renderImage({
        list(
        src = paste0("./www/home_page.gif"),
        width = 1700,
        height = 930
        )
    }, deleteFile = FALSE)

    air_polution_hourly <- readRDS(paste0("./Environment/AirPolution/date_hourly.rds"))
    fires <- readRDS(paste0("./Environment/Fires/fires.rds"))
    temp_hum <- readRDS(paste0("./Environment/temp/temp_hum.rds"))
    teachers <- readRDS(paste0("./Education/teachers.rds"))
    energy_efficiency <- readRDS(paste0("./Energy/Register of Specialized Experts.rds"))
    e_charging <- readRDS(paste0("./Energy/e-Charging Locations.rds"))

    pollutant_name <- reactive({
        unique(air_polution_hourly[,pollutant_name_en])
    })
    pollutant_code <- reactive({
        req(input$pollutant_name_select)
        unique(air_polution_hourly[pollutant_name_en == input$pollutant_name_select,pollutant_code])[1]
    })
    station_name <- reactive({
        unique(air_polution_hourly[,station_name_en])
    })
    pollutant_from <- reactive( air_polution_hourly[,min(date)] )
    pollutant_to <- reactive( air_polution_hourly[,max(date)] )

    output$pollutant_date_ui <- renderUI({
        dateRangeInput( 
        inputId = "pollutant_from_to", label = "Select Date Range",
        start = pollutant_from(), end = pollutant_to(),
        min = pollutant_from(), max = pollutant_to()
        )
    })
    output$pollutant_name_ui <- renderUI({
        pickerInput( 
        inputId = "pollutant_name_select", label = "Select Pollutant",
        choices = pollutant_name(), selected  = pollutant_name()[1], FALSE
        )
    })
    output$pollutant_station_ui <- renderUI({
        pickerInput( 
        inputId = "pollutant_station_select", label = "Select Station",
        choices = station_name(), selected  = station_name()[1], TRUE
        )
    })

    pollutant_dt <- reactive({
        req(input$pollutant_from_to)
        req(input$pollutant_name_select)
        req(input$pollutant_station_select)

        air_polution_hourly[station_name_en %in% input$pollutant_station_select &
        pollutant_name_en %in% input$pollutant_name_select &
        date >= input$pollutant_from_to[1] & date <= input$pollutant_from_to[2]
        ]

    })

    output$pollution_plot <- renderHighchart({
        hchart(pollutant_dt(), "line", hcaes(x = date, y = pollutant_value, group = station_name_en)) %>%
        hc_yAxis(title = list(text = input$pollutant_name_select)) %>%
        hc_title(text = pollutant_code()) 
    })

    fires_from <- reactive( fires[,min(Date)] )
    fires_to <- reactive( fires[,max(Date)] )

    output$fires_from_to <- renderUI({
        dateRangeInput( 
        inputId = "fires_from_to", label = "Select Date Range",
        start = fires_from(), end = fires_to(),
        min = fires_from(), max = fires_to()
        )
    })

    fires_dt <- reactive({
        
        req(input$fires_from_to)
        dt <- fires[Date >= input$fires_from_to[1] & Date <= input$fires_from_to[2],.(
        Date, Latitude, Longitude, `Burned Area (ha)`
        )]

        x <- dt[,Latitude]
        y <- dt[,Longitude]
        points <- cbind(x, y)
        v <- vect(points, crs="+proj=utm +zone=36N +datum=WGS84  +units=m")
        y <- project(v, "+proj=longlat +datum=WGS84")
        lonlat <- geom(y)[, c("x", "y")]

        dt[,lat := lonlat [,2]]
        dt[,lon := lonlat [,1]]
        dt

    })

    output$map <- renderLeaflet({
        req(input$fires_from_to)
        t <- fires_dt()
        m <- leaflet(t) %>%
        addTiles() %>% 
        addCircles(lng = t$lon, lat = t$lat,
                    radius = ~`Burned Area (ha)`, weight = 2,
                    popup = paste(t$Date, "<br>",
                                "Burned Area:", t$`Burned Area (ha)`, " ha","<br>"
                                ),
                    color = "#FF5733") %>% 
        setView(33.312168, 35.054224, zoom = 8)
        m
    })


    temp_hum_year <- unique(temp_hum[,year])
    temp_hum_year <- temp_hum_year[order(-temp_hum_year)]
    temp_hum_loc <- unique(temp_hum[,LOCATION])

    output$temp_hum_year_ui <- renderUI({
        pickerInput(
        inputId = "temp_hum_year_selected",
        label = "Choose a Year",
        choices = temp_hum_year,
        selected = temp_hum_year[1],
        multiple = FALSE
        )
    })
    output$temp_hum_loc_ui <- renderUI({
        pickerInput(
        inputId = "temp_hum_loc_selected",
        label = "Choose a Location",
        choices = temp_hum_loc,
        selected = temp_hum_loc[1],
        multiple = FALSE
        )
    })

    temp_hum_dt <- reactive({
        req(input$temp_hum_year_selected)
        req(input$temp_hum_loc_selected)

        temp_hum[
        year == input$temp_hum_year_selected &
        LOCATION == input$temp_hum_loc_selected
        ]

    })

    output$temp_clmrng <- renderHighchart({
        req(input$temp_hum_year_selected)
        req(input$temp_hum_loc_selected)

        x <- c("08:00", "Mean", "13:00")
        y <- sprintf('{point.%s}°C',c('temp8','temp_mean',"temp13"))
        tltip <- tooltip_table(x, y)

        hchart(
        temp_hum_dt(),
        type = "columnrange",
        hcaes(
            x = Date, 
            low = temp8,
            high = temp13,
            color = temp_mean
        )
        ) %>%
        hc_chart(
            polar = TRUE
        ) %>%
        hc_yAxis(
            max = 35,
            min = 5,
            labels = list(format = "{value} C"),
            showFirstLabel = FALSE
        ) %>%
        hc_xAxis(
            title = list(text = ""), 
            gridLineWidth = 2,
            labels = list(format = "{value: %b}")
        )%>%
        hc_tooltip(
            useHTML = T,
            pointFormat = tltip
        )%>% 
        hc_title(
            text = paste0("Temperature of Cyprus - ", input$temp_hum_loc_selected, ", ", input$temp_hum_year_selected)
        )
    })

    output$hum_clmrng <- renderHighchart({
        req(input$temp_hum_year_selected)
        req(input$temp_hum_loc_selected)
        
        x <- c("08:00", "Mean", "13:00")
        y <- sprintf('{point.%s}',c('hum8','hum_mean',"hum13"))
        tltip <- tooltip_table(x, y)

        hchart(
        temp_hum_dt(),
        type = "columnrange",
        hcaes(
            x = Date, 
            low = hum8,
            high = hum13,
            color = hum_mean
        )
        ) %>%
        hc_chart(
            polar = TRUE
        ) %>%
        hc_yAxis(
            max = 100,
            min = 0,
            labels = list(format = "{value %}"),
            showFirstLabel = FALSE
        ) %>%
        hc_xAxis(
            title = list(text = ""), 
            gridLineWidth = 2,
            labels = list(format = "{value: %b}")
        )%>%
        hc_tooltip(
            useHTML = T,
            pointFormat = tltip
        )%>% 
        hc_title(
            text = paste0("Relative Humitity of Cyprus - ", input$temp_hum_loc_selected, ", ", input$temp_hum_year_selected)
        )
    })
    

    teachers_years <- unique(teachers[,Year])
    teachers_years <- teachers_years[order(teachers_years, decreasing = T)]

    output$teachers_year_ui <- renderUI({
        pickerInput(
        inputId = "teachers_year_selected",
        label = "Choose a School Year",
        choices = teachers_years,
        selected = teachers_years[1],
        multiple = FALSE
        )
    })

    teachers_dt <- reactive({
        req(input$teachers_year_selected)
        teachers[Year %in% input$teachers_year_selected]
    })
    output$teachers_total <- renderHighchart({
        highchart()%>%
        hc_title(text = "Total") %>%
        hc_chart(
            type = "treemap",
            polar = F,
            inverted = F
        ) %>%
        hc_xAxis(
            categories = teachers_dt()$Position
        ) %>%
        hc_yAxis(
            visible = T
        ) %>%
        hc_tooltip(
            outside = T
        ) %>%
        hc_add_series(
            teachers_dt()[,.(name, color, value = Total)],
            showInLegend = F
        )
    })

    output$teachers_Primary <- renderHighchart({
        highchart()%>%
        hc_title(text = "Primary") %>%
        hc_chart(
            type = "column",
            polar = F,
            inverted = F
        ) %>%
        hc_xAxis(
            categories = teachers_dt()$Position
        ) %>%
        hc_yAxis(
            visible = T
        ) %>%
        hc_tooltip(
            outside = T
        ) %>%
        hc_add_series(
            teachers_dt()[,.(name, color, y = Primary)],
            showInLegend = F
        )
    })
    
    output$teachers_Pre_Primary <- renderHighchart({
        highchart()%>%
        hc_title(text = "Pre Primary") %>%
        hc_chart(
            type = "pyramid",
            polar = F,
            inverted = F
        ) %>%
        hc_xAxis(
            categories = teachers_dt()$Position
        ) %>%
        hc_yAxis(
            visible = T
        ) %>%
        hc_tooltip(
            outside = T
        ) %>%
        hc_add_series(
            teachers_dt()[,.(name, color, y = `Pre-Primary`)],
            showInLegend = F
        )
    })  

    output$teachers_Special <- renderHighchart({
        highchart()%>%
        hc_title(text = "Special") %>%
        hc_chart(
            type = "pie",
            polar = F,
            inverted = F
        ) %>%
        hc_xAxis(
            categories = teachers_dt()$Position
        ) %>%
        hc_yAxis(
            visible = T
        ) %>%
        hc_tooltip(
            outside = T
        ) %>%
        hc_add_series(
            teachers_dt()[,.(name, color, y = Special)],
            showInLegend = F
        )
    })  

    output$teachers_Generar_Secondary <- renderHighchart({
        highchart()%>%
        hc_title(text = "General Secondary") %>%
        hc_chart(
            type = "item",
            polar = F,
            inverted = F
        ) %>%
        hc_xAxis(
            categories = teachers_dt()$Position
        ) %>%
        hc_yAxis(
            visible = T
        ) %>%
        hc_tooltip(
            outside = T
        ) %>%
        hc_add_series(
            teachers_dt()[,.(name, color, y = `General Secondary`)],
            showInLegend = F
        )
    })  

    output$teachers_Technical_Secondary <- renderHighchart({
        highchart()%>%
        hc_title(text = "Technical Secondary") %>%
        hc_chart(
            type = "bar",
            polar = F,
            inverted = F
        ) %>%
        hc_xAxis(
            categories = teachers_dt()$Position
        ) %>%
        hc_yAxis(
            visible = T
        ) %>%
        hc_tooltip(
            outside = T
        ) %>%
        hc_add_series(
            teachers_dt()[,.(name, color, y = `Technical Secondary`)],
            showInLegend = F
        )
    })  


    output$energy_efficiency_dt <- renderDT({
        datatable(energy_efficiency, filter = 'top', options = list(
        pageLength = 20, autoWidth = TRUE
        ))
    })

    output$e_charging_dt <- renderDT({
        datatable(e_charging, filter = 'top', options = list(
        pageLength = 5, autoWidth = TRUE
        ))
    })


    }


    js <- "
    Shiny.addCustomMessageHandler('anim',
    function(x){

        var $box = $('#' + x.id + ' div.small-box');
        var value = x.value;

        var $icon = $box.find('i.fa');
        if(value <= 10 && $icon.hasClass('fa-arrow-up')){
        $icon.removeClass('fa-arrow-up').addClass('fa-arrow-down');
        }
        if(value > 10 && $icon.hasClass('fa-arrow-down')){
        $icon.removeClass('fa-arrow-down').addClass('fa-arrow-up');
        }

        var $s = $box.find('div.inner h3');
        var o = {value: 0};
        $.Animation( o, {
            value: value
        }, {
            duration: 1500
        }).progress(function(e) {
            $s.text((e.tweens[0].now).toFixed(1));
        });

    }
    );"

    shinyApp(ui, server)