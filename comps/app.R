#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)
library(leaflet)
library(data.table)
library(lubridate)
library(digest)
library(ggplot2)
library(DT)
library(here)


get_geo_data_from_address <- function(free_address) {
    
    require(httr)
    
    # Pause between requests
    Sys.sleep(3)
    
    nominatim_url <- "https://nominatim.openstreetmap.org/search/"
    
    free_address <- list(q=free_address)
    
    additional_parameters <- list(
        format='json',
        addressdetails='1',
        extratags='1',
        limit='1',
        countrycodes='us',
        'accept-language'='en',
        email='timolin@gmail.com'
    )
    
    # Query result
    r <- GET(nominatim_url, query=c(free_address, additional_parameters))
    return(content(r))
    
}

get_coord <- function(address) {
    require(digest)
    r <- get_geo_data_from_address(address)
    result <- r[1][[1]]
    result_obj <- list(
        input_address=address,
        address_key=digest(address, algo='sha1'),
        display_name=ifelse(!is.null(result$display_name), result$display_name, ''),
        lat=ifelse(!is.na(result$lat), (as.numeric(result$lat)), NA),
        lon=ifelse(!is.na(result$lon), as.numeric(result$lon), NA)
        
    )
    
    ## Return as a data.table
    ## Need to handle empty data here
    return(as.data.table(result_obj))
    
}

enhance_dataset <- function(dt) {
    
    dt[, address_key := sapply(Site_Address, digest, algo='sha1')]
    
    # dt[, Sale_Date := as.POSIXct(Sale_Date)]
    dt[, Sale_Date_2 := ifelse(Category == 'Subject', today(), as.Date(Sale_Date))]
    dt[, Sale_Price_2 := ifelse(Category == 'Subject', NA, Sale_Price)]
    dt[, Pr_per_SqFt := ifelse(Category == 'Subject', NA, X._per_SqFt)]
    
    # Aggregates
    dt[, Avg_Pr_per_SqFt := mean(Pr_per_SqFt, na.rm=T)]
    dt[, Fitted_Price := Structure_SqFt * Avg_Pr_per_SqFt, by=Category]
    
    ## Check if coordinates already exist
    n <- names(dt)
    if (is.element('lon', n) & is.element('lat', n)) {
        ## Coordinates already exists and we can avoid having to fetch them
        return(dt)
        
    } else {

        dt_coord <- do.call(
            rbind, lapply(dt[, Site_Address], get_coord)
        )
        
        dt_enhanced <- dt_coord[dt, on='address_key']
        
        ## Temp: write out the file so I don't have to keep fetching coords
        write.csv(dt_enhanced, file=here::here('./tmp_comp.csv'))
        
        return(dt_enhanced)
        
    }

}


plot_lf <- function(dt) {
    ctr <- list(
        lon = dt[, mean(lon, na.rm=T)],
        lat = dt[, mean(lat, na.rm=T)]
    )
    
    m <- leaflet(dt) %>% 
        addTiles() %>% 
        setView(lng=ctr$lon, lat=ctr$lat, zoom=10) %>% 
        # addMarkers(data=dt[Category == 'Comp'], label=~input_address, labelOptions=labelOptions(noHide=F)) %>% 
        addMarkers(data=dt[Category == 'Subject'], label=~input_address, labelOptions=labelOptions(noHide=T))
}

# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("Property Comparisons"),
    tabsetPanel(
        tabPanel(
            'Import data',
            fluidRow(
                column(
                    8,
                    fileInput('input_file', 'Select the path to the data file in CSV format')
                )
            ),

            fluidRow(
                column(
                    12,
                    tags$div(
                        hr(),
                        h3('Raw data')
                    ),
                    withSpinner(DTOutput('raw_data'))
                )
            )
        ),
        
        tabPanel(
            'Comps maps',
            fluidRow(
                leafletOutput('leaflet_map'),
                br(),
                hr(),
                h4('Subject property'),
                DTOutput('display_subject_data'),
                br(),
                h4('Comps'),
                DTOutput('display_comps_data')
            )
        )
    )
)


server <- function(session, input, output) {
    
    input_raw <- reactive({
        input_data <- input$input_file
        if(is.null(input_data)) {return()}
        
        dt <- data.table(
            read.csv(input_data$datapath)
        )
        
        dt_enhanced <- enhance_dataset(dt)
        
    })
    
    input_display_subject <- reactive({
        dt <- input_raw()[Category == 'Subject']
    })
    
    input_display_comps <- reactive({
        dt <- input_raw()[Category == 'Comp']
    })
    
    observeEvent(input$display_comps_data_rows_selected, {
        selected_comps_rows <- eventReactive(
            input$display_comps_data_rows_selected, {
                input_display_comps()[
                    unique(c(input$display_comps_data_rows_selected))
                ]
        })
        
    
        comp_dt <- reactive({
            selected_comps_rows()
        })
        
        sel <- reactive({
            is.null(input$display_comps_data_rows_selected)
        })
        
        ## for debug
        # cat(file=stderr(), paste0(as.character(sel()), '\n'))
        ##
        
        ## This logic ensures the last row that gets deslected clears the map
        ## Must have ignoreNULL = F and ignoreInit = T.
        ## The first flag makes the reactive code respond to NULL
        ## The second flag delays the activation so you don't load an empty 'input_display_subject()'
        if (sel()) {
            leafletProxy('leaflet_map', session) %>%
                clearMarkers() %>%
                addMarkers(data=input_display_subject(), lng=~lon, lat=~lat, label=~input_address, labelOptions=labelOptions(noHide=T))

        } else {
            leafletProxy('leaflet_map', session) %>%
                clearMarkers() %>%
                addMarkers(data=input_display_subject(), lng=~lon, lat=~lat, label=~input_address, labelOptions=labelOptions(noHide=T)) %>%
                addMarkers(
                    data=comp_dt(), lng=~lon, lat=~lat,
                    label=~input_address,
                    labelOptions=labelOptions(noHide=T)
                )
        }
        
    }, ignoreNULL=F, ignoreInit=T)
    
    output$leaflet_map <- renderLeaflet({
        dt <- data.table(input_raw())
        plot_lf(dt)
        
    })
    
    output$raw_data <- renderDT({
        if(is.null(input_raw())) {return()}
        
        # col_spec <- c('Site_Address', 'Sale_Date', 'Sale_Price', 'Fitted_Price', 'Pr_per_SqFt', 'Bedrooms', 'Bathrooms', 'lat', 'lon')
        raw_dt <- input_raw()

    })
    
    output$display_subject_data <- renderDT({
        if(is.null(input_display_subject())) {return()}
        dt_out <- input_display_subject()
        
        dt_out[, .(
            APN, Site_Address, Property_Type, Sale_Price, Sale_Date,
            Bedrooms, Bathrooms, Structure_SqFt, Pr_per_SqFt
        )]

    })
    
    output$display_comps_data <- renderDT({
        if(is.null(input_display_comps())) {return()}
        
        ## input_display_comps() has the lat, lon info needed for mapping
        ## but not going to display it in the return value
        dt_out <- input_display_comps()
        

        dt_out[,.(
            APN, Site_Address, Property_Type, Sale_Price, Sale_Date,
            Bedrooms, Bathrooms, Structure_SqFt, Pr_per_SqFt
        )]

    })
    

        
}

# Run the application 
shinyApp(ui = ui, server = server)
