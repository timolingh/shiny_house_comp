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
    Sys.sleep(1)
    
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

get_distance_from_subject <- function(lon, lat, ref_lon, ref_lat) {
    require(geosphere)
    d_meters <- distHaversine(
        c(lon, lat), 
        c(ref_lon, ref_lat)
    )
    
    d_miles <- d_meters * 0.0006214
    
        
}

vec_get_distance_from_subject <- Vectorize(get_distance_from_subject)

enhance_dataset <- function(dt) {
    
    dt[, address_key := sapply(Site_Address, digest, algo='sha1')]
    
    # dt[, Sale_Date := as.POSIXct(Sale_Date)]
    # dt[, Sale_Date_2 := ifelse(Category == 'Subject', today(), as.Date(Sale_Date))]
    # dt[, Sale_Price_2 := ifelse(Category == 'Subject', NA, Sale_Price)]
    dt[, Pr_per_SqFt := ifelse(Category == 'Subject', NA, Sale_Price / Structure_SqFt)]
    
    # Aggregates
    # dt[, Avg_Pr_per_SqFt := mean(Pr_per_SqFt, na.rm=T)]
    # dt[, Fitted_Price := Structure_SqFt * Avg_Pr_per_SqFt, by=Category]
    
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
        # write.csv(dt_enhanced, file=here::here('./tmp_comp.csv'))
        
        ## Reference coord
        dt_enhanced[, ref_lon := numeric()]
        dt_enhanced[, ref_lat := numeric()]
        
        if (nrow(dt_enhanced[Category == 'Subject']) > 0) {
            ref_long <- dt_enhanced[Category == 'Subject', lon][1]
            ref_lati <- dt_enhanced[Category == 'Subject', lat][1]
            
            dt_enhanced[, ref_lon := ref_long]
            dt_enhanced[, ref_lat := ref_lati]
        }
        
        dt_enhanced[, Distance_From_Subject_Miles := vec_get_distance_from_subject(lon, lat, ref_lon, ref_lat)]
        
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
                    12,
                    tags$div(
                        h3('Input data format'),
                        p('Prepare a CSV file with the following columns. Refer to the table below for an example:')
                    ),
                    tableOutput('column_specification'),
                    br(),
                    hr()
                )

            ),
            
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
                        h3('Raw data'),
                        p('Continue to the next tab when you see the data below')
                    ),
                    withSpinner(DTOutput('raw_data'))
                )
            )
        ),
        
        tabPanel(
            'Comps maps',
            fluidRow(
                column(
                    12,
                    textOutput('wait_for_data'),
                    leafletOutput('leaflet_map'),
                    br(),
                    hr()
                )
            ),
            fluidRow(
                column(
                    12,
                    h4('Subject property'),
                    DTOutput('display_subject_data'),
                    br()
                )
            ),
            
            fluidRow(
                column(12, h4('Filters'))
            ),
            
            fluidRow(
                column(
                    4,
                    dateRangeInput('sales_date', 'Sales Date', start=(today() - 30), end=NULL)
                ),
                
                column(
                    4,
                    sliderInput('distance_from_subject', 'Distance from Subject (mi)', min=0, max=10, value=5)
                    # verbatimTextOutput('diagnostic')
                )
            ),
            fluidRow(
                column(
                    12,
                    h4('Comparables'),
                    DTOutput('display_comps_data')
                )
            )
                    
            # 
                    
        )
    )
)


server <- function(session, input, output) {
    
    input_raw <- reactive({
        input_data <- input$input_file
        if(is.null(input_data)) {return()}
        
        dt <- data.table(
            read.csv(input_data$datapath)
        )[, 1:10]
        
        setnames(dt, c(
            'APN', 'Category', 'Site_Address', 'Property_Type',
            'Listing_Price', 'Sale_Price', 'Sale_Date', 'Bedrooms',
            'Bathrooms', 'Structure_SqFt'
        ))
        
        dt_enhanced <- enhance_dataset(dt)
        
    })
    
    input_display_subject <- reactive({
        dt <- input_raw()[Category == 'Subject']
    })
    
    input_display_comps <- reactive({
        dt <- input_raw()[Category == 'Comp']
    })
    
    output$wait_for_data <- renderText({
        
        if (!is.null(input_raw())) {return()}
        paste0('Waiting for data. Please go back to the Import data tab.')
        
    })
    
    # output$diagnostic <- renderText({
    #     input_raw()[, as.numeric(min(as.Date(Sale_Date, format='%m/%d/%Y', origin='1970-01-01')))]
    # })
    
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
        if(is.null(input_raw())) {return()}
        
        dt <- data.table(input_raw())
        plot_lf(dt)
        
    })
    
    output$column_specification <- renderTable({
        
        dt <- data.table(
            APN = c('0000-000-000', '0000-000-001'),
            Category = c('Subject', 'Comp'),
            Site_Address = c('100 A Street, Los Angeles, CA 90045', '225 New Street, Los Angeles, CA 90045'),
            Property_Type = c('SFR', 'SFR'),
            Sale_Price = c(1000000, 550000),
            Sale_Date = c('3/5/2022', '12/30/2021'),
            Bedrooms = c(3, 2),
            Bathrooms = c(2.5, 3),
            Structure_SqFt = c(2432, 1700)
        )
        

    })
    
    output$raw_data <- renderDT({
        if(is.null(input_raw())) {return()}
        
        # col_spec <- c('Site_Address', 'Sale_Date', 'Sale_Price', 'Fitted_Price', 'Pr_per_SqFt', 'Bedrooms', 'Bathrooms', 'lat', 'lon')
        raw_dt <- input_raw()

    })
    
    output$display_subject_data <- renderDT({
        # if(is.null(input_display_subject())) {return()}
        if(is.null(input_raw())) {return()}
        dt_out <- input_display_subject()
        
        dt_out[, .(
            APN, Site_Address, Property_Type, Sale_Price, Sale_Date,
            Bedrooms, Bathrooms, Structure_SqFt, Pr_per_SqFt
        )] %>% 
            DT::datatable() %>%  
            formatCurrency(columns=c('Sale_Price', 'Pr_per_SqFt'), digits=0)

    })
    
    output$display_comps_data <- renderDT({
        # if(is.null(input_display_comps())) {return()}
        if(is.null(input_raw())) {return()}
        
        ## input_display_comps() has the lat, lon info needed for mapping
        ## but not going to display it in the return value
        dt_out <- input_display_comps()[as.numeric(as_date(Sale_Date, format='%m/%d/%Y')) %between% c(input$sales_date[1], input$sales_date[2])]
        dt_out <- dt_out[Distance_From_Subject_Miles <= input$distance_from_subject]
        
        dt_out[,.(
            APN, Site_Address, Property_Type, Sale_Price, Sale_Date,
            Bedrooms, Bathrooms, Structure_SqFt, Pr_per_SqFt, Distance_From_Subject_Miles
        )] %>% 
            DT::datatable() %>% 
            formatCurrency(columns=c('Sale_Price', 'Pr_per_SqFt'), digits=0)

    })
    

        
}

# Run the application 
shinyApp(ui = ui, server = server)
