# This is a Shiny app


library(shiny)
library(sf)
library(tidyverse)
library(ggmap)
library(leaflet)

##### load data files
# load the census shape file
census <- st_read("C:/Users/micha/OneDrive/Documents/Data Viz/Github Data Sets/FinalProject/2010_CensusData/2010_CensusData.shp")

# load the abandoned properties shape file
abandon_prop <- st_read("C:/Users/micha/OneDrive/Documents/Data Viz/Github Data Sets/FinalProject/Abandoned_Property_Parcels/Abandoned_Property_Parcels.shp")

# load the school boundaries shape file
school <- st_read("C:/Users/micha/OneDrive/Documents/Data Viz/Github Data Sets/FinalProject/School_Boundaries/School_Boundaries.shp")

# load the business licenses data (geocoded)
business <-  read.csv("C:/Users/micha/OneDrive/Documents/Data Viz/Github Data Sets/FinalProject/Business_Licenses_geocoded.csv")

# load the parks data
parks <-  read.csv("C:/Users/micha/OneDrive/Documents/Data Viz/Github Data Sets/FinalProject/Parks_Locations_and_Features.csv")
summary(parks)

# load the public facilities data
facilities <-  read.csv("C:/Users/micha/OneDrive/Documents/Data Viz/Github Data Sets/FinalProject/Public_Facilities.csv")
#####

# convert the parks data to a spatial data frame
parks_spatial <- parks %>%
    st_as_sf(coords = c("Lon","Lat")) %>% 
    st_set_crs(value = 4326)

# convert the parks data to a spatial data frame
business_spatial <- business %>%
    st_as_sf(coords = c("X","Y")) %>% 
    st_set_crs(value = 4326)


# set the crs value for the census  and abandoned properties data to match the parks df
census <- st_transform(census, 4326)
abandon_prop <- st_transform(abandon_prop, 4326)


# extract/transform aspects of the school data for later use
school.data <- school %>% st_set_geometry(NULL)
school_type <- school.data$SchoolType


# create the list of names for the input lists of the ui 
program_list <- parks_spatial %>% select(Aqua_Feat__Pool:Water_Feature) %>% colnames()
abandon_type <- unique(abandon_prop$Outcome_St)
business_type <- unique(business_spatial$Classifi_1)


# create lists of labels and variable names for demographic categories
demographic_names <- c("Population Density (per sq. mile)",
                       "Total Population",
                       "Total Population: Male",
                       "Total Population: Female",
                       "Total Population: Under 5 years",
                       "Total Population: 5 to 9 years",
                       "Total Population: 10 to 14 years",
                       "Total Population: 15 to 17 years",
                       "Total Population: 18 to 24 years",
                       "Total Population: 25 to 34 years",
                       "Total Population: 35 to 44 years",
                       "Total Population: 45 to 54 years",
                       "Total Population: 55 to 64 years",
                       "Total Population: 65 and 74 years",
                       "Total Population: 75 to 84 years",
                       "Total Population: 85 years and over",
                       "Total population: White alone",
                       "Total population: Black or African American alone",
                       "Total population: American Indian and Alaska Native alone",
                       "Total population: Asian alone",
                       "Total population: Native Hawaiian and Other Pacific Islander alone",
                       "Total population: Some Other Race alone",
                       "Total population: Two or More Races",
                       "Total population: In group quarters: Institutionalized population",
                       "Total population: In group quarters: Noninstitutionalized population")




# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Data Viz Final Project: South Bend Mayoral Dashboard"),

    tabsetPanel(
        tabPanel("Parks",
                 br(), br(),
                 fluidRow(
                     column(width = 5,
                            selectInput(inputId = "program",
                                        label = "Park program/facility:",
                                        choices = program_list,
                                        selected = "Ballfield"
                            )
                     ),
                     column(width = 5,
                            selectInput(inputId = "demographic",
                                        label = "Demographic group:",
                                        choices = demographic_names
                            )
                     )
                 ),
                 
                 # Show output
                 fluidRow(
                     leafletOutput("parks_map"),
                 ),
                 br(), br(),
                 fluidRow(
                     column(width = 8,
                            plotOutput("parks_bar")
                            ),
                     column(width = 4,
                            plotOutput("parks_bar2")
                     ),
                 )
        ),
        
        tabPanel("Business and Abandon Properties",
                 br(), br(),
                 fluidRow(
                     column(width = 5,
                            selectInput(inputId = "property",
                                        label = "Property Type:",
                                        choices = abandon_type,
                                        selected = "Properties"
                            )
                     )
                 ),  
                 
                 # Show output
                 fluidRow(
                     leafletOutput("abandon_map"),
                 ),
                 br(), br(),
                 fluidRow(
                     column(width = 5,
                            selectInput(inputId = "business",
                                        label = "Business Type:",
                                        choices = business_type,
                                        selected = "RESTAURANTS A-M"
                            )
                     )
                 ), 
                 
                 fluidRow(
                     leafletOutput("business_map")
                 ),
                 br(), br(),
                 htmlOutput("text"),
                 fluidRow(
                     column(width = 12,
                            plotOutput("abandon_bar")
                     ),
                 ),  
                 br(), br(),
                 htmlOutput("text_1"),
                 fluidRow(  
                     column(width = 12,
                            plotOutput("business_bar")
                     ),
                 )
        ),
        
        tabPanel(title = 'Schools',
                 br(), br(),
                 fluidRow(
                     column(width = 5, 
                            selectInput("schooltype", "Choose a School Type", 
                                        c("All","Public", "Private")
                            )
                     ),
                     column(width = 5,
                            selectInput(inputId = "demo_school",
                                        label = "Demographic group:",
                                        choices = demographic_names)
                     )
                 ),
                 
                 fluidRow(
                     leafletOutput(outputId = 'school_map')
                 ),
                 br(), br(),
                 fluidRow(
                     div(style='height:1000px; overflow-y: scroll',
                         plotOutput("school_bar2")
                     )
                 )
        )
    )
)


# Define server logic required
server <- function(input, output) {

    #### Code used to create parks tab begins here ####
    # get the demographic column from the census df selected in the input
    demo_col <-  reactive({
        switch(input$demographic,
               "Population Density (per sq. mile)" = census$SE_T002_01,
               "Total Population" = census$SE_T003_00,
               "Total Population: Male" = census$SE_T003_01,
               "Total Population: Female" = census$SE_T003_02,
               "Total Population: Under 5 years" = census$SE_T008_01,
               "Total Population: 5 to 9 years" = census$SE_T008_02,
               "Total Population: 10 to 14 years" = census$SE_T008_03,
               "Total Population: 15 to 17 years" = census$SE_T008_04,
               "Total Population: 18 to 24 years" = census$SE_T008_05,
               "Total Population: 25 to 34 years" = census$SE_T008_06,
               "Total Population: 35 to 44 years" = census$SE_T008_07,
               "Total Population: 45 to 54 years" = census$SE_T008_08,
               "Total Population: 55 to 64 years" = census$SE_T008_09,
               "Total Population: 65 and 74 years" = census$SE_T008_10,
               "Total Population: 75 to 84 years" = census$SE_T008_11,
               "Total Population: 85 years and over" = census$SE_T008_12,
               "Total population: White alone" = census$SE_T054_01,
               "Total population: Black or African American alone" = census$SE_T054_02,
               "Total population: American Indian and Alaska Native alone" = census$SE_T054_03,
               "Total population: Asian alone" = census$SE_T054_04,
               "Total population: Native Hawaiian and Other Pacific Islander alone" = census$SE_T054_05,
               "Total population: Some Other Race alone" = census$SE_T054_06,
               "Total population: Two or More Races" = census$SE_T054_07,
               "Total population: In group quarters: Institutionalized population" = census$SE_T063_16,
               "Total population: In group quarters: Noninstitutionalized population" = census$SE_T063_17)
    })
    
    # create a condensed census df based on the column selected by input
    census_new <- reactive({
        census %>% select(NAMELSAD, Geo_QName, geometry) %>% cbind(demo_col())
    })
    
    
    # filter parks data to for only parks with the program selected in the input
    parks_new <- reactive({
        parks_spatial %>% filter(!is.na(get(input$program))) %>%
            select(Park_Name, Park_Type, input$program)
    })
    
    # join the parks and new census data in order to plot the bar chart
    plot_df <- reactive({
        st_join(x = parks_new(), y = census_new())
    })
    
    # create a bar chart of demographic value for the parks
    output$parks_bar <- renderPlot({
        ggplot(plot_df(), aes(x = Park_Name, y = demo_col.., fill = NAMELSAD)) +
            geom_col() + coord_flip() +
            geom_text(aes(label = round(demo_col.., 0), hjust = -0.1)) +
            labs(title = "Local Demographic Information for Each Park") +
            xlab(NULL) +
            ylab(input$demographic) +
            scale_fill_discrete(name = "Census Region")
    })
    
    # create a bar chart of the number of programs at each parks
    output$parks_bar2 <- renderPlot({
        ggplot(plot_df(), aes(x = Park_Name, y = get(input$program))) +
            geom_col() + coord_flip() +
            labs(title = "Facilities at each Park") +
            xlab(NULL) +
            ylab(paste("Number of ", input$program))
    })
    
    # create the map for the parks
    output$parks_map <- renderLeaflet({
        popup <- paste("<b>", parks_new()$Park_Name, "</b><br>",
                       "Type: ", parks_new()$Park_Type, "<br>", 
                       "Number of ", input$program, "s: ", #"<br>", sep ="")
                       parks_new()[[input$program]], "<br>", sep ="")
         
        
        pal <- colorFactor(palette = 'Dark2', domain = parks_new()$Park_Type)
        pal2 <- colorNumeric(palette = "YlGnBu", domain = census_new()$demo_col)
        
        leaflet(census_new())  %>%
            setView(-86.2520, 41.6764, zoom = 12) %>%
            addTiles()  %>%
            addPolygons(color = ~pal2(census_new()$demo_col), weight = 1, 
                        smoothFactor = 0.5, opacity = 0.2, fillOpacity = 0.5) %>%
            addLegend("bottomright", pal = pal2, values = ~census_new()$demo_col,
                      title = "Dem. values", opacity = 1) %>%
            addCircleMarkers(data = parks_new(), color = ~pal(parks_new()$Park_Type),
                             popup = ~popup, stroke = 0, fillOpacity = 1, 
                             radius = 5)
    })
    
    
    
    #### Code used to create business and abandoned properties tab begins here ####
    #create titles for bar charts as separate objects
    output$text <- renderText({
        HTML(paste("<b>Summary of All Abandon Properties by enforment Code </b>", "", sep="<br/>"))
    })
    
    output$text_1 <- renderText({
        HTML(paste("<b>Summary of All Businesses by Type</b>", "", sep="<br/>"))
    })
    
    
    # filter the abandoned properties data based on the input
    abandon_property <- reactive({
        abandon_prop %>% filter(Outcome_St==input$property) 
    })
    
    # filter the business properties data based on the input
    business_property <- reactive({
        business_spatial %>% filter(Classifi_1==input$business) 
    })
    
    # create the map for the abandoned properties
    output$abandon_map <- renderLeaflet({
        popup <- paste("<b>", abandon_property()$Street_Nam, "</b><br>",
                       "Status: ", abandon_property()$Code_Enfor, "<br>", 
                       "<br>", sep ="")
        
        
        pal <- colorFactor(palette = 'Dark2', domain = abandon_property()$Street_Nam)
        pal2 <- colorNumeric(palette = "YlGnBu", domain = census_new()$demo_col)
        
        leaflet(census_new())  %>%
            setView(-86.2520, 41.6764, zoom = 12) %>%
            addTiles()  %>%
            addPolygons(color = ~pal2(census_new()$demo_col), weight = 1, 
                        smoothFactor = 0.5, opacity = 0.2, fillOpacity = 0.5) %>%
            addLegend("bottomright", pal = pal2, values = ~census_new()$demo_col,
                      title = "Population Density (per sq. mile)", opacity = 1) %>%
            addPolygons(data = abandon_property(), popup = ~popup,
                        color = ~pal(abandon_property()$Code_Enfor))
    })
    
    # create the map for the business properties
    output$business_map <- renderLeaflet({
        popup <- paste("<b>", business_property()$Business_N, "</b><br>",
                       "Address: ", business_property()$Full_Addre, "<br>", 
                       "Type :- ", input$business, sep ="")
        
        
        pal <- colorFactor(palette = 'Dark2', domain = business_property()$Classifi_1)
        pal2 <- colorNumeric(palette = "YlGnBu", domain = census_new()$demo_col)
        
        leaflet(census_new())  %>%
            setView(-86.2520, 41.6764, zoom = 12) %>%
            addTiles()  %>%
            addPolygons(color = ~pal2(census_new()$demo_col), weight = 1, 
                        smoothFactor = 0.5, opacity = 0.2, fillOpacity = 0.5) %>%
            addLegend("bottomright", pal = pal2, values = ~census_new()$demo_col,
                      title = "Population Density (per sq. mile)", opacity = 1) %>%
            addCircleMarkers(data = business_property(), popup = ~popup,
                             color = ~pal(business_property()$Classifi_1), stroke = 0, fillOpacity = 1, 
                             radius = 5) 
    })
    
    # create bar chart for abandoned properties
    output$abandon_bar <- renderPlot({
        ggplot(abandon_prop, aes(x = Outcome_St, fill = Code_Enfor)) +
            geom_bar() +
            labs(title = "Summary of abandon Properties") +
            scale_fill_discrete(name = "Code Enforment") +
            theme(axis.text.x = element_text(angle = 90))
    })
    
    # create bar chart for business properties
    output$business_bar <- renderPlot({
        ggplot(business, aes(x = Classifi_1, fill = License__1)) +
            geom_bar() +
            labs(title = "Summary of business") +
            scale_fill_discrete(name = "License State") + 
            theme(axis.text.x = element_text(angle = 90))
    })
    
    
    
    #### Code used to create schools tab begins here ####
    # get the demographic column from the census df selected in the input
    demo_col2 <-  reactive({
        switch(input$demo_school,
               "Population Density (per sq. mile)" = census$SE_T002_01,
               "Total Population" = census$SE_T003_00,
               "Total Population: Male" = census$SE_T003_01,
               "Total Population: Female" = census$SE_T003_02,
               "Total Population: Under 5 years" = census$SE_T008_01,
               "Total Population: 5 to 9 years" = census$SE_T008_02,
               "Total Population: 10 to 14 years" = census$SE_T008_03,
               "Total Population: 15 to 17 years" = census$SE_T008_04,
               "Total Population: 18 to 24 years" = census$SE_T008_05,
               "Total Population: 25 to 34 years" = census$SE_T008_06,
               "Total Population: 35 to 44 years" = census$SE_T008_07,
               "Total Population: 45 to 54 years" = census$SE_T008_08,
               "Total Population: 55 to 64 years" = census$SE_T008_09,
               "Total Population: 65 and 74 years" = census$SE_T008_10,
               "Total Population: 75 to 84 years" = census$SE_T008_11,
               "Total Population: 85 years and over" = census$SE_T008_12,
               "Total population: White alone" = census$SE_T054_01,
               "Total population: Black or African American alone" = census$SE_T054_02,
               "Total population: American Indian and Alaska Native alone" = census$SE_T054_03,
               "Total population: Asian alone" = census$SE_T054_04,
               "Total population: Native Hawaiian and Other Pacific Islander alone" = census$SE_T054_05,
               "Total population: Some Other Race alone" = census$SE_T054_06,
               "Total population: Two or More Races" = census$SE_T054_07,
               "Total population: In group quarters: Institutionalized population" = census$SE_T063_16,
               "Total population: In group quarters: Noninstitutionalized population" = census$SE_T063_17)
    })
    
    # create a condensed census df based on the column selected by input
    census_new2 <- reactive({
        census %>% select(NAMELSAD, Geo_QName, geometry) %>% cbind(demo_col2())
    })
    
    
    #Reactive Subset for School Types
    school_react <- reactive({
        if (input$schooltype == 'Public') {school.data %>% filter(SchoolType == 'Public')}
        else if (input$schooltype == 'Private') {school.data %>% filter(SchoolType == 'Private')}
        else {school.data}
    })
    
    
    # join the schools and census data in order to plot the bar chart
    plot_df2 <- reactive({
        st_join(x = school, y = census_new2())
    })
    
    
    # create a bar chart of demographic value for the schools
    output$school_bar2 <-  renderPlot({
        ggplot(plot_df2(), aes(x = School, y = demo_col2.., fill = NAMELSAD)) +
            geom_col() + coord_flip() +
            geom_text(aes(label = round(demo_col2.., 0))) +
            labs(title = "Local Demographic Information for Each School") +
            xlab(NULL) +
            ylab(input$demo_school) +
            scale_fill_discrete(name = "Census Region")
    })
    
    
    # Create School Map
    output$school_map <- renderLeaflet({
        
        # choosing palletes
        pal3 <- colorFactor(palette = c('red', 'blue'), domain = c("Public", "Private"))
        pal4 <- colorNumeric(palette = "YlGnBu", domain = census_new2()$demo_col2)
        
        leaflet() %>%
            setView(-86.2520, 41.6764, zoom = 12) %>%
            addTiles() %>%
            addPolygons(data = school, color = ~pal3(school_react()$SchoolType)) %>%
            addPolygons(data = census_new2(), color = ~pal4(census_new2()$demo_col2), weight = 1, 
                        smoothFactor = 0.5, opacity = 0.2, fillOpacity = 0.5) %>%
            addLegend("bottomright", pal = pal4, values = census_new2()$demo_col2,
                      title = "Dem. values", opacity = 1)
        
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
