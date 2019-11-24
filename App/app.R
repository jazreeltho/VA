library(shiny)
library(sf)
library(dplyr)
library(tidyverse)
library(tmap)
library(lubridate)
library(plotly)
library(Hmisc)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(fmsb)
library(leaflet)
library(reshape2)
library(tidyr)
library(ggrepel)
library(corrplot)
tmap_mode("view")

### READ AND MERGE RELEVANT DATA FROM FILE ###
# 1. OVERALL LONDON DATA 
london_data_df <- read_csv('../Cleaned Data/londonData.csv')
london_data_df <- london_data_df %>% mutate(DATE = as.character(DATE))
london_data_sf <- st_as_sf(london_data_df, coords = c("Longitude","Latitude"), crs=4326)

# 2. LONDON WARD SPATIAL FILE
london_ward <- st_read(dsn = "../London_wards/London_wards_sf", 
                       layer = "London_Ward") %>%
                        st_transform(crs=4326)
london_ward_df <- data.frame(london_ward)

# 3. JOINING LONDON DATA SF WITH LONDON WARD SF 
# joint_data_sf <- st_join(london_data_sf, left=FALSE, london_ward)
# joint_data_df <- data.frame(joint_data_sf)
districtData_df <- read_csv('../Cleaned Data/districtData.csv')
districtData_df <- districtData_df %>% mutate(DATE = as.character(DATE))


# 4. POLICE DATA FILE 
police <- read_csv(file="../Cleaned Data/police-counters.csv")
police_sf <- st_as_sf(police,coords=c("longitude","latitude"), crs=4326)

# 5. AGGREGATE COUNTS OF Neighbourhood
# agg_Nbh <- read_csv('../Cleaned Data/agg_NBH.csv')
# agg_Nbh <- agg_Nbh %>% mutate(DATE = as.character(DATE))
# print(head(agg_Nbh))
# agg_Borough_join <- inner_join(london_ward, agg_Borough, by="DISTRICT" )


# DROP DOWN SELECT LISTS

nbhList <- unique(london_ward_df['NAME'])
boroughList <- unique(london_ward_df['DISTRICT'])
crimeList <- unique(london_data_df['Crime type'])
########## END OF ALL DATA #########


# Define UI for application
ui <- fillPage(
    
    titlePanel("London Crime Rate Dashboard"),
    
    tags$style("
        body {
            -moz-transform: scale(0.8, 0.8); /* Moz-browsers */
            zoom: 0.8; /* Other non-webkit browsers */
            zoom: 80%; /* Webkit browsers */
        }
               "),
    
    ### ALL PANELS ###
    tabsetPanel(
        
        ### TAB 1 ###
        tabPanel("Overview",
                 wellPanel(
                     fluidRow(
                         column(12,
                                sliderInput('overviewDate', 'Time Period',
                                            width='100%',
                                            min=as.Date("2017-01-01"),
                                            max=as.Date('2019-08-01'),
                                            value=c(as.Date('2017-01-01'), as.Date('2019-08-01')),
                                            timeFormat='%b %Y')
                         )
                     ),
                     fluidRow(
                         column(6,
                                selectInput('nbh_select', 'Neighbourhood',
                                            choices = c('All', nbhList),
                                            selected='All'
                                )
                         ),
                         column(6,
                                selectInput('crime_type', 'Crime Type',
                                            choices=c('All', crimeList),
                                            selected="All")
                         )
                     )
                 ),
                 fluidRow(
                     column(7,
                         leafletOutput('map_output', height='600px')
                            ),
                    column(5,
                             plotOutput('chart2', height='300px'),
                             plotOutput('chart3', height='300px')
                         )
                 )
            ),
        ### TAB 1 END ###
        
        
        ### TAB 2 ###
        tabPanel("Crime Correlation",  
            wellPanel(
                fluidRow(
                    column(12,
                           sliderInput('overviewDate2', 'Time Period',
                                       width='100%',
                                       min=as.Date("2017-01-01"),
                                       max=as.Date('2019-08-01'),
                                       value=c(as.Date('2017-01-01'), as.Date('2019-08-01')),
                                       timeFormat='%b %Y')
                    )
                ),
                fluidRow(
                    column(4, 
                           selectInput('nbh_select2', 'Neighbourhood',
                                       choices = c('All', nbhList),
                                       selected='All'
                           )
                       ),
                    column(4,
                           selectInput('crime_type1', 'Crime Type #1',
                                       choices=c(crimeList),
                                       selected="Anti-social behaviour")
                    ),
                    column(4,
                           selectInput('crime_type2', 'Crime Type #2',
                                       choices=c(crimeList),
                                       selected="Burglary")
                    )
                )
            ),
            fluidRow(
                column(6, 
                       plotOutput('chart5', height='600px')),
                column(6,
                       plotOutput('chart4', height='600px'))
            )
        )
        ### TAB 2 END ### 
        
    ),
    tags$footer(
        style = "
        height:50px"
    )
    ### END OF ALL PANELS ###
)


server <- function(input, output) {
    
    user_inputs <- reactiveValues()
    chart1data <- reactiveValues()
    chart2data <- reactiveValues()
    chart3data <- reactiveValues()
    
    observeEvent(c(input$overviewDate, input$nbh_select, input$crime_type), {
        
        user_inputs$date_select <- c(as.character(format(input$overviewDate[1],format = "%Y-%m-%d")), as.character(format(input$overviewDate[2],format = "%Y-%m-%d")))
        user_inputs$date_range <- c(format(seq(as.Date(user_inputs$date_select[1]), as.Date(user_inputs$date_select[2]), by="months"), format = "%Y-%m"))
        
        chart1data$districtData <- districtData_df %>% filter(DATE %in% c(format(seq(as.Date(user_inputs$date_select[1]), as.Date(user_inputs$date_select[2]), by="months"), format = "%Y-%m")))
        chart2data$districtData <- districtData_df %>% filter(DATE %in% user_inputs$date_range) 
        chart3data$districtData <- districtData_df %>% filter(DATE %in% user_inputs$date_range)
        
        
        
        
        if (input$nbh_select != 'All') {
            chart1data$districtData <- chart1data$districtData %>% filter(NAME == input$nbh_select)
            chart2data$districtData <- chart2data$districtData %>% filter(NAME == input$nbh_select)
            chart3data$districtData <- chart3data$districtData %>% filter(NAME == input$nbh_select)
        }
        
        if (input$crime_type != 'All') {
            chart1data$districtData <- chart1data$districtData %>% filter(Crime.type == input$crime_type)
            chart2data$districtData <- chart2data$districtData %>% filter(Crime.type == input$crime_type)
            chart3data$districtData <- chart3data$districtData %>% filter(Crime.type == input$crime_type)
        }

        chart1data$districtData <- chart1data$districtData %>% group_by(NAME) %>% count(NAME)
        chart1data$districtData <- chart1data$districtData %>% rename(`Crime Count` = n)
        chart1data$districtData <- inner_join(london_ward, chart1data$districtData, by='NAME')
        chart1data$toPlot <- tm_shape(chart1data$districtData) +
             
             #tm_polygons help to map each different geospatial shape. Palette is the colours you wanna shade it. Border colour can change. Auto palette.mappling please set FALSE if not the whole thing will go haywire. n = is the number of cluster you wanna group it into. Style leave it to quantile so they will help map each cluster. Alpha represents the transparency
             tm_polygons("Crime Count",palette = "Reds" ,id="NAME", title = "Crime level by Neighbourhood", border.col = "grey", n = 8, style="jenks" , alpha = 0.45) +
             
             #tm_shape is the plotting of the location drop pin
             tm_shape(police_sf) +
             
             #tm symbols shows the image you uploaded. shape = marker_icon where u set earlier on top of the code, Size just make it small if not it will be super big
             tm_symbols(shape=marker_icon(),
                        size = 0.15,
                        border.col = "black",
                        border.lwd = 1)
        
        chart2data$districtData <- chart2data$districtData %>% count(Crime.type)
        
        
        chart3data$districtData <- chart3data$districtData %>% count(MONTH,YEAR)
        chart3data$districtData <- subset(chart3data$districtData,select=c(MONTH, n))
        chart3data$districtData <- chart3data$districtData %>% group_by(MONTH) %>% summarise(n = mean(n))
        chart3data$average <- chart3data$districtData %>% summarise(m=mean(n))
        chart3data$averageofmonths <- as.vector(chart3data$average$m[1])
    })
    
    
    output$map_output <- renderLeaflet({
        tmap_leaflet(chart1data$toPlot)
    })
    
    
    output$dateText <- renderText({user_inputs$date_range})
       

    ### JAZ CHART 2 ###

    output$chart2 <- renderPlot({
        ggplot(data=chart2data$districtData, aes(x=reorder(Crime.type, -n), y=n, fill=n)) +
        geom_bar(stat="identity")+
        scale_fill_gradient(low = "grey", high = "red")+
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 60, hjust = 1))+
        xlab("Crime Type") +
        ylab("Crime Count") + 
        theme(legend.position = "none") + 
        scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
    })
    
    ###| JAZ CHART 2 |###
    
    ### JAZ CHART 3 ###

    output$chart3 <- renderPlot({
        ggplot(data=chart3data$districtData, aes(x=reorder(month.abb[as.numeric(MONTH)],as.numeric(MONTH)), y=n,group=1)) +
        geom_line()+
        geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
        xlab("Month") +
        ylab("Crime Rate") +
        geom_hline(yintercept = chart3data$averageofmonths,linetype="dashed",color = "red")
    })
    
    ###| JAZ CHART 3 |###
    
    
    ### JAZ CHART 4 ###
    user_inputs2 <- reactiveValues()
    chart4data <- reactiveValues()
    chart5data <- reactiveValues()
    
    observeEvent(
        c(input$overviewDate2, input$crime_type1, input$crime_type2, input$nbh_select2), {

            user_inputs2$date_select <- c(as.character(format(input$overviewDate2[1],format = "%Y-%m-%d")), as.character(format(input$overviewDate2[2],format = "%Y-%m-%d")))
            user_inputs2$date_range <- format(seq(as.Date(user_inputs2$date_select[1]), as.Date(user_inputs2$date_select[2]), by="months"), format = "%Y-%m")
            
            if (input$nbh_select2 == 'All') {
                chart4data$districtData <- districtData_df %>% filter(DATE %in% user_inputs2$date_range) %>% count(MONTH, YEAR, NAME, Crime.type)
                chart5data$districtData <- districtData_df %>% filter(DATE %in% user_inputs2$date_range) %>% count(MONTH, YEAR, NAME, Crime.type)
            }
            else {
                chart4data$districtData <- districtData_df %>% filter(DATE %in% user_inputs2$date_range) %>% count(MONTH, YEAR, NAME, Crime.type) %>% filter(NAME == input$nbh_select2)
                chart5data$districtData <- districtData_df %>% filter(DATE %in% user_inputs2$date_range) %>% count(MONTH, YEAR, NAME, Crime.type) %>% filter(NAME == input$nbh_select2)
            }
            
            chart4data$districtData <- subset(chart4data$districtData,select=c(MONTH, Crime.type, n))
            chart4data$districtData <- chart4data$districtData %>% group_by(MONTH,Crime.type) %>% summarise(n = mean(n))
            chart4data$districtData <- chart4data$districtData %>% filter(Crime.type == input$crime_type1|Crime.type == input$crime_type2)
            chart4data$districtData <- dcast(chart4data$districtData, formula = MONTH ~ Crime.type, fun.aggregate = sum, value.var = "n")
            
            chart5data$districtData <- subset(chart5data$districtData, select=-c(YEAR, NAME))
            chart5data$districtData <- chart5data$districtData %>% group_by(MONTH, Crime.type) %>% summarise(n = mean(n))
            
            chart5data$districtData <- dcast(data = chart5data$districtData, formula = MONTH ~ Crime.type, fun.aggregate = sum, value.var = "n")
            chart5data$districtData <- as.matrix(chart5data$districtData[2:ncol(chart5data$districtData)])
            chart5data$districtData <- cor(chart5data$districtData)
            chart5data$col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#E2EEC2", "#03c03c"))
        
    })
    
    output$chart4 <- renderPlot({
        chart4data$districtData %>% 
        ggplot(aes(x=chart4data$districtData[,2], y=chart4data$districtData[,3],text = paste("Crime Type 1: ", ... = chart4data$districtData[,2], "\nCrime Type 2: ", chart4data$districtData[,3], "\nMonth: ", MONTH, sep=""))) +
        geom_point(color="#69b3a2") +
        geom_text(aes(label=month.abb[as.numeric(MONTH)]), nudge_y = +0.1,hjust=0, vjust=0)+
        xlab("Crime Type1") +
        ylab("Crime Type2") +
        geom_segment(size = 1, color="#69b3a2", 
                     aes(
                         xend=c(tail(chart4data$districtData[,2], n=-1), NA), 
                         yend=c(tail(chart4data$districtData[,3], n=-1), NA)
                     ),
                     arrow=arrow(length=unit(0.3,"cm"))
        ) 
    })
    
    ###| JAZ CHART 4 |###
    
    
    ### JAZ CHART 5 ###
    
    output$chart5 <- renderPlot({
        corrplot(chart5data$districtData, method="color", col=chart5data$col(200),  
                 type="upper", order="hclust", 
                 addCoef.col = "black",
                 number.cex = 1,# Add coefficient of correlation
                 tl.col="black", tl.srt=45, #Text label color and rotation
                 # Combine with significance
                 sig.level = 0.01,
                 #p.mat = p.mat, sig.level = 0.01, insig = "blank", 
                 # hide correlation coefficient on the principal diagonal
                 diag=FALSE 
        )
        
    })
    

    ###| JAZ CHART 5 |###
    
    
    ### JUE HONG MAPS ###
    
    #------------------------POLICE STATIONS BY YEAR = 2018 MONTH = 10--------------------------------#
    #tmap_icons, sets the icon picture, with height can make smaller, keep.asp must be false to help resize
    # tmap_icons("../images/drop_pin.png",width = 28, height = 28, keep.asp = FALSE)
    # 
    # #tm_shape maps the selection data given, if you wanna map crime rate, you need to change districtdata_userselection
    # tm_shape(districtdata_userselection) + 
    #     
    #     #tm_polygons help to map each different geospatial shape. Palette is the colours you wanna shade it. Border colour can change. Auto palette.mappling please set FALSE if not the whole thing will go haywire. n = is the number of cluster you wanna group it into. Style leave it to quantile so they will help map each cluster. Alpha represents the transparency 
    #     tm_polygons("n",palette = "Reds" ,id="NAME", title = "Crime level by District", border.col = "grey", n = 8, auto.palette.mapping = FALSE, style="quantile" , alpha = 0.45) +
    #     
    #     #tm_shape is the plotting of the location drop pin  
    #     tm_shape(police_sp)+
    #     
    #     #TM_BUBBLES - make small bubble instead of drop down pin
    #     #TM_MARKERS - shows aggregated data in the circle
    #     
    #     #tm symbols shows the image you uploaded. shape = marker_icon where u set earlier on top of the code, Size just make it small if not it will be super big
    #     tm_symbols(shape=marker_icon(),
    #                size = 0.15,
    #                border.col = "black",
    #                border.lwd = 1)
    ###| JUE HONG MAPS |###
}

# Run the application 
shinyApp(ui = ui, server = server)

