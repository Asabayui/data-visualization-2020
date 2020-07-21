
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(shinydashboard) 
library(shinydashboardPlus) 
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)
library(leaflet)
library(DT)
library(plotly)
library(zoo)
library(lubridate)
library(tigris)






ui <- dashboardPagePlus(
    header = dashboardHeaderPlus(title = "Coronavirus", 
                                 enable_rightsidebar = TRUE, rightSidebarIcon = "gears"),
    sidebar = dashboardSidebar(
        sidebarMenu(
            menuItem("Monthly Map", tabName = "page3", icon = icon("users")),
            menuItem("Unemployment", tabName = "page4", icon = icon("line-chart")),
            menuItem("Relationship", tabName = "page5", icon = icon("line-chart"))
            
        )
    ),
    body = dashboardBody(
        tabItems(
            tabItem(tabName = "page3",  
                    radioButtons(
                        inputId = "casetype", label = "Select case type", 
                        choices = c("Confirmed","Deaths"), selected = "Confirmed"
                    ),
                    sliderInput(
                        inputId = "month3", min = 1, max = 7, 
                        label = h3("Select month"), value = 1
                    ),
                    
                    leafletOutput("monthlyMap", width="100%")
            ),
                    
            tabItem(tabName = "page4",
                    selectInput("area", label = h3("Select state and area"), 
                                choices = c(''),
                                multiple = TRUE,selected="Maryland"),
                    plotlyOutput("unline")
            ),
            tabItem(tabName = "page5",
                    sliderInput(
                        inputId = "month", min = 1, max = 5, 
                        label = h3("Select month"), value = 1
                    ),
                    radioButtons(
                        inputId = "casetype", label = "Select case type", 
                        choices = c("Confirmed","Deaths"), selected = "Confirmed"
                    ),
                    plotlyOutput("relationship")
                    )
        )
    ),
    
    
    
    
    rightsidebar = rightSidebar(
        tags$a("Data Source",href="https://github.com/CSSEGISandData/COVID-19",
               target="_blank")
        
    ),
    title = "DashboardPage"

)

server <- function(input, output, session) {
    
    dailydeaths <- read.csv("time_series_covid19_deaths_US.csv")
    dailyconfirmed <- read.csv("time_series_covid19_confirmed_US.csv")
    
    dailydeaths <- gather(data = dailydeaths, key = date, value = deaths, X1.22.20:ncol(dailydeaths)) %>%
        subset(select = -c(1:6))
    dailyconfirmed <- gather(data = dailyconfirmed, key = date, value = confirmed, X1.22.20:ncol(dailyconfirmed)) %>%
        subset(select = -c(1:6))
    
    # Merge two datasets
    # A bit large to read
    daily <- merge(dailydeaths, dailyconfirmed, by = c("Province_State", "Country_Region", "Lat", "Long_", "Combined_Key", "date"))
    daily$date <- as.Date(daily$date, format = "X%m.%d.%y")
    
   
    
    # For the daily-case positioning map
    daily_position <- subset(daily, Lat != 0 & Long_ != 0)
    
    
    ### Total Deaths & Confirmed Cases (Including New & Cumulative)
    total <- read.csv("US covid19.csv") %>%
        subset(select = c(1,3,14,23,20))
    
   # monthly cumulative confirmed and deaths
    cumulative1=daily %>% filter(date=="2020-01-31")%>%group_by(Province_State)%>% 
        summarise(month =1, confirmtotal = sum(confirmed),deathtotal=sum(deaths))
    cumulative2=daily %>% filter(date=="2020-02-29")%>%group_by(Province_State)%>% 
        summarise(month =2, confirmtotal = sum(confirmed),deathtotal=sum(deaths))
    cumulative3=daily %>% filter(date=="2020-03-31")%>%group_by(Province_State)%>% 
        summarise(month =3, confirmtotal = sum(confirmed),deathtotal=sum(deaths))
    cumulative4=daily %>% filter(date=="2020-04-30")%>%group_by(Province_State)%>% 
        summarise(month =4, confirmtotal = sum(confirmed),deathtotal=sum(deaths))
    cumulative5=daily %>% filter(date=="2020-05-31")%>%group_by(Province_State)%>% 
        summarise(month =5, confirmtotal = sum(confirmed),deathtotal=sum(deaths))
    cumulative6=daily %>% filter(date=="2020-06-30")%>%group_by(Province_State)%>% 
        summarise(month =6, confirmtotal = sum(confirmed),deathtotal=sum(deaths))
    cumulative7=daily %>% filter(date=="2020-07-14")%>%group_by(Province_State)%>% 
        summarise(month =7, confirmtotal = sum(confirmed),deathtotal=sum(deaths))
    
    cumulative_monthly = rbind(cumulative1,cumulative2,cumulative3,cumulative4,cumulative5,cumulative6,cumulative7)
    colnames(cumulative_monthly)[1]<-"State and area"
    
   # monthly increased confirmed and deaths
    increaseconfirm = rep(0,length(cumulative_monthly$month))
    for (i in 59:length(cumulative_monthly$month)){
        increaseconfirm[i] = cumulative_monthly$confirmtotal[i]-cumulative_monthly$confirmtotal[i-58]
    }
    cumulative_monthly$increaseconfirm=increaseconfirm
    
    increasedeath = rep(0,length(cumulative_monthly$month))
    for (i in 59:length(cumulative_monthly$month)){
        increasedeath[i] = cumulative_monthly$deathtotal[i]-cumulative_monthly$deathtotal[i-58]
    }
    cumulative_monthly$increasedeath=increasedeath
    
    #For US state map
    states <- states(cb=T)
    colnames(states)[6]<- "State and area"
    states %>% 
        leaflet() %>% 
        addTiles() %>% 
        addPolygons(popup=~`State and area`)

    states_merged_cumulative<- merge(states,cumulative_monthly,by="State and area")
    states_merged_cumulative<- na.omit(states_merged_cumulative)
    
    ### Unemployment Data
    unemployment<- read_csv("ststdsadata2.csv")
    
    
    # Create a new date column shown as year and month 
    yearmon = NULL
    for (i in 1:length(unemployment$Year)){
        yearmon[i]= paste(unemployment$Year[i],unemployment$Month[i],sep='')
    }
        unemployment$yearmon = as.yearmon(yearmon, "%Y%m")
    
   
   
    
    ### Summarize Monthly Data
    
    # Calculate the numbers for each month
    deaths_monthly <- daily %>%
        group_by(as.numeric(format(date,'%m')), Province_State) %>%
        summarise(sum_deaths = sum(deaths)) 
    colnames(deaths_monthly)[1:2] <- c("Month", "State and area")
    
    confirmed_monthly <- daily %>%
        group_by(as.numeric(format(date,'%m')), Province_State) %>%
        summarise(sum_confirmed = sum(confirmed))
    colnames(confirmed_monthly)[1:2] <- c("Month", "State and area")
    
    # Select monthly data for year 2020
    unemploy_monthly <- subset(unemployment, Year == 2020)
    unemploy_monthly$Month <- as.double(unemploy_monthly$Month)
    
    # Combine the monthly data
    monthly <- merge(confirmed_monthly, deaths_monthly, by = c("Month", "State and area"))
    monthly_plus_unemployment <- 
        merge(monthly, unemploy_monthly, by = c("Month", "State and area"), all.x = TRUE) %>%
        na.omit()
    
    # Deal with some columns
    monthly_plus_unemployment$`Civilian non-institutional population` <- 
        monthly_plus_unemployment$`Civilian non-institutional population`/1000
    
 
    # Adjusted input  
    updateSelectInput(session = session,
                      inputId = 'area',
                      choices = unique(unemployment$`State and area`),
                      selected = 'Maryland'
    )
    
    
    output$monthlyMap = renderLeaflet({
        
        #Plotting
        substates_merged_cumulative<-subset(states_merged_cumulative,month==input$month3)
        
        if(input$casetype == "Confirmed"){
            # Creating a color palette based on the number range in the confirmed column
            pal <- colorNumeric("Blues", substates_merged_cumulative$confirmtotal)
            # Setting up the pop up text
            popup_con <- paste0("<strong>", substates_merged_cumulative$`State and area`,
                                "</strong><br />Cumulatively confirmed cases: ", substates_merged_cumulative$confirmtotal,
                                "</strong><br />Increased confirmed cases: ", substates_merged_cumulative$increaseconfirm
            )
            
            leaflet() %>%
                addProviderTiles("CartoDB.Positron") %>%
                setView(-98.483330, 38.712046, zoom = 3.5) %>% 
                addPolygons(data=substates_merged_cumulative,
                            fillColor = ~pal(substates_merged_cumulative$confirmtotal), 
                            fillOpacity = 1, 
                            weight = 0.2, 
                            smoothFactor = 0.2, 
                            popup = ~popup_con) %>%
                addLegend(pal = pal, 
                          values = substates_merged_cumulative$confirmtotal, 
                          position = "bottomright", 
                          title = "Cumulatively confirmed")
            
        }else
            if(input$casetype == "Deaths"){
                pal2 <- colorNumeric(c("#fee5d9","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#99000d"), substates_merged_cumulative$death)
                # Setting up the pop up text
                popup_con2 <- paste0("<strong>", substates_merged_cumulative$`State and area`,
                                     "</strong><br />Cumulatively confirmed cases: ", substates_merged_cumulative$death,
                                     "</strong><br />Increased confirmed cases: ", substates_merged_cumulative$increasedeath
                )
                
                
                
                leaflet() %>%
                    addProviderTiles("CartoDB.Positron") %>%
                    setView(-98.483330, 38.712046, zoom = 3.5) %>% 
                    addPolygons(data=substates_merged_cumulative,
                                fillColor = ~pal2(substates_merged_cumulative$death), 
                                fillOpacity = 1, 
                                weight = 0.2, 
                                smoothFactor = 0.2, 
                                popup = ~popup_con2) %>%
                    addLegend(pal = pal2, 
                              values = substates_merged_cumulative$death, 
                              position = "bottomright", 
                              title = "Cumulative deaths")  
            }
        
    })
    
    
    output$unline = renderPlotly({
        
        # filter the data for each state or area
        filteredstate <- unemployment%>% filter(`State and area` %in% input$area)
        
        #create a line chart
        f<- filteredstate %>% ggplot(mapping=aes(x=yearmon,y=`Unemployment rate`)) +
            geom_line(aes(group=`State and area`,color=`State and area`), size=0.8)
        
        
        f=  f+labs(x = "Time", y = "Unemployment Rate(%)",
                   title= " Unemployment Rate from Jan 1976 to May 2020")+
            scale_y_continuous(limits=c(0,35),breaks = c(0,5,10,15,20,25,30,35))+ 
            geom_vline(aes(xintercept = as.yearmon("Jan 2020")),color="black",linetype="dashed",alpha=0.5,size=0.4) +
            annotate("text", x =  as.yearmon("May 2020"), y = 33,
                     label = " COVID-19", size = 3.3) +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+ #remove grey background 
            theme(legend.position = "none")+ 
            theme(plot.title = element_text(size=16,face = "bold",hjust = 0.5))
        ggplotly(f)
        
    })
    
    output$relationship = renderPlotly({
        
        filtered <- monthly_plus_unemployment %>% filter(Month==input$month)
        
        if(input$casetype == "Confirmed"){
            p <- ggplot(filtered, mapping = aes(x =sum_confirmed, y = `Unemployment rate`))
        }else
            if(input$casetype == "Deaths"){
                p <- ggplot(filtered, mapping = aes(x =sum_deaths, y = `Unemployment rate`))
            }
        p = p + 
            geom_point(mapping = aes(size = `Civilian non-institutional population`)) +
            geom_smooth(method = "lm", se = FALSE) +
            labs(x = paste0("Number of ", input$casetype, " Cases(log10)"), y = "Unemployment Rate(%)",
                 title = "Relationship Between Unemployment Rate & COVID-19", 
                 size = "Population in thousands:") +
            scale_x_log10(labels = scales::comma)
        
        return(ggplotly(p))
        
    })
    
}

shinyApp(ui=ui,server = server)





















