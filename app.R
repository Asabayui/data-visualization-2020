
#
#    http://shiny.rstudio.com/
#

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






ui <- dashboardPagePlus(
    header = dashboardHeaderPlus(title = "Coronavirus", 
                                 enable_rightsidebar = TRUE, rightSidebarIcon = "gears"),
    sidebar = dashboardSidebar(
        sidebarMenu(
            
            menuItem("Unemployment", tabName = "page1", icon = icon("line-chart"))
            
        )
    ),
    body = dashboardBody(
        tabItems(
            tabItem(tabName = "page1",
                    selectInput("area", label = h3("State and area"), 
                                choices = c(''),
                                multiple = TRUE,selected="Nevada"),
                    plotlyOutput("plot1")
            )
        )
    ),
    
    
    
    
    rightsidebar = rightSidebar(
        tags$a("Data Source",href="https://www.bls.gov/web/laus.supp.toc.htm",
               target="_blank")
        
    ),
    title = "DashboardPage"
)


server <- function(input, output, session) {
    unemployment<- read_csv("ststdsadata2.csv")
    
    
    #create a new date column shown as year and month 
    yearmon = NULL
    for (i in 1:length(unemployment$Year)){
        yearmon[i]= paste(unemployment$Year[i],unemployment$Month[i],sep='')
    }
    
    
    unemployment$yearmon = as.yearmon(yearmon, "%Y%m")
    
    #add a column to identify before or after coronavirus
   
    unemployment$corona<- ifelse((year(unemployment$yearmon)<2020),"Before ","After")
    
   
    
    updateSelectInput(session = session,
                      inputId = 'area',
                      choices = unique(unemployment$`State and area`),
                      selected = 'Nevada'
    )
    
    output$plot1 = renderPlotly({
        
        # filter the data for each state or area
        filteredstate <- unemployment%>% filter(`State and area` %in% input$area)
        
        #create a line chart
        f<- filteredstate %>% ggplot(mapping=aes(x=yearmon,y=`Unemployment rate`)) +
            geom_line(aes(group=`State and area`,color=`State and area`), size=0.8)
        
        
        f=  f+labs(x = "Time", y = "Unemployment Rate(%)",
                   title= " Unemployment Rate from Jan 1976 to May 2020")+
            scale_y_continuous(limits=c(0,35),breaks = c(0,5,10,15,20,25,30,35))+ 
            geom_vline(aes(xintercept = as.yearmon("Jan 2020")),color="black",linetype="dashed",alpha=0.5,size=0.4) +
            annotate("text", x =  as.yearmon("May 2020"), y = 20,
                     label = " COVID-19", size = 3.3) +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+ #remove grey background 
            theme(legend.position = "none")+ 
            theme(plot.title = element_text(size=16,face = "bold",hjust = 0.5))
        ggplotly(f)
        
        
        
    })
    
}

shinyApp(ui=ui,server = server)





















