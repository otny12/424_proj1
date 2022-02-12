#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(lubridate)

library(shinydashboard)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)


#read in datafile with forest park, ohare, and uic
stations <- read.delim(file = "fp_oh_uic.tsv", sep="\t", fileEncoding = "UTF-8")
#update dates to usable format
stations$newdate <- mdy(stations$date)

#prepare initial vis of 2001 to 2021 entires at uic station
years<-c(2001:2021)
uic <- subset(stations, stationname == "UIC-Halsted")
uic$year <-year(uic$newdate)
uic$month <-month(uic$newdate)
uic$daynames <- wday(uic$newdate, label=TRUE)

oh <- subset(stations, stationname == "O'Hare Airport")
oh$year <-year(oh$newdate)
oh$month <-month(oh$newdate)
oh$daynames <- wday(oh$newdate, label=TRUE)

fp <- subset(stations, stationname == "Forest Park")
fp$year <-year(fp$newdate)
fp$month <-month(fp$newdate)
fp$daynames <- wday(fp$newdate, label=TRUE)

# Define UI for application 
ui <- fluidPage(
  mainPanel(
    tabsetPanel(
      tabPanel("Charts",
  #tophalf shower
  fluidRow(
    column(width=10,
           box(solidHeader = TRUE, status = "primary", width=12,
               plotOutput("left_chart", height = 400)
            )
    ),column(2, DT::dataTableOutput("table"))
  ),#end fluidrow
  #Interactive elements top plot
  fluidRow(
    column(2,
           selectInput("select_station", h3(""), 
                       choices = c("UIC-Halsted"="uic1", "O'Hare Airport"="oh1", "Forest Park"="fp1"), selected = "uic1")),
    column(3,
           selectInput("select_year", h3(""), 
                       choices = years, selected = 2021)),
    column(7,
           radioButtons("radio",
                        h3(""),
                        choices = list("Yearly Bars" = "year",
                                       "Daily Bars" = "day",
                                       "Monthly Bars" = "month",
                                       "Day of Week Bars" = "week"
                                       ),
                        selected = "year", inline = TRUE)
    )
  )#end fluidrow
  
  ,hr()
  #bottom half shower
  ,  fluidRow(
      column(width=10,
           (
             box(solidHeader = TRUE, status = "primary", width = 12,
                 plotOutput("right_chart", height = 400)
             )
           )),column(2, DT::dataTableOutput("table1"))

),
  fluidRow(
    column(2,
           selectInput("select_station1", h3(""), 
                       choices = c("UIC-Halsted"="uic1", "O'Hare Airport"="oh1","Forest Park"="fp1"), selected = "uic1")),
    column(3,
           selectInput("select_year1", h3(""), 
                       choices = years, selected = 2021)),
    column(7,
           radioButtons("radio1",
                        h3(""),
                        choices = list("Yearly Bars" = "year",
                                       "Daily Bars" = "day",
                                       "Monthly Bars" = "month",
                                       "Day of Week Bars" = "week"
                        ),
                        selected = "year", inline = TRUE)
    )
  )
),
tabPanel("about", verbatimTextOutput("hello"))
)
)
)#end fluidpage


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  upper_plot <- reactive({
      switch (input$select_station,
        uic1 = uic,
        oh1 = oh,
        fp1 = fp
        )
    })
  
  #plot params
  plot1 <-reactive ({  #show barchart from 2001 to 2021 entries
      annual <-  aggregate(upper_plot()$rides, list(upper_plot()$year),sum)
      ggplot(annual, aes(x=Group.1, y=x)) + geom_bar(stat="identity", fill="steelblue") +
        labs(x="Year", y = "Ridership") +scale_x_discrete(name="Year",limits=years)

    })

  plot2 <- reactive({
    #show barchart for eachday in 2021
    y <-subset(upper_plot(), year == input$select_year )
    ggplot(y, aes(x=newdate, y=rides)) + geom_bar(stat="identity", fill="steelblue") +
      labs(x="Date", y = "Ridership")
  })
 
  plot3 <- reactive({
    y <-subset(upper_plot(), year == input$select_year )
    monthly <- aggregate(y$rides, list(y$month),sum) 
    #show barchart for each month of 2021
    ggplot(monthly, aes(x=Group.1, y=x)) +
      geom_bar(stat="identity", fill="steelblue") +
      labs(x="Month", y = "Ridership")
  })
  plot4 <- reactive({
    #show barchart of days of the week
    y <-subset(upper_plot(), year == input$select_year)
    weekdays <- aggregate(y$rides, list(y$daynames),sum) 
    ggplot(weekdays, aes(x=Group.1, y=x)) +
      geom_bar(stat="identity", fill="steelblue") +
      labs(x="Days of the Week", y="Ridership")
  })
  output$table <- DT::renderDataTable({
    tabdat <- switch(input$radio, 
           year = upper_plot()[, 6:5],
           day = subset(upper_plot(), year == input$select_year)[, 6:5],
           month= {
             y <-subset(upper_plot(), year == input$select_year )
             aggregate(y$rides, list(y$month),sum)
             }[,1:2],
           week={y <-subset(upper_plot(), year == input$select_year)
           aggregate(y$rides, list(y$daynames),sum)}[,1:2]
           )
    order <- 'desc'
    if(input$radio == "month")
      order <- 'asc'
    DT::datatable(tabdat,
      options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, order))),
      rownames=FALSE)
    })
  
  
  output$left_chart <- renderPlot({
    switch(input$radio, 
                   year = plot1(),
                   day = plot2(),
                   month=plot3(),
                   week=plot4())
  })
  
  
  #bottom text
  bottom_plot <- reactive({
    switch (input$select_station1,
            uic1 = uic,
            oh1 = oh,
            fp1 = fp
    )
  })
  plot5 <-reactive ({  #show barchart from 2001 to 2021 entries
    annual <- aggregate(bottom_plot()$rides, list(bottom_plot()$year),sum) 
    ggplot(annual, aes(x=Group.1, y=x)) + geom_bar(stat="identity", fill="steelblue") +
      labs(x="Year", y = "Ridership") + scale_x_discrete(name="Year",limits=years)
  })
  
  plot6 <- reactive({
    #show barchart for eachday in 2021
    y <-subset(bottom_plot(), year == input$select_year1)
    ggplot(y, aes(x=newdate, y=rides)) + geom_bar(stat="identity", fill="steelblue") +
      labs(x="Date", y = "Ridership")
  })
  
  plot7 <- reactive({
    y <-subset(bottom_plot(), year == input$select_year1)
    monthly <- aggregate(y$rides, list(y$month),sum) 
    #show barchart for each month of 2021
    ggplot(monthly, aes(x=Group.1, y=x)) +
      geom_bar(stat="identity", fill="steelblue") +
      labs(x="Month", y = "Ridership")
  })
  plot8 <- reactive({
    #show barchart of days of the week
    y <-subset(bottom_plot(), year == input$select_year1)
    weekdays <- aggregate(y$rides, list(y$daynames),sum) 
    ggplot(weekdays, aes(x=Group.1, y=x)) +
      geom_bar(stat="identity", fill="steelblue") +
      labs(x="Days of the Week", y="Ridership")
  })
  output$table1 <- DT::renderDataTable({
    tabdat <- switch(input$radio1, 
                     year = bottom_plot()[, 6:5],
                     day = subset(bottom_plot(), year == input$select_year1)[, 6:5],
                     month= {
                       y <-subset(bottom_plot(), year == input$select_year1 )
                       aggregate(y$rides, list(y$month),sum)
                     }[,1:2],
                     week={y <-subset(bottom_plot(), year == input$select_year1)
                     aggregate(y$rides, list(y$daynames),sum)}[,1:2]
    )
    order <- 'desc'
    if(input$radio1 == "month")
      order <- 'asc'
    DT::datatable(tabdat,
                  options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, order))),
                  rownames=FALSE)
  })
  
  output$right_chart <- renderPlot({
    switch(input$radio1, 
           year = plot5(),
           day = plot6(),
           month=plot7(),
           week=plot8())
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

