library(shiny)
library(ggplot2)
library(gridExtra)
library(readxl)
# library(DT)
library(shinythemes)
# install.packages("shinythemes")



ui <- fluidPage(
  navbarPage(
    themeSelector(),
    headerPanel('101~104年度交換學生資料'),
    sidebarLayout(
      sidebarPanel(
        h2("資料來源："),
        p("大專校院本國學生出國進修交流(https://data.gov.tw/dataset/24730)")
      ),
      mainPanel(
        h1("106_3 CS+X 資料科學程式設計"),
        h3("->使用101~104年度交換學生資料進行分析", style = "font-family: 'times'; font-si16pt"),
        br(),
        br(),
        p("簡報連結：",
          span("https://drive.google.com/file/d/1JUU_cj1MKSeaYgux1vVqpKkWSz0TKVAA/view?usp=sharing", style = "color:blue"),
          "複製我")
      )
    ),
    tabPanel('raw data',
             titlePanel('原始資料'),
             sidebarPanel(
               conditionalPanel(
                 checkboxGroupInput('show_vars', 'Columns in exc_data to show:',
                                    names(exc_data), selected = names(exc_data))),
               
               mainPanel(
                 tabsetPanel(
                   id = 'dataset',
                   tabPanel("exc_data", dataTableOutput("table1")))
               ))
    ),
    
    
    tabPanel('Selected',
             titlePanel('可選擇變項'),
             fluidRow(
               column(4,
                      selectInput('year',
                                  'year',
                                  c('All',
                                    unique(as.character(exc_data$year)
                                    )))),
               
               column(4,
                      selectInput('dep',
                                  'dep',
                                  c('All',unique(as.character(exc_data$dep)
                                  )))),
               
               column(4,
                      selectInput('ori_uni',
                                  'original',
                                  c('All', unique(as.character(exc_data$ori_uni)
                                  ))))
               
             ),
             
             fluidRow(
               dataTableOutput('table2')
             )
             
    ),
    
    tabPanel('bar chart', 
             titlePanel('exchange country by each school'),
             sidebarPanel(
               selectInput('school', 'Select schools', as.character(unique(r1$school))
               )),
             mainPanel(
               plotOutput('plot1',width = 800, height = 600),
               img(src="taiwan.png", height = 400, width = 600),
               img(src="school.png", height = 400, width = 700)
             )
    ),
   
    tabPanel('gender',
             fluidPage(
               titlePanel("性別差異"),
               sidebarLayout(
                 sidebarPanel(),
                 mainPanel(
                   img(src="p1.png", height = 400, width = 700)
                 )
               )))
    
    
  ))


server <- function(input, output) {
  
  data <- reactive({r1[r1$school == input$school,]})
  
  output$plot1 <- renderPlot({
    
    
    g1 <- ggplot(data(), aes(x=year,y=China))
    g1 <- g1 + geom_col(binwidth = 2, fill="red", color = "black",alpha = .2)
    g1 <- g1 + labs(x = "China")
    g1 <- g1 + labs(y = "count")
    
    
    
    g2 <- ggplot(data(), aes(x=year,y=America))
    g2 <- g2 + geom_col(binwidth = 4, fill="blue", color = "black",alpha = .2)
    g2 <- g2 + labs(x = "America")
    g2 <- g2 + labs(y = "count")
    
    
    g3 <- ggplot(data(), aes(x=year,y=Japan))
    g3 <- g3 + geom_col(binwidth = .5, fill="yellow", color = "black",alpha = .2)
    g3 <- g3 + labs(x = "Japan")
    g3 <- g3 + labs(y = "count")
    
    g4 <- ggplot(data(), aes(x=year,y=Koera))
    g4 <- g4 + geom_col(binwidth = .5, fill="green", color = "black",alpha = .2)
    g4 <- g4 + labs(x = "Koera")
    g4 <- g4 + labs(y = "Frequency")
    
    
    #Plotting 4 graphs
    grid.arrange(g1,g2,g3,g4,nrow=2, ncol=2)
  })
  
  
  output$table1 <- renderDataTable({
    datatable(exc_data[, input$show_vars])
  })
  
  
  output$table2 <- renderDataTable({
    data <- exc_data
    if (input$year != "All") {
      data <- data[data$year == input$year,]
    }
    if (input$dep != "All") {
      data <- data[data$dep == input$dep,]
    }
    if (input$ori_uni != "All") {
      data <- data[data$ori_uni == input$ori_uni,]
    }
    data
  })
}



shinyApp(ui = ui, server = server)


