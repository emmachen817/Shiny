library(shiny)
library(shinyIncubator)
require(rCharts)
library(rHighcharts)
shinyUI(navbarPage(
  title = '功能選取',
  tabPanel('詞雲',
           titlePanel("詞雲展示"),
           sidebarLayout(
             sidebarPanel(
               fileInput('file1', 'Choose File',accept=c('text/txt','.txt')),
               sliderInput("freq", "字詞頻率控制:",min = 2, max = 10, value = 3),
               sliderInput("max","字詞展示個數:",min = 5, max = 25, value = 17),
               downloadButton('wf', 'DownloadWordFreq')
             ),
             mainPanel(plotOutput("plot"),chartOutput("baplot"))
             ,position = c("right"))
  ),
  tabPanel('集群',
           titlePanel("集群展示"),
           sidebarLayout(
             mainPanel(plotOutput("clplot", height = 1000, width = 1000)),
             sidebarPanel(
               sliderInput("pz", "集群字體大小",min = 0.5, max = 3, value = 1.23),
               numericInput("cn","集群群數",min=2,max=8,value=5),
               downloadButton('dc', 'Download')), fluid = TRUE)),
  tabPanel('ptt爬文',
           titlePanel("ptt詞雲展現"),
           checkboxInput(inputId = "individual_obs",
                         label = strong("求職版"),
                         value = FALSE),
           checkboxInput(inputId = "density",
                         label = strong("電影版"),
                         value = FALSE),
           sidebarPanel(
             sliderInput("freq1", "字詞頻率控制:",min = 10, max = 30, value = 20),
             sliderInput("max1","字詞展示個數:",min = 10, max = 50, value = 30)
           ),
           # Show a tabset that includes a plot, summary, and table view
           # of the generated distribution
           mainPanel(plotOutput("ptplot")), fluid = TRUE),
  tabPanel('ptt文字相關',
           titlePanel("ptt字詞相關展現"),
           textInput("dd", "輸入:"),
           dataTableOutput(outputId="table")), fluid = TRUE)
)




