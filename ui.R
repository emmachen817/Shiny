library(shiny)
library(shinyIncubator)
require(rCharts)
library(rHighcharts)
shinyUI(navbarPage(
  title = '�\����',
  tabPanel('����',
           titlePanel("�����i��"),
           sidebarLayout(
             sidebarPanel(
               fileInput('file1', 'Choose File',accept=c('text/txt','.txt')),
               sliderInput("freq", "�r���W�v����:",min = 2, max = 10, value = 3),
               sliderInput("max","�r���i�ܭӼ�:",min = 5, max = 25, value = 17),
               downloadButton('wf', 'DownloadWordFreq')
             ),
             mainPanel(plotOutput("plot"),chartOutput("baplot"))
             ,position = c("right"))
  ),
  tabPanel('���s',
           titlePanel("���s�i��"),
           sidebarLayout(
             mainPanel(plotOutput("clplot", height = 1000, width = 1000)),
             sidebarPanel(
               sliderInput("pz", "���s�r��j�p",min = 0.5, max = 3, value = 1.23),
               numericInput("cn","���s�s��",min=2,max=8,value=5),
               downloadButton('dc', 'Download')), fluid = TRUE)),
  tabPanel('ptt����',
           titlePanel("ptt�����i�{"),
           checkboxInput(inputId = "individual_obs",
                         label = strong("�D¾��"),
                         value = FALSE),
           checkboxInput(inputId = "density",
                         label = strong("�q�v��"),
                         value = FALSE),
           sidebarPanel(
             sliderInput("freq1", "�r���W�v����:",min = 10, max = 30, value = 20),
             sliderInput("max1","�r���i�ܭӼ�:",min = 10, max = 50, value = 30)
           ),
           # Show a tabset that includes a plot, summary, and table view
           # of the generated distribution
           mainPanel(plotOutput("ptplot")), fluid = TRUE),
  tabPanel('ptt��r����',
           titlePanel("ptt�r�������i�{"),
           textInput("dd", "��J:"),
           dataTableOutput(outputId="table")), fluid = TRUE)
)



