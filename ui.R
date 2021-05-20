library(shiny)
library(dplyr)
library(shinycssloaders)
library(ggplot2)
library(lattice)
library(shinymaterial)
library(markdown)
shinyUI(fluidPage(
    
    title = "Advanced Text Analysis",
   # titlePanel("Advance Text Analysis"),
    titlePanel(title=div(img(src="logo.png",align='right'),"Advanced Text Analysis")),
    # Input in sidepanel:
    sidebarPanel(
        conditionalPanel(condition = "input.tabselected==1",
                         fileInput("file", "Upload data"),
                         fileInput("model","Upload model file"),
                         uiOutput('id_var'),
                         uiOutput("doc_var"),
                         
                         # materialSwitch(
                         #     inputId = "Id077",
                         #     label = "Lemmatization",
                         #     value = TRUE,
                         #     status = "primary"
                         # ),
                         strong(p("Data Preprocessing")),
                         htmlOutput("pre_proc1"),
                         htmlOutput("pre_proc2"),
                         htmlOutput("pre_proc3"),
                         htmlOutput('pos_select_ui'),
                         textInput('stopw',label = "Enter stopwords seperated by comma"),
                         
                         sliderInput('pos_slider',"Select top k words to display",min = 1,max = 100,value = 20,step = 1),
                         
                         ),
        conditionalPanel(condition = "input.tabselected==2",
                         uiOutput("doc_sel"),
                         uiOutput("sent_sel"),
                         sliderInput('size_sel',"Plot Size",min=1,max=10,value=2,step = 1)
                         ),
        #progressBar(id = "pb4", value = 50, display_pct = TRUE)
        conditionalPanel(condition = "input.tabselected==3",
                         selectInput("key_algo",'Select keyword extraction algorithm',choices = c("RAKE","Noun-Verb Phrase")),
                         sliderInput('key_slider',"Select top k keywords to display",min = 1,max = 100,value = 20,step = 1)
        )
        
        ),
    
    # Main Panel:
    mainPanel( 
        tabsetPanel(type = "tabs",
                    #
                    tabPanel("Overview & Example dataset",value=1,
                             h4(p("How to use this App")),
                             includeMarkdown("overview.md"),
                             br(),
                             br(),
                             hr(),
                             h4(p("Download Sample text file")),
                             downloadButton('downloadData1', 'Download Nokia Lumia reviews txt file'),br(),br(),
                             p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                             img(src = "example1.png")),
                            # tags$a(href="www.rstudio.com", "Click here!")
                    
                    
                              
                             
                    tabPanel("Summary Stats",value=1,
                             h4("Sentence level summary"),
                             htmlOutput("text"),
                             hr(),
                             h4("Token level summary"),
                             htmlOutput("text2"),
                             hr(),
                             h4("Wordcloud"),
                             plotOutput('wc')),
                    
                    tabPanel("POS TAG",value=1,
                             h4("Summary Table"), 
                             helpText("Note: Please wait annotation will take time"),
                             withSpinner(dataTableOutput(outputId = "a_table")),
                             withSpinner(plotOutput("pos_plot")),
                             dropdownButton(
                                 
                                 tags$h3("List of Inputs"),
                                 
                                 sliderInput(inputId = 'min_freq',
                                             label = 'Minimum Frequency',
                                             value = 3,
                                             min = 1,
                                             max = 10),
                                 
                                 sliderInput(inputId = 'max_word',
                                             label = 'Maximum Word',
                                             value = 20,
                                             min = 5,
                                             max = 100),
                                 
                                 circle = TRUE, status = "danger",
                                 icon = icon("gear"), width = "200px",
                                 
                                 tooltip = tooltipOptions(title = "Click to see inputs !")
                             ),
                             
                             plotOutput(outputId = 'word_cloud',width = "800",height = "400"),
                             h4("Download DTM of selected POS"),
                             #h3("-------------"),
                             verbatimTextOutput("dtm_text"),
                             downloadButton('download_dtm', 'Download DTM'),br(),
                    
                             ),
                    tabPanel("Document level Analysis",value=2,
                             h4("Selcted sentence"),
                             helpText("Note: Please wait annotation will take time"),
                             verbatimTextOutput('sel_sent1'),
                             h4("Sentence level dpendency tree"),
                             plotOutput("dep_tre",width = "100%"),
                             ),
                  
                    tabPanel("Keyword Extraction",value=3,
                             h4('Keyword Extraction'),
                             withSpinner(plotOutput('key_plot'))
                             
                    ),id="tabselected"
                    
        )
    )
)
)
