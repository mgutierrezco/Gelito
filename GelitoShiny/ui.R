library(shiny)

shinyUI(
  navbarPage(
    
    "Gelito (Gene Lists Tools)",
    
    tabPanel("Initializer",
             
             sidebarLayout(
               sidebarPanel(
                 strong("Choose ID column number (>0):"),
                 numericInput(inputId = "id", label = NA, 
                              value = 1, min = 1, width = "70px", step = 1)
               ),
               mainPanel(
                 h5("Results can be obtained following order tab."),
                 h5(":: Initialize -> Operations -> Venn Diagram / Subset"),
                 strong("Please, introduce all required input fields before next tab.")
               )
             ),
    
             sidebarLayout(
               sidebarPanel(
                 strong("Numbers of filter columns (comma separated):"),
                 textInput(inputId = "nf", label = NA, value = "", 
                           placeholder = "Columns list")
               ),
               mainPanel(
                 fluidPage(
                   uiOutput("set_criterions")
                 ),
                 mainPanel(
                   uiOutput("set_button")
                 )
               )
             ),
             
             sidebarLayout(
               sidebarPanel(
                 strong("Output column numbers (comma separated):"),
                 textInput(inputId = "cn", label = NA, value = "",
                           placeholder = "Column list - Required")
               ),
               mainPanel()
             ),
             
             sidebarLayout(
               sidebarPanel(
                 strong("Choose input files number (>0):"),
                 numericInput("ni", label = NA, value = 1, min = 1, width = "70px", step = 1)
               ),
               mainPanel(
                 fluidPage(
                   uiOutput("set_inputfiles")
                 )
               )
             ),
             
             sidebarLayout(
               sidebarPanel(
                 actionButton(inputId = "do_configure", 
                              label = "Configure", icon = icon("refresh")),
                 br(), 
                 br(),
                 p("Configure and initialize environment")
               ),
               mainPanel(
                 p("First of all, introduce left screen data, and click on Configure button.
                   After that, complete rest of fields on right screen"),
                 em("Then you can go Operations Tab"),
                 br(), br(),
                 code("If click on button Configure,"), br(),
                 code("right screen data (files and criterions) will be initialized.")
               )
             ),
             helpText("Please, after this Tab, go to Operations Tab ans stay there until process be finished.")
    ),
    
    tabPanel("Operations (Necessary)",
             wellPanel(
               selectInput("operation", "Choose operation for lists:",
                           choices = c("Intersect", "Union", "Common")),
               actionButton(inputId = "do_operation", 
                            label = "View operation"),
               downloadLink("downloadOperation", "Download"),
               radioButtons("format", "File format to download:",
                            inline = TRUE,
                            c("TAB" = "txt",
                              "CSV" = "csv",
                              "XLS" = "xls"))
             ),
             wellPanel(
               textOutput("o_id"),
               textOutput("o_nf"),
               textOutput("o_cc"),
               textOutput("o_cn"),
               textOutput("o_ll"),
               hr(),
               pre(uiOutput("set_operation"))
             , width = 10),
             helpText("Please, this is Pass Two (necessary). Wait until finish process.")
    ),
    
    tabPanel("Venn Diagram",
             wellPanel(
               plotOutput("set_diagram"),
               helpText("There are as sets as files uploaded")
             )
    ),
    
    tabPanel("Subset",
             wellPanel(
               fileInput("em", "Choose input file with matrix expression:",
                         accept = c('text/txt', 'text/plain', '.txt')),
               uiOutput("set_subset"),
               actionButton(inputId = "do_subset", 
                             label = "Execute subset"),
               textOutput("o_em"),
               #dataTableOutput("results"),
               tableOutput("results"),
               uiOutput("download"),
               helpText("Please, first upload expression matrix")
             )
    )

  )
)

