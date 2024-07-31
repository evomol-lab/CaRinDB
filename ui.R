#if(!require(shinythemes, quietly=TRUE, warn.conflicts=FALSE)){ install.packages('shinythemes', quiet=TRUE) }
not_sel <- "Not Selected"
# ==== ui.R ===============================================================
ui <- fluidPage(theme = shinytheme("united"),
  tags$head(
    tags$style(
      HTML(
        "div#driver-popover-item {
          max-width: 700px;
          width: 700px;
          background-color: #E0FFFF;
          color: #191970;
        }
        div#driver-highlighted-element-stage, div#driver-page-overlay {
          background: transparent !important;
          outline: 5000px solid rgba(0, 0, 0, .75)
        }
        "
      )
    ),
   # includeHTML("www/google-analytics.html")
  ),
    # Application title
  titlePanel(
    windowTitle = "CaRinDB",
    title = tags$head(tags$link(rel="icon",
                                href=img_uri_favicon("icons/favicon.png"),
                                type="image/x-icon"))
  ),
  navbarPage(
    windowTitle = "CaRinDB",
    div(img(src="favicon.png", align="left", width="40px"), style="border: 0px; padding: 0px; margin: -10px 0 0 10px;" ), 
    id = "nav",
    position = "fixed-top",
    br(br()),
    # ==== Tab CaRinDB ===============================================================
    tabPanel(icon("home", lib = "glyphicon"), # Home
            fluidRow(
            wellPanel(p("CaRinDB is an integrated database of Cancer Mutations, Residue Interaction Networks and AlphaFold Protein Structure Database.")
                                    #br(),
                                    #icon("cog", lib = "glyphicon"), 
                                    #em( "Click on legends of plots to activate or deactivate labels."), br(),
                                    #br(),
                                    #actionButton("guide", " Run guided tour", icon = icon("info-sign", lib = "glyphicon"))
            ),
             div(
                 id = "plots",
                 div(
                   id = "plots_sec1",
                   #fluidRow(
                     # Row 1 ----
                     #sidebarPanel(
                     column(9,
                            shinycssloaders::withSpinner(plotlyOutput("fig.barTissue"), size = 1, type=1, color.background = "white")
                     ),
                     column(3,
                            shinycssloaders::withSpinner(plotlyOutput("fig.pieType_Mut"), size = 0.5, type=1, color.background = "white")
                     ),
                     # Row 2 ----
                   column(12,
                      wellPanel(
                      column(12,  
                      p("CaRinDB integrated to AlphaFold Protein Structure Database:")
                      ),
                      br(br())
                      )
                    ),
                   column(9,
                          shinycssloaders::withSpinner(plotlyOutput("fig.AF.barTissue"), size = 1, type=1, color.background = "white")
                   ),
                   column(3,
                          #shinycssloaders::withSpinner(plotlyOutput("fig.pieType_Mut"), size = 0.5, type=1, color.background = "white")
                   ),
                    column(12,
                      wellPanel(
                      column(3,  
                      ),
                      br(br())))
                     )
                )
            )
        ),
    # ==== Tab CaRinDB Variants ===============================================================
    tabPanel(
      " CaRinDB", 
      icon = icon("list-alt", lib = "glyphicon"),
       fluidRow(
           column(12, 
                  wellPanel(
                  p("Complete CaRinDB Variants with SNPs per Sampĺes.  ")
                  )
          )
       ),
      # Sidebar with a slider input for number of bins
      sidebarLayout(
        sidebarPanel(
          pickerInput(
            inputId = "select_tissues",
            label = "Select Tissues",
            choices = tissues,
            selected = tissues,
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 5"),
            multiple = TRUE
          ),
        conditionalPanel(
          'input.tab_carindb === "Summary"',
          # ==== BEGIN SubTab CaRinDB plots ===============================================================
          #p("Aqui"),
          # ==== END SubTab CaRinDB plots ===============================================================
        ),
        conditionalPanel(
          'input.tab_carindb === "Custom plot"',
          selectInput("num_var_1_DB", "x-axis", choices = c(not_sel)),
          selectInput("num_var_2_DB", "y-axis", choices = c(not_sel)),
          selectInput("fact_var_DB", "Factor Variable", choices = c(not_sel)),
          br(),
          actionButton("run_button_DB", "Run Analysis", icon = icon("play"))
        ),
        conditionalPanel(         
          'input.tab_carindb === "Dataset"',
            radioButtons("show_unique", 
                         "Show", 
                         choices = list("Unique samples" = "unique" , "All samples" = "all"),  
                         selected = c("all"),
                         inline = TRUE),
            pickerInput(
              inputId = "show_vars",
              label = "Select columns in CaRinDB", 
              choices = names(CaRinDB),
              selected = names(CaRinDB)[c(1:9)],
              options = list(
                `actions-box` = TRUE,
                `selected-text-format` = "count > 5",
                size = 10), 
              multiple = TRUE
            ),
          icon("cog", lib = "glyphicon"),                                           
          em( "Use ",
              a("regex", href="https://en.wikipedia.org/wiki/Regular_expression", target="_blank"),
              " to search in datatables."
          ),
          br(),
            verbatimTextOutput(outputId = "res")             
        ),
        width = 3 
        ),
        mainPanel(
          tabsetPanel(
            id = 'tab_carindb',
            tabPanel("Plots",
                     p(HTML(paste0("Plots of selected tissues: ", textOutput("res_tissues", inline = T), "."))),
                     # Row 1 ----                     
                     column(6,
                            shinycssloaders::withSpinner(plotlyOutput("fig.bar.NdamageTotMut"), size = 0.5, type=1, color.background = "white")
                     ),
                     column(6,
                            shinycssloaders::withSpinner(plotlyOutput("fig.barInterResTotMut"), size = 0.5, type=1, color.background = "white")
                     ),
                     # Row 2 ----
                     column(4,
                            shinycssloaders::withSpinner(plotlyOutput("fig.Betweenness"), size = 0.5, type=1, color.background = "white")
                     ),
                     column(4,
                            shinycssloaders::withSpinner(plotlyOutput("fig.bFactor"), size = 0.5, type=1, color.background = "white")
                     ),
                     column(4,
                            shinycssloaders::withSpinner(plotlyOutput("fig.bar.deleteriaTotMut"), size = 0.5, type=1, color.background = "white")
                     ),
                     column(12,
                            br(br()))
            ),
            tabPanel("Dataset",
                     downloadButton("downloadDB", "all data"),
                     shinycssloaders::withSpinner(DT::dataTableOutput("tb_CaRinDB"), size = 0.5, type=1, color.background = "white")
            ),
            tabPanel(
              title = "Custom plot",
              plotOutput("plot_DB")
            ),
            tabPanel("Summary",
                     p("Summary description of selected tissues."),
                     column(12,
                            shinycssloaders::withSpinner(htmlOutput("summary_CaRinDB"),
                                                         size = 0.5, type=1, 
                                                         color.background = "white")
                     )
            )
        ),
        width = 9
      )
    )
  ),
  
  # ==== Tab CaRinDB::AlphaFold Variants ===============================================================
  tabPanel(
    'CaRinDB::AlphaFold',
    icon = icon("list-alt", lib = "glyphicon"),
    fluidRow(
      column(12,
             wellPanel(
               p("Complete CaRinDB Variants integrated to AlphaFold Protein Structure Database.")
             )
      )
    ),
    # ===============================================================    
#       # Sidebar - CaRinAF
      sidebarLayout(
        sidebarPanel(
          pickerInput(
            inputId = "select_tissues_AF",
            label = "Select Tissues of CaRinDB::AlphaFold",
            choices = tissues_AF,
            selected = tissues_AF,
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 5"),
            multiple = TRUE
          ),
          conditionalPanel(
            'input.tab_carindbAF === "Summary"',
            #p("Summary sidebar.")
          ),
          conditionalPanel(
            'input.tab_carindbAF === "Custom plot"',
            selectInput("num_var_1_AF", "x-axis", choices = c(not_sel)),
            selectInput("num_var_2_AF", "y-axis", choices = c(not_sel)),
            selectInput("fact_var_AF", "Factor Variable", choices = c(not_sel)),
            br(),
            actionButton("run_button_AF", "Run Analysis", icon = icon("play"))
          ),
          conditionalPanel(
          'input.tab_carindbAF === "Dataset"',
            radioButtons("show_unique_AF",
                         "Show",
                         choices = list("Unique rows" = "unique" , "All rows" = "all"),
                         selected = c("all"),
                         inline = TRUE),
            pickerInput(
              inputId = "show_vars_AF",
              label = "Select columns in CaRinDB::AlphaFold",
              choices = names(CaRinAF),
              selected = names(CaRinAF)[c(1:9)],
              options = list(
                `actions-box` = TRUE,
                `selected-text-format` = "count > 5"),
              multiple = TRUE
            ),
            icon("cog", lib = "glyphicon"),
            em( "Use ",
                a("regex", href="misc/cheatsheets_regex.pdf", target="_blank"),
                " to search in datatables."
            ),
            br(),
            verbatimTextOutput(outputId = "res_AF")
          ),
        width = 3
          ),
        mainPanel(
          tabsetPanel(
            id = 'tab_carindbAF',
            tabPanel("Plots",
                     p(HTML(paste0("Plots of selected tissues: ", textOutput("res_tissues_AF", inline = T), "."))),
                     # Row 1 ----
                     column(6,
                            shinycssloaders::withSpinner(plotlyOutput("fig.AF.bar.NdamageTotMut"), size = 0.5, type=1, color.background = "white")
                     ),
                     column(6,
                            shinycssloaders::withSpinner(plotlyOutput("fig.AF.bar.InterResTotMut"), size = 0.5, type=1, color.background = "white")
                     ),
                     # Row 2 ----
                     column(4,
                            shinycssloaders::withSpinner(plotlyOutput("fig.AF.Betweenness"), size = 0.5, type=1, color.background = "white")
                     ),
                     column(4,
                            shinycssloaders::withSpinner(plotlyOutput("fig.AF.pLDDT"), size = 0.5, type=1, color.background = "white")
                     ),
                     column(4,
                            shinycssloaders::withSpinner(plotlyOutput("fig.AF.bar.deleteriaTotMut"), size = 0.5, type=1, color.background = "white")
                            
                     ),
                     column(12,
                            br(br()))
            ),
            tabPanel("Dataset",
                     downloadButton("downloadAF", "all data"),
                     shinycssloaders::withSpinner(DT::dataTableOutput("tb_CaRinAF"), size = 0.5, type=1, color.background = "white")
            ),
            tabPanel(
              title = "Custom plot",
              plotOutput("plot_AF")
            ),
            tabPanel("Summary",
                     # Row 3 ----                     
                     p("Summary description of selected tissues."),
                     column(12,
                            shinycssloaders::withSpinner(htmlOutput("summary_CaRinAF"),
                                                         size = 0.5, type=1, 
                                                         color.background = "white")
                     )
            ),
          ),
          width = 9
        )
      )
    )
  )
)

