#if(!require(shinythemes, quietly=TRUE, warn.conflicts=FALSE)){ install.packages('shinythemes', quiet=TRUE) }
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
    tabPanel('CaRinDB',
            fluidRow(
            wellPanel(p("CaRinDB: An integrated database of Cancer Mutations and Residue Interaction Networks.")
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
                      column(3,  
                      pickerInput(
                         inputId = "select_tissues",
                         label = "Select Tissues",
                         choices = tissues,
                         selected = tissues,
                         options = list(
                           `actions-box` = TRUE,
                           size = 8,
                           `selected-text-format` = "count > 5"),
                         multiple = TRUE
                       )
                      ),
                      br(br())))
                     ),
                     # Row 3 ----
                     column(6,
                            shinycssloaders::withSpinner(plotlyOutput("fig.bar.NdamageTotMut"), size = 0.5, type=1, color.background = "white")
                     ),
                     column(6,
                            shinycssloaders::withSpinner(plotlyOutput("fig.barInterResTotMut"), size = 0.5, type=1, color.background = "white")
                     ),
                     # Row 4 ----
                     column(6,
                            shinycssloaders::withSpinner(plotlyOutput("fig.Betweenness"), size = 0.5, type=1, color.background = "white")
                     ),
                     # Row 5 ----                 
                     #column(6,
                     #        shinycssloaders::withSpinner(plotlyOutput("fig.barDegMut"), size = 0.5, type=1, color.background = "white")
                     #),

                     #column(6,
                     #        shinycssloaders::withSpinner(plotlyOutput("fig.bar.ChangeTypeTotMut"), size = 0.5, type=1, color.background = "white")
                     #),
                     # Line 4 ---- 
                     #column(6,
                     #        shinycssloaders::withSpinner(plotlyOutput("fig.clusteringCoef"), size = 0.5, type=1, color.background = "white")
                     #),
                     #column(6,
                            #shinycssloaders::withSpinner(plotlyOutput("fig.typechangeProt"), size = 0.5, type=1, color.background = "white")
                            
                     #),
                     # Line 5 ---- 
                     column(6,
                            shinycssloaders::withSpinner(plotlyOutput("fig.bar.deleteriaTotMut"), size = 0.5, type=1, color.background = "white")
                            
                     ),
                     column(6,
                            shinycssloaders::withSpinner(plotlyOutput("fig.bFactor"), size = 0.5, type=1, color.background = "white")
                     )#,
                     # Line 6 ---- 
                     #
                     #column(12,
                     #        shinycssloaders::withSpinner(plotlyOutput("fig.Degree_RING"), size = 0.5, type=1, color.background = "white")
                     #)
                )
            )
        ),
    # ==== Tab CaRinDB Variants ===============================================================
    tabPanel(
      'CaRinDB Variants',
       fluidRow(
           column(12, 
                  wellPanel(
                  p("Complete CaRinDB with SNPs per Sampĺes.  "),
                  icon("cog", lib = "glyphicon"),                                           
                  em( "Use ",
                      a("regex", href="misc/cheatsheets_regex.pdf", target="_blank"), 
                      " to search in datatables."
                  ),
                  br()
                  )
          )
       ),
      # Sidebar with a slider input for number of bins
      sidebarLayout(
        div(
        id = "options_CaRinDB",
          sidebarPanel(
            radioButtons("show_unique", 
                         "Show", 
                         choices = list("Unique rows" = "unique" , "All rows" = "all"),  
                         selected = c("all"),
                         inline = TRUE),
            pickerInput(
              inputId = "show_tissues",
              label = "Select Tissues of CaRinDB:",
              choices = tissues,
              selected = tissues,
              options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = "count > 5"),
              multiple = TRUE
            ),pickerInput(
              inputId = "show_vars",
              label = "Select columns in CaRinDB:", 
              choices = names(CaRinDB),
              selected = names(CaRinDB)[c(1:6)],
              options = list(
                `actions-box` = TRUE,
                `selected-text-format` = "count > 5",
                size = 10), 
              multiple = TRUE
            ),
            verbatimTextOutput(outputId = "res"),
            width = 3
          )),
        mainPanel(
          shinycssloaders::withSpinner(DT::dataTableOutput("tb_CaRinDB"), size = 0.5, type=1, color.background = "white"),
         #  tabsetPanel(
         #      id = 'tab',
         #      tabPanel("CaRinDB",)
         #  ),
          width = 9
        )
      )
    ),
    # ==== Tab CaRinDB/AlphaFold Variants ===============================================================
      tabPanel(
        'CaRinDB/AlphaFold Variants',
        fluidRow(
          column(12,
                 wellPanel(
                   p("Complete CaRinDB/AlphaFold with SNPs per Sampĺes. "),
                   icon("cog", lib = "glyphicon"),
                   em( "Use ",
                       a("regex", href="misc/cheatsheets_regex.pdf", target="_blank"),
                       " to search in datatables."
                   ),
                   br()
                 )
          )
      ),
      # Sidebar - CaRinAF
      sidebarLayout(
        div(
          id = "options_CaRinAF",
          sidebarPanel(
            radioButtons("show_unique_AF",
                         "Show",
                         choices = list("Unique rows" = "unique" , "All rows" = "all"),
                         selected = c("all"),
                         inline = TRUE),
            pickerInput(
              inputId = "show_tissues_AF",
              label = "Select Tissues of CaRinDB/AlphaFold:",
              choices = tissues_AF,
              selected = tissues_AF,
              options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = "count > 5"),
              multiple = TRUE
            ),pickerInput(
              inputId = "show_vars_AF",
              label = "Select columns in CaRinDB/AlphaFold:",
              choices = names(CaRinAF),
              selected = names(CaRinAF)[c(1:6)],
              options = list(
                `actions-box` = TRUE,
                `selected-text-format` = "count > 5",
                size = 10),
              multiple = TRUE
            ),
            verbatimTextOutput(outputId = "res_AF"),
            width = 3
          )),
        mainPanel(
          shinycssloaders::withSpinner(DT::dataTableOutput("tb_CaRinAF"), size = 0.5, type=1, color.background = "white"),
          #  tabsetPanel(
          #      id = 'tab',
          #      tabPanel("CaRinDB",)
          #  ),
          width = 9
        )
      ) # --
    )
  )
)

