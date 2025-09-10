# if(!require(shinythemes, quietly=TRUE, warn.conflicts=FALSE)){ install.packages('shinythemes', quiet=TRUE) }
not_sel <- "Not Selected"
# ==== ui.R ===============================================================
ui <- fluidPage(
  use_cicerone(),
  theme = shinytheme("united"),
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
        .d-flex-buttons {
          display: flex;
          justify-content: center;
        }
        "
      )
    ),
    # includeHTML("www/google-analytics.html")
  ),
  # Application title
  titlePanel(
    windowTitle = "CaRinDB",
    title = tags$head(tags$link(
      rel = "icon",
      href = img_uri_favicon("icons/favicon.png"),
      type = "image/x-icon"
    ))
  ),
  navbarPage(
    windowTitle = "CaRinDB",
    div(img(src = "favicon.png", align = "left", width = "40px"), style = "border: 0px; padding: 0px; margin: -10px 0 0 10px;"),
    id = "nav",
    position = "fixed-top",
    # ==== Tab CaRinDB ===============================================================
    tabPanel(
      "Home",
      id = 'home',
      icon = icon("home", lib = "glyphicon"), # Home
      fluidRow(
        style = "padding: 3em 0;",
        div(
          id = "plots",
          column(
            12,
            wellPanel(
              style = "display: grid; place-items: center;text-align:center",
              fluidRow(
                style = "display: flex; place-items: center; margin: 1em 0;",
                img(src = "favicon.png", align = "left", width = "40px"),
                h1(style = "margin: 0 0.2em", "CaRinDB")
              ),
              fluidRow(
                style = "place-items: center;text-align:justify",
              p(
                strong("CaRinDB")," is an interactive database designed to streamline cancer mutation research by integrating data from The Cancer Genome Atlas (TCGA) and advanced structural analysis tools, along with advanced effect predictions and molecular features such as Residue Interaction Networks (RINs) derived from Protein Data Bank experimental structures and AlphaFoldDB computational models. Covering 33 distinct cancer types, CaRinDB offers a broad spectrum of insights into cancer mutation dynamics."
              ),
              p(
                "This platform allows users to extract, visualize, and interactively explore diverse mutations through an intuitive interface, and allows for evaluation of their structural impact."
              ),
              p(
                "CaRinDB provides a curated dataset featuring residue connectivity metrics, allele frequencies, references to biological databases, and functional predictions from 22 distinct tools, making it a valuable resource for AI/ML-based research. CaRinDB is well suited for training AI and machine learning models, enabling breakthroughs in understanding the molecular basis of cancer and its clinical implications, such as precision medicine and therapeutic target discovery."
              ),
              p(
                "Unlike existing tools, CaRinDB facilitates integration of polymorphism data with protein structural data and residue interaction networks, offering precision in mutation analysis."
              ),
              br(),
              actionButton("guide", " Run guided tour", icon = icon("info-sign", lib = "glyphicon"))
              ),              
              radioButtons(
                "db_source",
                "Database:",
                c("CaRinDB" = "CaRinDB",
                  "CaRinDB::AlphaFold" = "CaRinAF"),
                inline = TRUE
              ),
              shinycssloaders::withSpinner(
                uiOutput("gene_search"),
                size = 0.2,
                type = 1,
                color.background = "white"
              ),
              column(
                8,
                shinycssloaders::withSpinner(
                  uiOutput("fig.bar.gene_search.ui"),
                  size = 0.5,
                  type = 1,
                  color.background = "white"
                )
              ),
              column(
                12,
                style="margin-top: 1em",
                shinycssloaders::withSpinner(
                  DT::dataTableOutput("tb_gene_search"),
                  size = 0.5,
                  type = 1,
                  color.background = "white"
                )
              )
            )
          ),
          div(
            id = "plots_sec1",
            # fluidRow(
            # Row 1 ----
            # sidebarPanel(
            column(
              6,
              shinycssloaders::withSpinner(plotlyOutput("fig.barTissue"), size = 1, type = 1, color.background = "white")
            ),
            column(
              3,
              shinycssloaders::withSpinner(plotlyOutput("fig.pieType_am"), size = 0.5, type = 1, color.background = "white")
            ),
            column(
              3,
              shinycssloaders::withSpinner(plotlyOutput("fig.pieType_ndamage"), size = 0.5, type = 1, color.background = "white")
            ),
            # Row 2 ----
            column(
              12,
              wellPanel(
                column(
                  12,
                  p("CaRinDB integrated to AlphaFold Protein Structure Database:")
                ),
                br()
              )
            ),
            column(
              6,
              shinycssloaders::withSpinner(plotlyOutput("fig.AF.barTissue"), size = 1, type = 1, color.background = "white")
            ),
            column(
              3,
              shinycssloaders::withSpinner(plotlyOutput("fig.AF.pieType_am"), size = 0.5, type=1, color.background = "white")
            ),
            column(
              3,
              shinycssloaders::withSpinner(plotlyOutput("fig.AF.pieType_ndamage"), size = 0.5, type=1, color.background = "white")
            ),
            column(
              12,
            ),
          )
        )
      ),
    ),
    # ==== Tab CaRinDB Variants ===============================================================
    tabPanel(
      "CaRinDB",
      icon = icon("list-alt", lib = "glyphicon"),
      style = "padding: 3em 0;",
      fluidRow(
        column(
          12,
          wellPanel(
            p("Complete CaRinDB Variants with SNPs per Samples.  ")
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
              `selected-text-format` = "count > 5"
            ),
            multiple = TRUE
          ),
          conditionalPanel(
            'input.tab_carindb === "Summary"',
            # ==== BEGIN SubTab CaRinDB plots ===============================================================
            # p("Aqui"),
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
              choices = list("Unique samples" = "unique", "All samples" = "all"),
              selected = c("all"),
              inline = TRUE
            ),
            pickerInput(
              inputId = "show_vars",
              label = "Select columns in CaRinDB",
              choices = filtered_choices,
              selected = names(CaRinDB)[c(1:9)],
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                selectedTextFormat = "count > 5",
                size = 15,
                liveSearch = TRUE,
                title = "Click to select variables..."
              )
            ),
            icon("cog", lib = "glyphicon"),
            em(
              "Use ",
              a("regex", href = "https://en.wikipedia.org/wiki/Regular_expression", target = "_blank"),
              " to search in datatables."
            ),
            br(),
            verbatimTextOutput(outputId = "res")
          ),
          width = 3
        ),
        mainPanel(
          tabsetPanel(
            id = "tab_carindb",
            tabPanel(
              "Summary",
              p("Summary description of selected tissues.", id ="carindb_summary"),
              column(
                12,
                shinycssloaders::withSpinner(htmlOutput("summary_CaRinDB"),
                                             size = 0.5, type = 1,
                                             color.background = "white"
                )
              )
            ),
            tabPanel(
              "Plots",
              p(HTML(paste0("Plots of selected tissues: ", textOutput("res_tissues", inline = T), ".")), id ="carindb_plots"),
              # Row 1 ----
              column(
                6,
                shinycssloaders::withSpinner(plotlyOutput("fig.bar.NdamageTotMut"), size = 0.5, type = 1, color.background = "white")
              ),
              column(
                6,
                shinycssloaders::withSpinner(plotlyOutput("fig.barInterResTotMut"), size = 0.5, type = 1, color.background = "white")
              ),
              # Row 2 ----
              column(
                4,
                shinycssloaders::withSpinner(plotlyOutput("fig.Betweenness"), size = 0.5, type = 1, color.background = "white")
              ),
              column(
                4,
                shinycssloaders::withSpinner(plotlyOutput("fig.bFactor"), size = 0.5, type = 1, color.background = "white")
              ),
              column(
                4,
                shinycssloaders::withSpinner(plotlyOutput("fig.bar.deleteriaTotMut"), size = 0.5, type = 1, color.background = "white")
              ),
              column(
                12,
                br(br())
              )
            ),
            tabPanel(
              "Dataset",
              downloadButton("downloadDB", "all data"),
              shinycssloaders::withSpinner(DT::dataTableOutput("tb_CaRinDB"), size = 0.5, type = 1, color.background = "white")
            ),
            tabPanel(
              title = "Custom plot",
              plotOutput("plot_DB")
            )
          ),
          width = 9
        )
      )
    ),

    # ==== Tab CaRinDB::AlphaFold Variants ===============================================================
    tabPanel(
      "CaRinDB::AlphaFold",
      icon = icon("list-alt", lib = "glyphicon"),
      style = "padding: 3em 0;",
      fluidRow(
        column(
          12,
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
              `selected-text-format` = "count > 5"
            ),
            multiple = TRUE
          ),
          conditionalPanel(
            'input.tab_carindbAF === "Summary"',
            # p("Summary sidebar.")
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
              choices = list("Unique rows" = "unique", "All rows" = "all"),
              selected = c("all"),
              inline = TRUE
            ),
            pickerInput(
              inputId = "show_vars_AF",
              label = "Select columns in CaRinDB::AlphaFold",
              choices = filtered_choices_af,
              selected = names(CaRinAF)[c(1:9)],
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                selectedTextFormat = "count > 5",
                size = 15,
                liveSearch = TRUE,
                title = "Click to select variables..."
              )
            ),
            icon("cog", lib = "glyphicon"),
            em(
              "Use ",
              a("regex", href = "misc/cheatsheets_regex.pdf", target = "_blank"),
              " to search in datatables."
            ),
            br(),
            verbatimTextOutput(outputId = "res_AF")
          ),
          width = 3
        ),
        mainPanel(
          tabsetPanel(
            id = "tab_carindbAF",
            tabPanel(
              "Summary",
              # Row 3 ----
              p("Summary description of selected tissues."),
              column(
                12,
                shinycssloaders::withSpinner(htmlOutput("summary_CaRinAF"),
                                             size = 0.5, type = 1,
                                             color.background = "white"
                )
              )
            ),
            tabPanel(
              "Plots",
              p(HTML(paste0("Plots of selected tissues: ", textOutput("res_tissues_AF", inline = T), "."))),
              # Row 1 ----
              column(
                6,
                shinycssloaders::withSpinner(plotlyOutput("fig.AF.bar.NdamageTotMut"), size = 0.5, type = 1, color.background = "white")
              ),
              column(
                6,
                shinycssloaders::withSpinner(plotlyOutput("fig.AF.bar.InterResTotMut"), size = 0.5, type = 1, color.background = "white")
              ),
              # Row 2 ----
              column(
                4,
                shinycssloaders::withSpinner(plotlyOutput("fig.AF.Betweenness"), size = 0.5, type = 1, color.background = "white")
              ),
              column(
                4,
                shinycssloaders::withSpinner(plotlyOutput("fig.AF.pLDDT"), size = 0.5, type = 1, color.background = "white")
              ),
              column(
                4,
                shinycssloaders::withSpinner(plotlyOutput("fig.AF.bar.deleteriaTotMut"), size = 0.5, type = 1, color.background = "white")
              ),
              column(
                12,
                br(br())
              )
            ),
            tabPanel(
              "Dataset",
              downloadButton("downloadAF", "all data"),
              shinycssloaders::withSpinner(DT::dataTableOutput("tb_CaRinAF"), size = 0.5, type = 1, color.background = "white")
            ),
            tabPanel(
              title = "Custom plot",
              plotOutput("plot_AF")
            )
          ),
          width = 9
        )
      )
    ),
    # ==== Tab How To Use ===============================================================
    tabPanel(
      "How To Use",
      icon = icon("question-sign", lib = "glyphicon"),
      style = "padding: 3em 0;",
      fluidRow(
        column(
          12,
          wellPanel(
            style = "text-align: center;",
            h2("How to Use CaRinDB"),
            # fluidRow(
            #   style = "place-items: center;text-align:justify",
            #   HTML("<p>The first menu (<strong>Home</strong>) contains a general summary  of both DB and DB::AlphaFold datasets accessible through the portal.</p>
            #         <p>The main graph shows the number of mutations separated by type of cancer, and the pie chart shows the percentage of mutations classified according to the AlphaMissense class and the NDamage parameter, which represents the number of computational predictors that consider a particular mutation as deleterious.</p>
            #         <p>Users can also search the database using specific Gene Symbols.</p>
            #         <p>The second and third menus provide specific and interactive access to the <strong>CaRinDB</strong> and <strong>CaRinDB::AlphaFold</strong> datasets, respectively. Here, users can perform data mining and generate insights for their research. This entire analysis can be performed for all 33 tissues, or for a specific tissue.</p>
            #         <p>The <strong>Summary</strong> tab provides a summary description of all attributes (columns) of the dataset.</p>
            #         <p>Upon clicking the <strong>Plots</strong> tab, the user can visualize a set of graphs regarding some key database features. They are: the number of mutations versus N DamageCalc (Number of predictors that classified this mutation as damaging), the number of mutations versus Inter_Res_Tot (Total residue-residue interactions), the number of mutations versus Betweenness Weighted (RIN parameter), the number of mutations versus the B-factor (RIN parameter) or pLDDT (AlphaFold parameter) of the present residue, and the number of mutations versus the classes of deleterious mutations.</p>
            #         <p>The next tab (<strong>Dataset</strong>) is a web visualization of the table with the selected mutation data. Users can choose the fields/columns and types of cancer from the menu on the right. A small description of each item is shown next to each name. The table presents direct links with other information sources such as GeneCards, dbSNP, NCBI, Uniprot, and PDB/AlphaFoldDB. The panel on the left side allows users to filter on any of the provided columns using plain text and regular expressions.  Recovered results can be downloaded as CSV or PDF formatted files (all pages or current page only), through specific buttons.</p>
            #         <p>In the last tab (<strong>Custom plot</strong>), the user can explore the data in customized plots for specific or all types of cancer. The user can determine the plot's structure according to their parameters selection included in the x and y axes and the factor variable.</p>")
            #   ),
            p("Run our guided tour on Home  how to use our platform."),
            actionButton("guide", "Run guided tour", icon = icon("info-sign", lib = "glyphicon")),
            br(),br(),
            #p("Watch the video below for a simple explanation on the AlphaMissense predictions integrated to CaRinDB."),            
            #HTML('<iframe width="860" height="515" src="https://www.youtube.com/embed/fCd6B5HRaZ8?si=xxFB1T7pBQ7VBnSu" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>')
           )
        )
      ),
    ),
    # ==== Tab About ===============================================================
    tabPanel(
      "About",
      icon = icon("align-left", lib = "glyphicon"),
      style = "padding: 3em 0;",
      fluidRow(
        column(
          6,
          wellPanel(
            h4(HTML("<b>Data Sources</b>")),
            p("The construction of the databases available in", strong("CaRinDB"), "involved numerous public data repositories:",
            a(href = "https://portal.gdc.cancer.gov/repository", target = "_blank", "National Cancer Institute - GDC Data Portal"),
            ", missense mutations were annotated in ",
            a(href = "https://annovar.openbioinformatics.org/en/latest/user-guide/download/", target = "_blank", "ANNOVAR"), ", ",
            a(href = "https://pcingola.github.io/SnpEff/", target = "_blank", "SnpEFF"), ", ",
            a(href = "https://www.ncbi.nlm.nih.gov/", target = "_blank", "NCBI - National Center for Biotechnology Information"), ", ",
            a(href = "https://www.ncbi.nlm.nih.gov/clinvar/", target = "_blank", "ClinVar"), ", ",
            a(href = "https://www.uniprot.org/uploadlists", target = "_blank", "Uniprot"), ", and ",
            a(href = "https://www.rcsb.org/", target = "_blank", "PDB - Protein Data Bank"),".",
            "Residue interaction network data was obtained through the ",
            a(href = "https://ring.biocomputingup.it/submit", target = "_blank", "RING"), " program.",
            "3D protein structure predictions were also obtained from ",
            a(href = "https://alphafold.ebi.ac.uk/", target = "_blank", "Alphafold"), ".",
            "Predictions to verify the pathogenicity of mutations were also obtained from ",
            a(href = "https://alphamissense.hegelab.org/", target = "_blank", "AlphaMissense"), ".")
            #p("The construction of the databases available in **CaRinDB** involved numerous public data repositories: 
            #[National Cancer Institute - GDC Data Portal](https://portal.gdc.cancer.gov/repository), missense mutations were annotated in [ANNOVAR]
            #(https://annovar.openbioinformatics.org/en/latest/user-guide/download/), [SnpEFF](https://pcingola.github.io/SnpEff/), [NCBI - National Center for Biotechnology Information](https://www.ncbi.nlm.nih.gov/), [ClinVar](https://www.ncbi.nlm.nih.gov/clinvar/), [Uniprot](https://www.uniprot.org/uploadlists), [PDB - Protein Data Bank](https://www.rcsb.org/). Residue interaction network data was obtained through the [RING](https://ring.biocomputingup.it/submit) program, 3D protein structure predictions were also obtained from [Alphafold](https://alphafold.ebi.ac.uk/), and predictions to verify the pathogenicity of mutations were also obtained from [AlphaMissense](https://alphamissense.hegelab.org/).")
          )
        ),
        column(
          6,
          wellPanel(
            h4(HTML("<b>Citation</b>")),
            h5(HTML(
              "<i># If you have used CaRinDB for your research, please cite:</i>"
            )),
            
            p(strong("CaRinDB: An integrated database of common cancer mutations and residue interaction network parameters."), 
              "Daniela Coelho Batista Guedes Pereira", tags$sup("1,2"),",",
              "João Vitor Ferreira Cavalcante", tags$sup("1"),",", 
              "Raul Maia Falcão",tags$sup("1"),",",
              "Jorge Estefano Santana de Souza",tags$sup("1"),",",
              "Rodrigo Juliani Siqueira Dalmolin", tags$sup("1"),",",
              "Gustavo Antônio de Souza", tags$sup("1"),",",
              "Thaís Gaudencio do Rêgo", tags$sup("1,2"),",",
              "Patrick Terrematte", tags$sup("1,2"),",",
              "and João Paulo Matos Santos Lima",tags$sup("1,3"),",",
              strong("To be published.")),
            h5(strong("Affiliations")),
            HTML(
              paste0(
                tags$sup("1"),
                " Bioinformatics Multidisciplinary Environment (BioME),  Digital Metropolis Institute (IMD), Federal University of Rio Grande do Norte (UFRN), Brazil."
              )
            ),
            br(),
            HTML(paste0(
              tags$sup("2"), " Centro de Informática, Universidade Federal da Paraíba (UFPB), João Pessoa, PB, Brazil."
            )),
            br(),
            HTML(paste0(
              tags$sup("3"), " Institute of Tropical Medicine (IMT), UFRN, Natal, RN, Brazil"
            )),
            br(),
            br(),
            h4(HTML("<b>Contact</b>")),
            p("The CaRinDB team is available to assist users who want to import their data on demand. If you have some question, feedback, or request, contact the ",
              "Corresponding author:", br(),
              "João Paulo Matos Santos Lima (jpmslima [at] imd.ufrn.br)"
              ),
          )
        )
      ),
      fluidRow(
        column(
          4,
          wellPanel(
            h4(HTML("<b>License</b>")),
            p("Data contained within the CaRinDB Database is provided for non-commercial research.")
          )
        ),
        column(
          8,
          wellPanel(
            h4(HTML("<b>Data Dictionary</b>")),
            DT::dataTableOutput("dt_data_dict"),
          )
        )
      )
    )
  ),
  div("BioME/IMD/UFRN, EvoMol-Lab, The authors would like to thank CAPES.",
      style = "position: fixed;left: 0;bottom: 0;width: 100%;background-color: #e95420;color: white;text-align: center;padding:0.5em 0;")
)
