# ==== server.R ===============================================================
server <- function(input, output, session) {
  # suppress warnings
  storeWarn <- getOption("warn")
  options(warn = -1)

  # B - Buttons Bfrtip
  # l - Length changing input control
  # f - Filtering input
  # r - pRocessing display element
  # t - Table
  # i - Table information summary
  # p - Pagination control
  # General options for all tables ----
  list.options <- list(
    # pageLength = 10,
    lengthMenu = list(
      c(10, 25, 50, 100, -1),
      c("10", "25", "50", "100", "all")
    ),
    paging = T,
    search = list(regex = TRUE),
    searchHighlight = TRUE,
    colReorder = TRUE,
    orientation = "landscape", # <'col-md-2''dwnld'>
    dom = "<'row'<'col-md-3'l><'col-md-6'B><'col-md-3'f>><'row'<'col-md-12't>><'row'<'col-md-3'i><'col-md-1'><'col-md-8'p>>",
    # dom = 'lBfrtip',
    buttons =
      list(
        list(
          extend = "pdf",
          text = img_uri_icon("icons/pdf_icon.png"),
          pageSize = "A4",
          orientation = "landscape",
          filename = "CaRinDB"
        ),
        list(
          extend = "csv",
          text = '<span class="glyphicon glyphicon-download-alt"></span> Current Page (csv)',
          filename = "CaRinDB_page",
          exportOptions = list(
            modifier = list(page = "current")
          )
        )
        # ,
        # list(extend = "csv",
        #     text = '<span class="glyphicon glyphicon-download-alt"></span> All Pages+ (csv)',
        #     filename = "CaRinDB_all",
        #     exportOptions = list(
        #       modifier = list(page = 'all')
        #     )
        # )
      )
  )

  output$downloadDB <- downloadHandler(
    filename = function() {
      paste("CaRinDB-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(CaRinDB, file)
    }
  )

  output$downloadAF <- downloadHandler(
    filename = function() {
      paste("CaRinAlphaFold-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(CaRinAF, file)
    }
  )


  CaRinDB_Tissue <- count(CaRinDB, Tissue) %>%
    arrange(desc(n))

  CaRinDB_Tissue$Tissue <- factor(CaRinDB_Tissue$Tissue, levels = CaRinDB_Tissue$Tissue)

  CaRinDB_am_class <- count(CaRinDB, am_class) %>%
    mutate(am_class = as.factor(am_class))

  CaRinDB_Ndamage <- count(CaRinDB, Ndamage) %>%
    mutate(Ndamage = as.factor(Ndamage))

  output$tb_gene_search <- DT::renderDataTable(
    {
      # w$show()
      DT::datatable(
        CaRinDB[CaRinDB$Gene_EFF == "KDM1A", names(CaRinDB)[c(1:9)]],
        class = "cell-border stripe",
        rownames = FALSE,
        filter = "none",
        options = list(dom = '<"d-flex-buttons"B>t',
        # dom = 'lBfrtip',
        buttons =
          list(
            list(
              extend = "pdf",
              text = img_uri_icon("icons/pdf_icon.png"),
              pageSize = "A4",
              orientation = "landscape",
              filename = "CaRinDB"
            ),
            list(
              extend = "csv",
              text = '<span class="glyphicon glyphicon-download-alt"></span> Current Page (csv)',
              filename = "CaRinDB_page",
              exportOptions = list(
                modifier = list(page = "current")
              )
            )
          )),
        extensions = c("Buttons", "ColReorder"),
        escape = FALSE
      )
    },
    server = TRUE
  )

  # Bar plot of Samples by Tissue ----
  output$fig.barTissue <- renderPlotly({
    plot_ly(
      data = CaRinDB_Tissue, x = ~Tissue, y = ~n, type = "bar", labels = ~Tissue,
      text = ~n, textposition = "outside", sort = T, text_auto = ".2s", textangle = 0, textfont.size = 8
    ) %>%
      layout(
        showlegend = F, yaxis = list(title = "#Samples by Cancer Type"),
        xaxis = list(title = "Cancer_Type", tickangle = -45),
        font = list(size = 10),
        title = "CaRinDB with 33 tissues"
      )
  })

  # Pie chart of Am_class SNPs  ----
  output$fig.pieType_am <- renderPlotly({
    plot_ly() %>%
      add_pie(
        data = CaRinDB_am_class, labels = ~am_class, values = ~n,
        hole = 0, name = "am_class", textinfo = "label+value", insidetextorientation = "radial", sort = FALSE
      ) %>%
      layout(
        title = "AM Class", showlegend = T,
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        font = list(size = 10)
      )
  })

  # Pie chart of Ndamage SNPs  ----
  output$fig.pieType_ndamage <- renderPlotly({
    plot_ly() %>%
      add_pie(
        data = CaRinDB_Ndamage, labels = ~Ndamage, values = ~n,
        hole = 0, name = "Ndamage", textinfo = "label+value", insidetextorientation = "radial", sort = FALSE
      ) %>%
      layout(
        title = "Ndamage", showlegend = T,
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        font = list(size = 10)
      )
  })

  #------------------------------------------
  # Ndamage vs. Mutation number OK ----
  output$fig.bar.NdamageTotMut <- renderPlotly({
    plot_ly(
      data = count(CaRinDB[CaRinDB$Tissue %in% input$select_tissues, ], NdamageCalc), x = ~NdamageCalc, y = ~n, type = "bar",
      text = ~n, textposition = "outside"
    ) %>%
      layout(
        showlegend = F,
        yaxis = list(title = "# Mutations"),
        xaxis = list(title = "# N DamageCalc", tickangle = -45),
        font = list(size = 10)
      )
  })

  # Inter_Res_tot vs. Mutation number OK ----
  output$fig.barInterResTotMut <- renderPlotly({
    plot_ly(
      data = count(CaRinDB[CaRinDB$Tissue %in% input$select_tissues, ], Inter_Res_tot), x = ~Inter_Res_tot, y = ~n, type = "bar",
      text = ~n, textposition = "outside", sort = T, text_auto = ".2s", textangle = 0, textfont.size = 8
    ) %>%
      layout(
        showlegend = F,
        yaxis = list(title = "# Mutations"),
        xaxis = list(title = "# Inter_Res_tot", tickangle = -45),
        font = list(size = 10)
      )
  })

  # - betweenness vs. Frequency OK ----
  output$fig.Betweenness <- renderPlotly({
    plot_ly(x = CaRinDB$betweennessWeighted_node[CaRinDB$Tissue %in% input$select_tissues], type = "histogram") %>%
      layout(
        showlegend = F,
        # title = 'Betweenness',
        yaxis = list(title = "# Mutations"),
        xaxis = list(title = "# Betweenness Weighted"),
        updatemenus = list(list(
          y = 1.1,
          active = 0,
          buttons = list(
            list(
              label = "Linear",
              method = "update",
              args = list(list(visible = c(T)), list( # xaxis = list(type = 'linear'),
                yaxis = list(type = "linear")
              ))
            ),
            list(
              label = "Log",
              method = "update",
              args = list(list(visible = c(T)), list(yaxis = list(type = "log")))
            )
          )
        ))
      )
  })

  # - b-factor vs. Mutation number OK ----
  output$fig.bFactor <- renderPlotly({
    plot_ly(x = CaRinDB$Bfactor_CA_RING[CaRinDB$Tissue %in% input$select_tissues], type = "histogram") %>%
      layout(
        showlegend = F,
        yaxis = list(title = "# Mutations"),
        xaxis = list(title = "# B-factor"),
        updatemenus = list(list(
          y = 1.1,
          active = 0,
          buttons = list(
            list(
              label = "Linear",
              method = "update",
              args = list(list(visible = c(T)), list(yaxis = list(type = "linear")))
            ),
            list(
              label = "Log",
              method = "update",
              args = list(list(visible = c(T)), list(yaxis = list(type = "log")))
            )
          )
        ))
      )
  })

  # Deleteria5/10/20 vs. Mutation number OK ----
  output$fig.bar.deleteriaTotMut <- renderPlotly({
    plot_ly(
      data = count(CaRinDB[CaRinDB$Tissue %in% input$select_tissues, ], Deleteria, Deleteria5, Deleteria10, Deleteria11), x = list("Non Deleteria", "Deleteria", "Deleteria5", "Deleteria10", "Deleteria11"), y = ~n, type = "bar",
      text = ~n, textposition = "outside"
    ) %>%
      layout(
        showlegend = F,
        yaxis = list(title = "# Mutations"),
        xaxis = list(title = "# Deleteria"), # , tickangle = -45
        font = list(size = 10)
      )
  })

  # # Degree (x) vs. Mutation number ok ----
  # output$fig.barDegMut <- renderPlotly({
  #   plot_ly(data = count(CaRinDB[CaRinDB$Tissue %in%  input$select_tissues, ], Degree_RING), x = ~Degree_RING, y = ~n, type = 'bar',
  #           text = ~n, textposition="outside", sort = T, text_auto='.2s', textangle=0, textfont.size=8
  #   ) %>%
  #     layout(showlegend = F,
  #            yaxis = list(title = "# Mutations"),
  #            xaxis = list(title = "# Degree RING"),
  #            updatemenus = list(list(
  #              y = 1.1,
  #              active = 0,
  #              buttons= list(
  #                list(label = 'Linear',
  #                     method = 'update',
  #                     args = list(list(visible = c(T)), list(xaxis = list(type = 'linear'),
  #                                                            yaxis = list(type = 'linear')))),
  #                list(label = 'Log',
  #                     method = 'update',
  #                     args = list(list(visible = c(T)), list(xaxis = list(type = 'log'),
  #                                                            yaxis = list(type = 'log')
  #                     ))))))
  #     )
  # })
  #
  # output$fig.Degree_RING <- renderPlotly({
  #   plot_ly(data = CaRinDB[CaRinDB$Tissue %in%  input$select_tissues, ], x = ~Degree_RING, y = ~dbNSFP_gnomAD_exomes_AF, type = "scatter") %>%
  #     layout(showlegend = F,
  #            yaxis = list(title = '# dbNSFP_gnomAD_exomes_AF'),
  #            xaxis = list(title = '# Degree RING'),
  #            updatemenus = list(list(
  #              y = 1.1,
  #              active = 0,
  #              buttons= list(
  #                list(label = 'Linear',
  #                     method = 'update',
  #                     args = list(list(visible = c(T)), list(xaxis = list(type = 'linear'),
  #                                                            yaxis = list(type = 'linear')
  #                     ))),
  #                list(label = 'Log',
  #                     method = 'update',
  #                     args = list(list(visible = c(T)), list(xaxis = list(type = 'log')#,
  #                                                            #yaxis = list(type = 'log')
  #                     ))))))
  #     )
  # })
  #
  # # - coefclustering vs. Mutation number ----
  # output$fig.clusteringCoef <- renderPlotly({
  #   plot_ly(x = CaRinDB$clusteringCoef_node[CaRinDB$Tissue %in%  input$select_tissues],  type = "histogram") %>%
  #     layout(showlegend = F,
  #            yaxis = list(title = '# Mutations'),
  #            xaxis = list(title = "# Clustering Coeficient"))
  # })
  #
  #
  # # ChangeType vs. Mutation number ----
  # output$fig.bar.ChangeTypeTotMut <- renderPlotly({
  #   plot_ly(data = count(CaRinDB[CaRinDB$Tissue %in%  input$select_tissues, ], typechangeProt), x = ~typechangeProt, #list("Del", "Subst", "Translation\n Termination"),
  #           y = ~n, type = 'bar',
  #           text = ~n, textposition="outside"
  #   ) %>%
  #     layout(showlegend = F,
  #            yaxis = list(title = '# Mutations'),
  #            xaxis = list(title = "# Change Type"), # , tickangle = -45
  #            font  = list(size = 10)
  #     )
  # })

  # CaRinDB ----
  output$tb_CaRinDB <- DT::renderDataTable(
    {
      # w$show()
      DT::datatable(
        if (input$show_unique == "unique") {
          CaRinDB[CaRinDB$Tissue %in% input$select_tissues & !duplicated(CaRinDB[CaRinDB$Tissue %in% input$select_tissues, input$show_vars]), input$show_vars, drop = FALSE]
        } else {
          CaRinDB[CaRinDB$Tissue %in% input$select_tissues, input$show_vars, drop = FALSE]
        },
        class = "cell-border stripe",
        rownames = FALSE,
        filter = "top",
        extensions = c("Buttons", "ColReorder"),
        options = list.options,
        escape = FALSE
      )
    },
    server = TRUE
  )

  # CaRinDB::AlphaFold PLOTS----
  CaRinAF_Tissue <- count(CaRinAF, Tissue) %>%
    arrange(desc(n))

  CaRinAF_Tissue$Tissue <- factor(CaRinAF_Tissue$Tissue, levels = CaRinAF_Tissue$Tissue)

  CaRinAF_am_class <- count(CaRinAF, am_class) %>%
    mutate(am_class = as.factor(am_class))

  CaRinAF_Ndamage <- count(CaRinAF, Ndamage) %>%
    mutate(Ndamage = as.factor(Ndamage))

  # Bar plot of Samples by Tissue ----
  output$fig.AF.barTissue <- renderPlotly({
    plot_ly(
      data = CaRinAF_Tissue, x = ~Tissue, y = ~n, type = "bar", labels = ~Tissue,
      text = ~n, textposition = "outside", sort = T, text_auto = ".2s", textangle = 0, textfont.size = 8
    ) %>%
      layout(
        showlegend = F, yaxis = list(title = "#Samples by Cancer Type"),
        xaxis = list(title = "Cancer_Type", tickangle = -45),
        font = list(size = 10),
        title = "CaRinDB::AlphaFold with 33 tissues"
      )
  })

  # Pie chart of AF Am_class SNPs  ----
  output$fig.AF.pieType_am <- renderPlotly({
    plot_ly() %>%
      add_pie(
        data = CaRinAF_am_class, labels = ~am_class, values = ~n,
        hole = 0, name = "am_class", textinfo = "label+value", insidetextorientation = "radial", sort = FALSE
      ) %>%
      layout(
        title = "AM Class", showlegend = T,
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        font = list(size = 10)
      )
  })

  # Pie chart of Ndamage SNPs  ----
  output$fig.AF.pieType_ndamage <- renderPlotly({
    plot_ly() %>%
      add_pie(
        data = CaRinAF_Ndamage, labels = ~Ndamage, values = ~n,
        hole = 0, name = "Ndamage", textinfo = "label+value", insidetextorientation = "radial", sort = FALSE
      ) %>%
      layout(
        title = "Ndamage", showlegend = T,
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        font = list(size = 10)
      )
  })

  # Pie chart of Am_class SNPs  ----
  output$fig.ApieType_am <- renderPlotly({
    plot_ly() %>%
      add_pie(
        data = CaRinDB_am_class, labels = ~am_class, values = ~n,
        hole = 0, name = "am_class", textinfo = "label+value", insidetextorientation = "radial", sort = FALSE
      ) %>%
      layout(
        title = "AM Class", showlegend = T,
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        font = list(size = 10)
      )
  })

  # Pie chart of Ndamage SNPs  ----
  output$fig.pieType_ndamage <- renderPlotly({
    plot_ly() %>%
      add_pie(
        data = CaRinDB_Ndamage, labels = ~Ndamage, values = ~n,
        hole = 0, name = "Ndamage", textinfo = "label+value", insidetextorientation = "radial", sort = FALSE
      ) %>%
      layout(
        title = "Ndamage", showlegend = T,
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        font = list(size = 10)
      )
  })

  #------------------------------------------
  # Ndamage vs. Mutation number OK ----
  output$fig.AF.bar.NdamageTotMut <- renderPlotly({
    plot_ly(
      data = count(CaRinAF[CaRinAF$Tissue %in% input$select_tissues_AF, ], NdamageCalc), x = ~NdamageCalc, y = ~n, type = "bar",
      text = ~n, textposition = "outside"
    ) %>%
      layout(
        showlegend = F,
        yaxis = list(title = "# Mutations"),
        xaxis = list(title = "# N DamageCalc", tickangle = -45),
        font = list(size = 10)
      )
  })

  # Inter_Res_tot vs. Mutation number OK ----
  output$fig.AF.bar.InterResTotMut <- renderPlotly({
    plot_ly(
      data = count(CaRinAF[CaRinAF$Tissue %in% input$select_tissues_AF, ], Inter_Res_tot), x = ~Inter_Res_tot, y = ~n, type = "bar",
      text = ~n, textposition = "outside", sort = T, text_auto = ".2s", textangle = 0, textfont.size = 8
    ) %>%
      layout(
        showlegend = F,
        yaxis = list(title = "# Mutations"),
        xaxis = list(title = "# Inter_Res_tot", tickangle = -45),
        font = list(size = 10)
      )
  })

  # - betweenness vs. Frequency OK ----
  output$fig.AF.Betweenness <- renderPlotly({
    plot_ly(x = CaRinAF$betweennessWeighted_node[CaRinAF$Tissue %in% input$select_tissues_AF], type = "histogram") %>%
      layout(
        showlegend = F,
        # title = 'Betweenness',
        yaxis = list(title = "# Mutations"),
        xaxis = list(title = "# Betweenness Weighted"),
        updatemenus = list(list(
          y = 1.1,
          active = 0,
          buttons = list(
            list(
              label = "Linear",
              method = "update",
              args = list(list(visible = c(T)), list( # xaxis = list(type = 'linear'),
                yaxis = list(type = "linear")
              ))
            ),
            list(
              label = "Log",
              method = "update",
              args = list(list(visible = c(T)), list(yaxis = list(type = "log")))
            )
          )
        ))
      )
  })

  # - b-factor vs. Mutation number OK ----
  output$fig.AF.pLDDT <- renderPlotly({
    plot_ly(x = CaRinAF$pLDDT_RING[CaRinAF$Tissue %in% input$select_tissues_AF], type = "histogram") %>%
      layout(
        showlegend = F,
        yaxis = list(title = "# Mutations"),
        xaxis = list(title = "# pLDDT"),
        updatemenus = list(list(
          y = 1.1,
          active = 0,
          buttons = list(
            list(
              label = "Linear",
              method = "update",
              args = list(list(visible = c(T)), list(yaxis = list(type = "linear")))
            ),
            list(
              label = "Log",
              method = "update",
              args = list(list(visible = c(T)), list(yaxis = list(type = "log")))
            )
          )
        ))
      )
  })

  # Deleteria5/10/20 vs. Mutation number OK ----
  output$fig.AF.bar.deleteriaTotMut <- renderPlotly({
    plot_ly(
      data = count(CaRinAF[CaRinAF$Tissue %in% input$select_tissues_AF, ], Deleteria, Deleteria5, Deleteria10, Deleteria11), x = list("Non Deleteria", "Deleteria", "Deleteria5", "Deleteria10", "Deleteria11"), y = ~n, type = "bar",
      text = ~n, textposition = "outside"
    ) %>%
      layout(
        showlegend = F,
        yaxis = list(title = "# Mutations"),
        xaxis = list(title = "# Deleteria"), # , tickangle = -45
        font = list(size = 10)
      )
  })

  # CaRinDB::AlphaFold DataTable----
  output$tb_CaRinAF <- DT::renderDataTable(
    {
      # w$show()
      DT::datatable(
        if (input$show_unique_AF == "unique") {
          CaRinAF[CaRinAF$Tissue %in% input$select_tissues_AF & !duplicated(CaRinAF[CaRinAF$Tissue %in% input$select_tissues_AF, input$show_vars_AF]), input$show_vars_AF, drop = FALSE]
        } else {
          CaRinAF[CaRinAF$Tissue %in% input$select_tissues_AF, input$show_vars_AF, drop = FALSE]
        },
        class = "cell-border stripe",
        rownames = FALSE,
        filter = "top",
        extensions = c("Buttons", "ColReorder"),
        options = list.options,
        escape = FALSE
      )
    },
    server = TRUE
  )

  output$res_tissues <- renderText({
    paste0(input$select_tissues, collapse = ", ")
  })

  output$res_tissues_AF <- renderText({
    paste0(input$select_tissues_AF, collapse = ", ")
  })


  # output$CaRinDBSummary <- renderUI({
  #
  #   CaRinDBSumProfile <- summarytools::view(summarytools::dfSummary(CaRinDB, style="grid", method = "render"),
  #                                             #varnumbers   = FALSE,
  #                                             #valid.col    = FALSE,
  #                                             omit.headings = TRUE,
  #                                             bootstrap.css = FALSE,
  #                                             escape.pipe = TRUE,
  #                                             file = "./www/CaRinDB.html"
  #   )
  #   CaRinDBSumProfile
  # })
  #
  # getPageDB<-function() {
  #   return(includeHTML("./CaRinDB.html"))
  # }
  #
  # output$incDB<-renderUI({
  #   getPageDB()
  # })
  output$summary_CaRinDB <- renderUI({
    print(
      summarytools::dfSummary(
        dplyr::select(CaRinDB[CaRinDB$Tissue %in% input$select_tissues, grep("_search", # Remove db references
                                                                             colnames(CaRinDB),
                                                                             value = T,
                                                                             invert = T)], c(-3, -5, -7)),
        graph.magnif = 0.8,
        na.col = FALSE,
        footnote = NA
      ),
      method = "render",
      headings = FALSE,
      bootstrap.css = FALSE
    )
  })

  output$summary_CaRinAF <- renderUI({
    print(
      summarytools::dfSummary(
        dplyr::select(CaRinAF[CaRinAF$Tissue %in% input$select_tissues_AF, grep("_search",  # Remove db references
                                                                                colnames(CaRinAF),
                                                                                value = T,
                                                                                invert = T)], c(-3, -5, -7)),
        graph.magnif = 0.8,
        na.col = FALSE,
        footnote = NA
      ),
      method = "render",
      headings = FALSE,
      bootstrap.css = FALSE
    )
  })

  not_sel <- "Not Selected"

  draw_plot <- function(data_input, num_var_1, num_var_2, fact_var) {
    # if(fact_var!=not_sel){
    #   data_input[,(fact_var):= as.factor(data_input[,get(fact_var)])]
    # }
    if (num_var_1 != not_sel & num_var_2 != not_sel & fact_var != not_sel) {
      ggplot(
        data = data_input,
        aes_string(x = num_var_1, y = num_var_2, color = fact_var)
      ) +
        geom_point() +
        theme(
          axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12),
          axis.text.y = element_text(vjust = 1, hjust = 1, size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12)
        )
    } else if (num_var_1 != not_sel & num_var_2 != not_sel & fact_var == not_sel) {
      ggplot(
        data = data_input,
        aes_string(x = num_var_1, y = num_var_2)
      ) +
        geom_point() +
        theme(
          axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12),
          axis.text.y = element_text(vjust = 1, hjust = 1, size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12)
        )
    } else if (num_var_1 != not_sel & num_var_2 == not_sel & fact_var != not_sel) {
      ggplot(
        data = data_input,
        aes_string(x = fact_var, y = num_var_1)
      ) +
        geom_violin() +
        theme(
          axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12),
          axis.text.y = element_text(vjust = 1, hjust = 1, size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12)
        )
    } else if (num_var_1 == not_sel & num_var_2 != not_sel & fact_var != not_sel) {
      ggplot(
        data = data_input,
        aes_string(x = fact_var, y = num_var_2)
      ) +
        geom_violin() +
        theme(
          axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12),
          axis.text.y = element_text(vjust = 1, hjust = 1, size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12)
        )
    } else if (num_var_1 != not_sel & num_var_2 == not_sel & fact_var == not_sel) {
      ggplot(
        data = data_input,
        aes_string(x = num_var_1)
      ) +
        geom_histogram() +
        theme(
          axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12),
          axis.text.y = element_text(vjust = 1, hjust = 1, size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12)
        )
    } else if (num_var_1 == not_sel & num_var_2 != not_sel & fact_var == not_sel) {
      ggplot(
        data = data_input,
        aes_string(x = num_var_2)
      ) +
        geom_histogram() +
        theme(
          axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12),
          axis.text.y = element_text(vjust = 1, hjust = 1, size = 12),
          legend.text = element_text(size = 12)
        )
    } else if (num_var_1 == not_sel & num_var_2 == not_sel & fact_var != not_sel) {
      ggplot(
        data = data_input,
        aes_string(x = fact_var)
      ) +
        geom_bar() +
        theme(
          axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12),
          axis.text.y = element_text(vjust = 1, hjust = 1, size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12)
        )
    }
  }

  data_input_DB <- reactive({
    dplyr::select(CaRinDB[CaRinDB$Tissue %in% input$select_tissues, ], c(-3, -5, -7))
  })

  data_input_AF <- reactive({
    dplyr::select(CaRinAF[CaRinAF$Tissue %in% input$select_tissues_AF, ], c(-3, -5, -7))
  })

  observeEvent(data_input_DB(), {
    choices_num_DB <- c(not_sel, names(data_input_DB() %>% select(which(sapply(., is.double)))))
    choices_fct_DB <- c(not_sel, names(data_input_DB() %>% select(which(sapply(., is.character)))))
    updateSelectInput(inputId = "num_var_1_DB", choices = choices_num_DB)
    updateSelectInput(inputId = "num_var_2_DB", choices = choices_num_DB)
    updateSelectInput(inputId = "fact_var_DB", choices = choices_fct_DB)
  })

  observeEvent(data_input_AF(), {
    choices_AF_num <- c(not_sel, names(data_input_AF() %>% select(which(sapply(., is.double)))))
    choices_AF_fct <- c(not_sel, names(data_input_AF() %>% select(which(sapply(., is.character)))))
    updateSelectInput(inputId = "num_var_1_AF", choices = choices_AF_num)
    updateSelectInput(inputId = "num_var_2_AF", choices = choices_AF_num)
    updateSelectInput(inputId = "fact_var_AF", choices = choices_AF_fct)
  })

  num_var_1_DB <- eventReactive(input$run_button_DB, input$num_var_1_DB)
  num_var_2_DB <- eventReactive(input$run_button_DB, input$num_var_2_DB)
  fact_var_DB <- eventReactive(input$run_button_DB, input$fact_var_DB)

  num_var_1_AF <- eventReactive(input$run_button_AF, input$num_var_1_AF)
  num_var_2_AF <- eventReactive(input$run_button_AF, input$num_var_2_AF)
  fact_var_AF <- eventReactive(input$run_button_AF, input$fact_var_AF)

  plot_DB <- eventReactive(input$run_button_DB, {
    draw_plot(data_input_DB(), num_var_1_DB(), num_var_2_DB(), fact_var_DB())
  })

  plot_AF <- eventReactive(input$run_button_AF, {
    draw_plot(data_input_AF(), num_var_1_AF(), num_var_2_AF(), fact_var_AF())
  })

  output$plot_DB <- renderPlot(plot_DB())
  output$plot_AF <- renderPlot(plot_AF())
}
