# ==== server.R ===============================================================
server <- function(input, output, session) {
  # suppress warnings  
  storeWarn<- getOption("warn")
  options(warn = -1) 
  
  # B - Buttons
  # l - Length changing input control
  # f - Filtering input
  # r - pRocessing display element
  # t - Table
  # i - Table information summary
  # p - Pagination control
  # General options for all tables ----
  list.options <- list(
    pageLength = 10,
    lengthMenu =  list(c(10, 25, 50, 100, -1), 
                       c('10', '25', '50','100', 'All')),
    paging = T,
    search = list(regex = TRUE),
    searchHighlight = TRUE,
    colReorder = TRUE,
    orientation ='landscape',
    dom = "<'row'<'col-md-3'l><'col-md-6'B><'col-md-3'f>><'row'<'col-md-12't>><'row'<'col-md-3'i><'col-md-1'><'col-md-8'p>>",
    #dom = 'lBfrtip',
    buttons =
      list(
        list(extend = 'pdf',
             text = img_uri_icon('icons/pdf_icon.png'),
             pageSize = 'A4',
             orientation = 'landscape',
             filename = 'CaRinDB'
        ),
        list(extend = "csv", 
             text = '<span class="glyphicon glyphicon-download-alt"></span> Current Page (csv)', 
             filename = "CaRinDB_page",
             exportOptions = list(
               modifier = list(page = "current")
             )
        ),
        list(extend = "csv", 
             text = '<span class="glyphicon glyphicon-download-alt"></span> All Pages (csv)', 
             filename = "CaRinDB_page",
             exportOptions = list(
               modifier = list(page = "all")
             )
        )
      )
  )
  
  CaRinDB_Tissue <- count(CaRinDB, Tissue) %>%
    arrange(desc(n))
  
  CaRinDB_Tissue$Tissue <-  factor(CaRinDB_Tissue$Tissue, levels = CaRinDB_Tissue$Tissue)
  
  CaRinDB_Type_Mut_EFF <- count(CaRinDB, Type_Mut_EFF) %>%
    mutate(Type_Mut_EFF = as.factor(Type_Mut_EFF)) 
  
  
  # Bar plot of Samples by Tissue ----
  output$fig.barTissue <- renderPlotly({
    plot_ly(data = CaRinDB_Tissue , x = ~Tissue, y = ~n, type = 'bar', labels = ~Tissue, 
            text = ~n, textposition="outside", sort = T, text_auto='.2s', textangle=0, textfont.size=8  
    ) %>%
      layout(showlegend = F, yaxis = list(title = '#Samples by Cancer Type'),
             xaxis = list(title = "Cancer_Type", tickangle = -45),
             font  = list(size = 10))
  })
  
  # Pie chart of Tissue of Unique SNPs  ----
  output$fig.pieType_Mut <- renderPlotly({
    plot_ly() %>% 
      add_pie(data = CaRinDB_Type_Mut_EFF, labels = ~Type_Mut_EFF, values = ~n, 
              hole = 0, name = "Type_Mut_EFF", textinfo='label+value', insidetextorientation='radial', sort = FALSE) %>% 
      layout(title = "#Mutation Type", showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             font  = list(size = 10))
  })
  
  #------------------------------------------
  # Ndamage vs. Mutation number ok ---- 
  output$fig.bar.NdamageTotMut <- renderPlotly({
    plot_ly(data = count(CaRinDB[CaRinDB$Tissue %in%  input$select_tissues, ], NdamageCalc), x = ~NdamageCalc, y = ~n, type = 'bar', 
            text = ~n, textposition="outside"
    ) %>%
      layout(showlegend = F, 
             yaxis = list(title = '# Mutations'),
             xaxis = list(title = '# N DamageCalc', tickangle = -45),
             font  = list(size = 10)
      )
  })
  
  # Inter_Res_tot vs. Mutation number ok ----
  output$fig.barInterResTotMut <- renderPlotly({
    plot_ly(data = count(CaRinDB[CaRinDB$Tissue %in%  input$select_tissues, ], Inter_Res_tot), x = ~Inter_Res_tot, y = ~n, type = 'bar', 
            text = ~n, textposition="outside", sort = T, text_auto='.2s', textangle=0, textfont.size=8  
    ) %>%
      layout(showlegend = F, 
             yaxis = list(title = '# Mutations'),
             xaxis = list(title = '# Inter_Res_tot', tickangle = -45),
             font  = list(size = 10)
      )
  })
  
  # - betweenness vs. Frequency ok ----
  output$fig.Betweenness <- renderPlotly({
    plot_ly(x = CaRinDB$betweennessWeighted_node[CaRinDB$Tissue %in%  input$select_tissues],  type = "histogram") %>%
      layout(showlegend = F, 
             #title = 'Betweenness',
             yaxis = list(title = '# Mutations'),
             xaxis = list(title = '# Betweenness Weighted'),
             updatemenus = list(list(
               y = 1.1,
               active = 0,
               buttons= list(
                 list(label = 'Linear',
                      method = 'update',
                      args = list(list(visible = c(T)), list(#xaxis = list(type = 'linear'),
                        yaxis = list(type = 'linear')))),
                 list(label = 'Log',
                      method = 'update', 
                      args = list(list(visible = c(T)), list(yaxis = list(type = 'log')
                      ))))))
      )
    
  })
  
  # Degree (x) vs. Mutation number ok ----
  output$fig.barDegMut <- renderPlotly({
    plot_ly(data = count(CaRinDB[CaRinDB$Tissue %in%  input$select_tissues, ], Degree_RING), x = ~Degree_RING, y = ~n, type = 'bar', 
            text = ~n, textposition="outside", sort = T, text_auto='.2s', textangle=0, textfont.size=8  
    ) %>%
      layout(showlegend = F, 
             yaxis = list(title = "# Mutations"),
             xaxis = list(title = "# Degree RING"),
             updatemenus = list(list(
               y = 1.1,
               active = 0,
               buttons= list(
                 list(label = 'Linear',
                      method = 'update',
                      args = list(list(visible = c(T)), list(xaxis = list(type = 'linear'),
                                                             yaxis = list(type = 'linear')))),
                 list(label = 'Log',
                      method = 'update', 
                      args = list(list(visible = c(T)), list(xaxis = list(type = 'log'),
                                                             yaxis = list(type = 'log')
                      ))))))
      )
  })
  
  output$fig.Degree_RING <- renderPlotly({
    plot_ly(data = CaRinDB[CaRinDB$Tissue %in%  input$select_tissues, ], x = ~Degree_RING, y = ~dbNSFP_gnomAD_exomes_AF, type = "scatter") %>%
      layout(showlegend = F, 
             yaxis = list(title = '# dbNSFP_gnomAD_exomes_AF'),
             xaxis = list(title = '# Degree RING'),
             updatemenus = list(list(
               y = 1.1,
               active = 0,
               buttons= list(
                 list(label = 'Linear',
                      method = 'update',
                      args = list(list(visible = c(T)), list(xaxis = list(type = 'linear'),
                                                             yaxis = list(type = 'linear')
                      ))),
                 list(label = 'Log',
                      method = 'update', 
                      args = list(list(visible = c(T)), list(xaxis = list(type = 'log')#,
                                                             #yaxis = list(type = 'log')
                      ))))))
      )
  })
  
  # - coefclustering vs. Mutation number ----
  output$fig.clusteringCoef <- renderPlotly({
    plot_ly(x = CaRinDB$clusteringCoef_node[CaRinDB$Tissue %in%  input$select_tissues],  type = "histogram") %>%
      layout(showlegend = F, 
             yaxis = list(title = '# Mutations'),
             xaxis = list(title = "# Clustering Coeficient")) 
  })
  
  
  # ChangeType vs. Mutation number ----
  output$fig.bar.ChangeTypeTotMut <- renderPlotly({
    plot_ly(data = count(CaRinDB[CaRinDB$Tissue %in%  input$select_tissues, ], typechangeProt), x = ~typechangeProt, #list("Del", "Subst", "Translation\n Termination"), 
            y = ~n, type = 'bar', 
            text = ~n, textposition="outside"
    ) %>%
      layout(showlegend = F, 
             yaxis = list(title = '# Mutations'),
             xaxis = list(title = "# Change Type"), # , tickangle = -45
             font  = list(size = 10)
      )
  })
  
  # Deleteria5/10/20 vs. Mutation number ----
  output$fig.bar.deleteriaTotMut <- renderPlotly({
    plot_ly(data = count(CaRinDB[CaRinDB$Tissue %in%  input$select_tissues, ], Deleterious, Deleterious5, Deleterious10), x = list("Non Deleteria", "Deleteria", "Deleteria5",  "Deleteria10"),  y = ~n, type = 'bar', 
            text = ~n, textposition="outside") %>%
      layout(showlegend = F, 
             yaxis = list(title = '# Mutations'),
             xaxis = list(title = '# Deleteria'), # , tickangle = -45
             font  = list(size = 10)
      )
  })
  
  # - b-factor vs. Mutation number ----
  output$fig.bFactor <- renderPlotly({
    plot_ly(x = CaRinDB$Bfactor_CA_RING[CaRinDB$Tissue %in%  input$select_tissues],  type = "histogram") %>%
      layout(showlegend = F, 
             yaxis = list(title = '# Mutations'),
             xaxis = list(title = '# B-factor'),
             updatemenus = list(list(
               y = 1.1,
               active = 0,
               buttons= list(
                 list(label = 'Linear',
                      method = 'update',
                      args = list(list(visible = c(T)), list(yaxis = list(type = 'linear')))),
                 list(label = 'Log',
                      method = 'update', 
                      args = list(list(visible = c(T)), list(yaxis = list(type = 'log')
                      ))))))
      ) 
  })
  
  
  # CaRinDB ----
  output$tb_CaRinDB <- DT::renderDataTable({
    #w$show() 
    DT::datatable(
      if(input$show_unique == "unique"){ 
        CaRinDB[ CaRinDB$Tissue %in% input$show_tissues & !duplicated(CaRinDB[CaRinDB$Tissue %in% input$show_tissues, input$show_vars]) , input$show_vars, drop = FALSE]
      } else{
        CaRinDB[CaRinDB$Tissue %in% input$show_tissues , input$show_vars, drop = FALSE]
      } ,
      class = 'cell-border stripe',
      rownames = FALSE,
      filter = 'top',
      extensions = c('Buttons', "ColReorder"),
      options = list.options,  
      escape=FALSE)
  },
  server = TRUE
  )
  
  # CaRinDB/AlphaFold ----
  output$tb_CaRinAF <- DT::renderDataTable({
    #w$show()
    DT::datatable(
      if(input$show_unique_AF == "unique"){ 
        CaRinAF[ CaRinAF$Tissue %in% input$show_tissues_AF & !duplicated(CaRinAF[CaRinAF$Tissue %in% input$show_tissues_AF, input$show_vars_AF]) , input$show_vars_AF, drop = FALSE]
      } else{
        CaRinAF[CaRinAF$Tissue %in% input$show_tissues_AF , input$show_vars_AF, drop = FALSE]
      } ,
      class = 'cell-border stripe',
      rownames = FALSE,
      filter = 'top',
      extensions = c('Buttons', "ColReorder"),
      options = list.options,  
      escape=FALSE)
  },
  server = TRUE
  )
  
  
}
