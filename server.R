shinyServer(function(input, output, session) {
  
  refUnitid <- reactive({
    set_parameters(unitid = input$selUnitid)
  })
  
  refMetric <- reactive({
    input$selMetric
  })
  
  observe(
    updateSelectInput(session = session,
                      inputId = 'selMetric',
                      choices = refUnitid()$metric_names,
                      selected = tail(refUnitid()$metric_names, 1))
  )
  
  output$instName <- renderText(glue('{refUnitid()$institution_name} ({refUnitid()$sector})'))
  output$dispMetric <- renderText(refMetric())
  
  output$stressPct <- renderValueBox({
    val <- stress[unitid == refUnitid()$unitid, .(stress_score_pct * 100)]
    
    box_color <- ifelse(val == 0, 'green', ifelse(val <= 33.0, 'yellow', 'red'))
    box_icon <- ifelse(val == 0, 'ok-sign', ifelse(val <= 33.0, 'question-sign', 'remove-sign'))
    
    valueBox(
      value = paste0(round(val, digits = 0), '%'),
      subtitle = "Stress Percentage",
      color = box_color,
      icon = icon(box_icon, lib = "glyphicon")
    )
  })
  
  # output$tab1 <- DT::renderDataTable(institutions)

  output$plot1 <- renderPlot({
    fmt <- ifelse(refMetric() == 'Retention',
                  scales::label_percent(),
                  ifelse(refMetric() == 'Market Price',
                         scales::label_dollar(),
                         ifelse(refMetric() == 'State Appropriations',
                                scales::label_dollar(scale = .000001,
                                                     suffix = 'M'),
                                scales::label_comma())))

    box_me(refMetric(), refUnitid()) +
      # set y-axis to comma format
      scale_y_continuous(labels = fmt) +
      labs(title = glue('{refUnitid()$institution_name} vs. Four Year {refUnitid()$sector} Institutions'),
           y = refMetric())
      
  })

  output$plot2 <- renderPlot({
    
    fmt <- ifelse(refMetric() == 'Retention',
                  scales::label_percent(),
                  ifelse(refMetric() == 'Market Price',
                         scales::label_dollar(),
                         ifelse(refMetric() == 'State Appropriations',
                                scales::label_dollar(scale = .000001,
                                                     suffix = 'M'),
                                scales::label_comma())))
    
    plot_me(refMetric(), refUnitid()) +
      scale_y_continuous(labels = fmt) +
      labs(y = refMetric())
  })
  
})
