dashboardPage(
  dashboardHeader(title = "Institutional Stress Metrics"),
  
  dashboardSidebar(
    width = 375,
    selectizeInput(inputId = 'selUnitid',
                   label = 'Reference Institution',
                   choices = setNames(institutions$unitid, institutions$entry),
                   options= list(maxOptions = 2000),
                   selected = 181464),
    selectInput(inputId = 'selMetric',
                label = 'Select Metric',
                choices = set_parameters(181464)$metric_names,
                selected = tail(set_parameters(181464)$metric_names, 1))
  ),  
  dashboardBody(
    fluidRow(
      valueBoxOutput("stressPct", width = 4),
      div(style="padding-left: 14px;display:inline-block",
          h2(textOutput('instName')),
          style="float:right")
    ),
    # fluidRow(
    #   box(DT::dataTableOutput("tab1"), width = 12)
    # ),
    fluidRow(
      div(style="padding-left: 14px;display:inline-block", h3(textOutput('dispMetric')), style="float:right")
    ),
    fluidRow(
      plotOutput(outputId = 'plot2',
                 height = 400,
                 width = 650)
    ),
    fluidRow(
      plotOutput(outputId = 'plot1',
                 height = 400,
                 width = 650)
    )
  )
)