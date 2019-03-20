
source("./utils.R")


ui <- fluidPage(
  list(tags$head(HTML('<link rel="icon", href="icono.png", 
                                 type="image/png" />'))),
  div(style="padding: 1px 0px; width: '100%'",
      titlePanel(
        title="", windowTitle="Workload Management"
      )
  ),
  includeCSS("styles.css"),
  # Application title
  headerPanel(
    #fluidRow(
    div( 
      img(height = 104, width = 104, src = "logo.png"),
      " Workload Management"
    )
  ),
  tags$style(HTML("#tables {
                   width:1400px;
                   overflow-x:scroll
                   }
                   ")),
  tags$head(includeCSS('www/head-styles.css')),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      tags$style(".well {background-color: #447587;}
                   label, input, button, select { 
                 font-family: 'Roboto', sans-serif;
                 font-weight: 350;
                 line-height: 1;
                 color: #FFFFFF;
                 }
                 body { 
                 font-family: 'Roboto', sans-serif;
                 font-weight: 150;
                 line-height: 1;
                 color: #000000;
                 }"),
      tags$style(HTML(gsub("colcol", "#33B9A5", ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: colcol}"))),
      tags$style(HTML(gsub("colcol", "#00A2F3", ".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: colcol}"))),
      tags$style(HTML(gsub("colcol", "#27B701", ".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: colcol}"))),
      sliderInput("threshold",
                  "Threshold",
                  min = 0, max = 100,
                  post  = "%", value = 75, step = 5),
      sliderInput("KPIs",
                  "Número de KPIs",
                  min = 2, max = maxKPI,
                  pre  = "KPI ", value = 3),
      sliderInput("nPymes",
                  "Top Pymes",
                  min = 5, max = 30,
                  post  = " PyMEs", value = 9),
      selectInput('campanas',
                  'Campaña',
                  nombres.campanas), width = 3
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel(p(icon("bell"), "Centro de Alertamiento"),
                 tabsetPanel(tabPanel(p(icon("table"), "Resumen"), uiOutput(outputId = "tables")),
                             tabPanel(p(icon("chart-line"), "Gráfico"), showOutput("plot", "nvd3")))
                 ),
        tabPanel(p(icon("desktop"), "Conveyor Belt"))
      )
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dataInput <- reactive({
    RandomKPI(data.init,
              n.KPI = input$KPIs,
              flag = (input$threshold)/100,
              campanas = input$campanas)
  })
  
  output$tables <- renderUI({
    out <- dataInput() %>%
      FormatoIconos(n.Pymes = input$nPymes) %>%
      unlist()
    
    return(HTML(paste0("<div>", out, "</div>")))
  })
  
  output$plot <- renderChart2({
    dataInput() %>%
      PloteoSabroso()
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
#rstudioapi::getSourceEditorContext()
