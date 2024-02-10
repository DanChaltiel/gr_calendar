
options(encoding="UTF-8")

suppressPackageStartupMessages({
  library(shiny)
  library(plotly) 
  library(DT)
})

css = tags$head(tags$style(HTML("
  #shiny-notification-panel {
    //position:fixed;
    //left: 50%;
    //transform: translate(-50%, 0%);
    width: 400px;
  }
  
  .well {
    padding: 8px;
  }
  
  .progress {
    margin-bottom: 0;
  }
  .form-group {
    margin-bottom: 0;
  } 
")))



fluidPage(
  css,
  
  titlePanel("GR Calendar"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      
      wellPanel(
        h3("Paramètres"),
        # selectInput("selected_year", label="Année", choices="Tout"),
        selectInput("selected_year", label="Année", choices=c()),
        numericInput("n_rtt",
                     "Nombre de RTT/JNT:",
                     value = 18),
        checkboxInput("plot_plotly", label="Plotly ?"),
      ),
      
      wellPanel(
        h3("Fichiers"),
        fileInput("file", label="Importer des entrées", accept=".rds", buttonLabel="Parcourir"), 
        tags$label("Exporter les entrées"),
        downloadButton("action_download", "Télécharger"),
      ),
      
      wellPanel(
        h3("Ajouter une période"),
        p("(Cliquer sur le graphique)"),
        selectInput("period_type", label="Type", choices=c("CA"="conges", "RTT/JNT"="rtt", "Récup JF"="recup_jf", "Congrès"="congres")),
        fluidRow(
          column(width=6, dateInput("period_from", label="Début")),
          column(width=6, dateInput("period_to", label="Fin")),
        ),
        actionButton("action_input", "Ajouter") #TODO centrer
        actionButton("action_example", "Charger un Exemple"),
      ),
    ),
    
    mainPanel(
      h3(uiOutput("plot_title")),
      wellPanel(
        conditionalPanel(
          condition = "input.plot_plotly",
          plotlyOutput("calendarPlotly", fill=T)
        ),
        
        conditionalPanel(
          condition = "!input.plot_plotly",
          plotOutput("calendarPlot", fill=T, click="plot_click")
        )                 
      ),
      # textOutput("warnings"),
      wellPanel(
        fluidRow(
          column(width = 4, h3("Congés annuels"), DTOutput("table_CA"), 
                 actionButton("action_del_ca", "Supprimer")),
          column(width = 4, h3("RTT/JNT"), DTOutput("table_RTT"), 
                 actionButton("action_del_rtt", "Supprimer")),
          column(width = 4, h3("Récupérations JF"), DTOutput("table_recupJF"), 
                 actionButton("action_del_jf", "Supprimer")),
        ),
      ),
    )
  )
)
