
#TODO: supprimer 02/11 JF comme si c'était un RTT
#TODO: bouton exemple qui charge mes congés
#TODO: gérer les congres, ajouter le nom ?
#TODO: boutons fleches pour changer d'année
#TODO: tableau par période. Exemple congé : n_days, truc de l'été, truc de octobre (ou c'est les RTT?)


# Init ----------------------------------------------------------------------------------------

options(
  # lubridate.week.start = 1,
  DT.options = list(pageLength=5, lengthChange=FALSE, searching=FALSE)
)

suppressPackageStartupMessages({
  library(dplyr)
  library(DT)
  library(forcats)
  library(glue)
  library(lubridate)
  library(plotly) 
  library(purrr)
  library(shiny)
  library(stringr)
  library(tidyr)
  library(tibble)
})

source("functions.R")


#GLOBALS
month_names = month(1, label=TRUE) %>% levels()
user_input = list()

cli::cli_inform(c(v="Shiny server started, encoding={getOption('encoding')}"))

# Server --------------------------------------------------------------------------------------


function(input, output, session) {
  toggle_click_fromto = 0
  all_user_input = list_c(user_input)
  if(length(all_user_input)==0) all_user_input = today()
  first_date = floor_date(min(all_user_input), "year")
  last_date = ceiling_date(max(all_user_input), "year") - days(1)
  input_years = seq(year(min(all_user_input)), year(max(all_user_input)))
  
  jours_feries = get_jours_feries(first_date, last_date)
  
  r_user_input = reactiveValues(!!!user_input)
  
  db_dates = reactive({
    rtn = get_data(reactiveValuesToList(r_user_input), jours_feries) 
    showNotificationCli("db_dates() : {.val {nrow(rtn)}} lignes", type="default")
    rtn
  }) 
  
  selected_year = reactive({
    a = input$selected_year  
    if(a=="Tout") a=NULL
    if(is.null(a) | a=="") a = year(today())
    a
  })
  
  ## Update le choix d'années selon user_input ----
  updateSelectInput(session, "selected_year",
                    # choices = c("Tout", rev(input_years)),
                    choices = c(rev(input_years)),
                    selected = max(input_years))
  
  ## Match `period_to` & `period_from` ----
  observeEvent(input$period_from, {
    updateDateInput(session, "period_to", value=input$period_from)
  })
  
  
  ## Build r_user_input ----
  observeEvent(input$action_input, { 
    type = isolate(input$period_type)
    from = isolate(input$period_from)
    to = isolate(input$period_to) 
    
    showNotificationCli("Ajout de données : {.val {toupper(type)}} du  {.val {from}} au  {.val {to}}", 
                        type="default")
    r_user_input[[type]] = unique(c(seq(from, to, by="day"), r_user_input[[type]]))
  })
  
  ## Example r_user_input ----
  observeEvent(input$action_example, { 
    source("user_input_example.R")
    example_input = get_example_input()
    update_input(example_input, r_user_input, parent.frame())
    showNotificationCli("Chargement de exemple : {.val {sum(lengths(example_input))}} entrées", 
                        type="default") 
  })
  
  ## Download user input ----
  output$action_download = downloadHandler(filename="gr_calendar.rds", content = function(file) {
    input = reactiveValuesToList(r_user_input)
    showNotificationCli("Téléchargement de données : {.val {sum(lengths(input))}} entrées", 
                        type="default")
    saveRDS(input, file=file)
  })
  
  ## Upload user input ----
  observeEvent(input$file, {    
    f = input$file
    ext = tools::file_ext(f$datapath)
    req(f)
    validate(need(ext=="rds", "L'appli ne comprend que les fichiers .rds"))

    new_content = readRDS(f$datapath)
    update_input(new_content, r_user_input, parent.frame())
    showNotificationCli("Chargement de données : {.val {sum(lengths(new_content))}} entrées", 
                        type="default")
    
    # iwalk(new_content, ~ {r_user_input[[.y]] <<- unique(c(.x, r_user_input[[.y]]))})
  })
  
  ## Plot click = Update date input ----
  observe({
    x = req(input$plot_click)
    if(!is.null(x)){
      a = selected_year()
      tgt = make_date(year=as.numeric(a), month=13-round(x$y), day=round(x$x))
      if(is.na(tgt)) return()
      inputId = c("period_from", "period_to")[toggle_click_fromto+1]
      updateDateInput(session, inputId, value=tgt)
      toggle_click_fromto <<- 1-toggle_click_fromto
    }
    # browser()
  })
  observe({
    x = event_data("plotly_click", source="calendar_plot")
    if(!is.null(x)){
      a = selected_year()
      tgt = make_date(year=as.numeric(a), month=13-x$y, day=x$x)
      if(is.na(tgt)) return()
      inputId = c("period_from", "period_to")[toggle_click_fromto+1]
      updateDateInput(session, inputId, value=tgt)
      toggle_click_fromto <<- 1-toggle_click_fromto
    }
  })
   
  
  ## Boutons supprimer ----
  suppr_period = function(i, tb, val, label) {
    if(length(i)==0) {
      showNotificationCli("Sélectionner une ligne pour la supprimer", type="error")
      return(0)
    }
    
    dat = tb[i,] %>% 
      mutate(across(any_of(c("start", "stop", "date")), dmy))
    if(nrow(dat)!=1) {
      showNotificationCli("Erreur 22838, merci d'ajouter une issue, si possible avec le fichier rds attaché.",
                          type="error")
      return(0)
    }
    
    if(val=="recupJF"){
      x = dat$date
      showNotificationCli("Suppression du {label} du {.val {date}}", type="default")
    } else {
      x = seq(dat$start, dat$stop, by="days")
      showNotificationCli("Suppression des {label} du {.val {format_dmy(dat$start)}} 
                          au {.val {format_dmy(dat$stop)}}", 
                          type="default")
    }    
    
    
    r_user_input[[val]] = setdiff(r_user_input[[val]], x)
  }
  
  
  observeEvent(input$action_del_ca,  
               suppr_period(input$table_CA_rows_selected, table_CA(), "conges", "congés"))
  observeEvent(input$action_del_rtt, 
               suppr_period(input$table_RTT_rows_selected, table_RTT(), "rtt", "RTT/JNT"))
  observeEvent(input$action_del_jf, 
               suppr_period(input$table_recupJF_rows_selected, table_recupJF(), "recupJF", "Récupération JF"))
  
  
  ##TODO Suppressions CA 
  # observeEvent(input$action_del_ca, {
  #   return(NULL)
  #   
  #   i = input$table_CA_rows_selected
  #   if(length(i)==0) {
  #     showNotificationCli("Sélectionner une ligne pour la supprimer", type="error")
  #     return(NULL)
  #   }
  #   dat = table_CA()[i,] %>% 
  #     mutate(start=dmy(start), stop=dmy(stop))
  #   if(nrow(dat)!=1) {
  #     showNotificationCli("Erreur 22838, merci d'ajouter une issue, si possible avec le fichier rds attaché.",
  #                         type="error")
  #     return(NULL)
  #   }
  #   
  #   x = seq(dat$start, dat$stop, by="days")
  # 
  #   showNotificationCli("Suppression des congés du {.val {format_dmy(dat$start)}} au {.val {format_dmy(dat$stop)}}", 
  #                       type="default")
  #   
  #   r_user_input$conges = setdiff(r_user_input$conges, x)
  # })
  
  
  
  ## Output Warnings ? ----
  output$warnings = renderPrint({
    "WARNING!"
  })
  
  
  
  ## Output Tables CA/RTT/JF ----
  table_CA = reactive({
    db_dates() %>% 
      filter(seq_conges==TRUE) %>% 
      summarise(
        start = wmin(date), 
        stop = wmax(date), 
        length = sum(!weekend),
        length2 = sum(!weekend & !ferie),
        raw_length=n(),
        .by=gp_conges
      ) %>% 
      mutate(across(where(is.Date), format_dmy)) %>% 
      select(start, stop, length)
  })
  table_RTT = reactive({
    db_dates() %>% 
      filter(seq_rtt==TRUE) %>% 
      summarise(
        start = wmin(date), 
        stop = wmax(date), 
        length = sum(!weekend),
        length2 = sum(!weekend & !ferie),
        raw_length=n(),
        .by=gp_rtt
      ) %>% 
      mutate(across(where(is.Date), format_dmy)) %>% 
      select(start, stop, length=length2)
  })
  table_recupJF = reactive({
    db_dates() %>%
      filter(recup_jf) %>% 
      mutate(across(where(is.Date), format_dmy)) %>% 
      select(date, source=recup_jf_source)
  })
  
  output$table_CA = renderDT(table_CA(), options=list(), server=FALSE, rownames=FALSE, selection="single")
  output$table_RTT = renderDT(table_RTT(), options=list(), server=FALSE, rownames=FALSE, selection="single")
  output$table_recupJF = renderDT(table_recupJF(), options=list(), server=FALSE, rownames=FALSE, selection="single")
  
  
  
  ## Output Plot ----
  output$plot_title = renderText(paste("Calendrier de l'année", selected_year()))
  output$calendarPlotly = renderPlotly({ 
    if(!input$plot_plotly) return(NULL)
    a = selected_year()
    db = db_dates()
    showNotificationCli("render calendarPlot annee={.val {a}}, plotly={.val {TRUE}}", type="default")

    get_calendar_plot(data=db, annee=a, plotly=TRUE)
  })
  output$calendarPlot = renderPlot({ 
    if(input$plot_plotly) return(NULL)
    a = selected_year()  
    db = db_dates()
    # showNotificationCli("render calendarPlot annee={.val {a}}, plotly={.val {input$plot_plotly}}", type="default")
    showNotificationCli("render calendarPlot annee={.val {a}}, plotly={.val {FALSE}}", type="default")
    # if(is.null(a) | a=="" | nrow(db)==0) return(ggplot())
    validate(need(!is.null(a) & a!="" & nrow(db)>0, 
                  "Calcul du graphique"))
    get_calendar_plot(data=db, annee=a, plotly=FALSE)
  })
}
