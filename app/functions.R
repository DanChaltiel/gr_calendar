

print("Functions loaded")


get_jours_feries = function(first_date, last_date, dir="jf"){
  input_years = seq(year(first_date), year(last_date))
  for(y in input_years){
    jours_feries_file = glue("{dir}/jours_feries_{y}.json")
    if(!file.exists(jours_feries_file)){
      api = glue("https://calendrier.api.gouv.fr/jours-feries/metropole/{y}.json")
      # cli_inform("Caching {.url {api}}")
      download.file(url=api, destfile=jours_feries_file, quiet=TRUE)
    }
  }
  
  rtn = dir(dir, pattern="jours_feries_\\d+\\.json", full.names=TRUE) %>% 
    map(~jsonlite::fromJSON(.x)) %>% 
    unlist()
  rtn 
}


get_data = function(user_input, jours_feries){
  month_names = month(1, label=TRUE) %>% levels()
    
  # print("jrioezapruezio")
  # print(user_input)
  # print(jours_feries)
  
  all_user_input = list_c(user_input) 
  if(length(all_user_input)==0) all_user_input = today()
  first_date = floor_date(min(all_user_input), "year")
  last_date = ceiling_date(max(all_user_input), "year") - days(1)
  
  db_dates =
    tibble(date = seq(first_date, last_date, by = "day"),
           year = year(date),
           month = month(date, label = TRUE),
           day = mday(date),
           weekday = wday(date, label = TRUE, abbr = FALSE),
           w = substr(weekday, 1, 1) %>% str_replace_all("s|d", "")) %>%
    complete(day, month, year) %>%
    arrange(desc(date)) %>% 
    mutate(
      month = factor(month, levels=month_names) %>% fct_rev(),
      day_label = day %>% str_pad(width=2, pad="0") %>% factor(),
      ferie_name = jours_feries[as.character(date)],
      
      weekend = w=="",
      ferie = !is.na(ferie_name),
      conges = date %in% user_input$conges,
      congres = date %in% user_input$congres,
      recup_jf = date %in% user_input$recup_jf, #récupération de JF
      rtt_bak = date %in% user_input$rtt,
      rtt = ifelse(rtt_bak & (ferie|conges), FALSE, rtt_bak),
      work = !(weekend|ferie|rtt|conges),
      
      jf_a_recup = ferie & (conges|weekend), #JF à récupérer
      tmp = (ceiling(month(date)/3+1)*3),
      jf_a_recup_max = if_else(!jf_a_recup, NA, make_datetime(year, tmp, 1)-days(1)), #fin du trimestre suivant
      jf_a_recup_day_gp = map2(date, jf_a_recup_max, ~date[recup_jf & date>.x & date<.y]),
      jf_a_recup_day_gp = if_else(jf_a_recup, jf_a_recup_day_gp, NA),
      
      gp_rtt=data.table::rleid(rtt|weekend|ferie), 
      gp_conges=data.table::rleid(conges|weekend), 
      
      period_rtt = cumsum(day==31 & month=="déc"), 
      period_conges = cumsum(day==30 & month=="avr"), 
      
      datef = format(date, "%A %d %B %Y"),
      
      error = case_when(
        rtt_bak & conges ~ "RTT/JNT posé pendant des congés",
        rtt_bak & ferie ~ "RTT/JNT posé pendant un jour férié",
        .default = NA
      )
    ) %>%
    mutate(jf_a_recup_day = cur_group()[[1]][[1]][row_number()] %||% NA,
           .by=jf_a_recup_day_gp) %>% 
    mutate(seq_rtt = ifelse(any(rtt) & first(rtt|weekend|ferie) , TRUE, FALSE),
           .by = gp_rtt) %>%
    mutate(seq_conges = ifelse(any(conges) & first(conges|weekend) , TRUE, FALSE),
           .by = gp_conges) %>% 
    mutate(
      recup_jf_source = map_chr(date, ~{
        # a = !is.na(jf_a_recup_day) & jf_a_recup_day==.x
        a = replace_na(jf_a_recup_day==.x, FALSE)
        
        if(sum(a) >1) browser()
        if(sum(a) == 0) return(NA)
        glue("{format_dmy(date[a])} ({ferie_name[a]})")
        # rtn = glue("{format_dmy(date[a])} ({ferie_name[a]})")
        # if(length(rtn)!=0) browser()
        # if(.x==ymd('2024-11-05')) browser()
        # if(length(rtn)==0) rtn = NA
        # rtn
      })      ,
      day_type = case_when(
        is.na(date) ~ "None",
        jf_a_recup ~ "Férié à récupérer",
        ferie ~ "Férié",
        weekend ~ "Weekend",
        recup_jf ~ "Recup JF",
        rtt ~ "RTT/JNT",
        conges ~ "Congés",
        congres ~ "Congrès",
        .default="Jour de semaine"
      ),
      day_type_color = case_match(
        day_type, 
        "None"~"black",
        "Férié à récupérer"~"grey40",
        "Férié"~"grey20",
        "Weekend"~"grey70",
        "Recup JF"~"chocolate4",
        "RTT/JNT"~"orange",
        "Congés"~"yellow",
        "Congrès"~"cyan3",
        "Jour de semaine"~"white",
        .default="purple"
      ),
      tile_color = case_when(
        jf_a_recup & is.na(jf_a_recup_day) ~ "darkred",
        jf_a_recup ~ "darkgreen",
        .default = "grey80"
      ),
      label = case_when(
        is.na(date) ~ "",
        !is.na(jf_a_recup_day) ~ glue("{datef}<br>Férié ({ferie_name})<br>Récupéré le {format_dmy(jf_a_recup_day)}"),
        recup_jf ~ glue("{datef}<br>{day_type}<br>Récupération du {recup_jf_source}"),
        jf_a_recup ~ glue("{datef}<br>Férié ({ferie_name})<br>À récupérer avant le {format_dmy(jf_a_recup_max)}"),
        ferie ~ glue("{datef}<br>Férié ({ferie_name})"),
        .default = glue("{datef}<br>{day_type}")
      ),
    )
}
# db_dates = get_data(user_input, jours_feries)


get_calendar_plot = function(data=db_dates, annee=NULL, source="calendar_plot", plotly=TRUE){
  if(is.null(annee)){
    data = data %>%
      mutate(month = glue("{month} - {year}") %>% as_factor())
    annee_label = paste0("Années ", min(data$year), "-", (max(data$year)))
  } else {
    data = data %>%
      filter(year==annee)
    annee_label = paste0("Année ", annee)
  }
  # if(nrow(data)==0) cli_abort("Plot data empty (année={annee})")
  datetype_colors = data %>% distinct(day_type, day_type_color) %>% deframe() 
  tile_colors = data %>% distinct(x=tile_color, tile_color) %>% deframe()
  tile_width = c(.7,.1) %>% set_names("TRUE","FALSE")#TODO ça marche pas !!!
  ajd = today()
  p = data %>% 
    ggplot() +
    aes(x=day_label, y=month, text=label) +
    geom_tile(aes(fill=day_type, color=tile_color, linewidth=jf_a_recup)) +
    annotate("point", y=month(ajd, label=TRUE), x=mday(ajd),
             color="darkgreen", size=5) + #, label="Aujourd'hui"
    # geom_tile(aes(fill=day_type, color=tile_color), linewidth=0.1)+
    geom_text(aes(label=w), na.rm=TRUE) +
    scale_x_discrete(position = "top") +
    scale_linewidth_manual(values=tile_width) +
    scale_color_manual(values=tile_colors) +
    scale_fill_manual(values=datetype_colors) +
    labs(x=NULL, y=NULL) +
    # labs(title=annee_label) +
    coord_fixed() +
    theme_minimal() +
    theme(
      legend.position="none"
    )
  
  if(!plotly) return(p)
  
  ggplotly(p, tooltip="text", source=source) %>%
    config(modeBarButtonsToRemove = c("zoom", "pan2d", "select2d", "lasso2d", 
                                      "zoomIn2d", "zoomOut2d","hoverclosest", "hoverCompare")) %>%
    # "autoScale2d", "resetScale2d"
    layout(xaxis = list(side ="top" ) )  %>% 
    event_register('plotly_click')
}


# db_dates = get_data(user_input)
# get_calendar_plot(2024, data=db_dates)  %>%
#   print()





# BAK -----------------------------------------------------------------------------------------



get_sequence = function(data=db_dates, type=c("rtt", "conges")){
  type = match.arg(type)
  per = paste0("period_", type)
  sequ = paste0("seq_", type)
  gp = paste0("gp_", type)
  data %>% 
    mutate(
      start_period = min(date, na.rm=TRUE), 
      stop_period = max(date, na.rm=TRUE), 
      length_period = round(as.numeric(stop_period-start_period)/30.44),
      .by=!!sym(per)
    ) %>% 
    filter(!!sym(sequ)==TRUE) %>% 
    summarise(
      start = min(date), 
      stop = max(date), 
      length = sum(!weekend),
      length2 = sum(!weekend & !ferie),
      raw_length=n(),
      .by=c(!!sym(per), start_period, stop_period, length_period, !!sym(gp))
    ) %>% 
    arrange(start)
}



# Utils ---------------------------------------------------------------------------------------



format_dmy = function(x) format(x, "%d/%m/%Y")

# cli_glue = function(x, .envir=parent.frame()) cli::cli_fmt(cli::cli_text(x, .envir=.envir), collapse=TRUE, strip_newline=TRUE) |> stringr::str_replace_all("\n", " ")
 
showNotificationCli = function(msg, duration=15, type="default") {
  # cli::cli_inform(msg, .envir = parent.frame()) 
  # msg = cli_glue(msg, .envir = parent.frame())
  if(missing(duration) & type!="default") duration=NULL
  showNotification(HTML(ansi_html(msg)), type=type, duration=duration)
}

setdiff.Date = function(x, y) base::setdiff(x, y) %>% as_date()
intersect.Date = function(x, y) base::intersect(x, y) %>% as_date()

wmin = function(...) suppressWarnings(min(...) )
wmax = function(...) suppressWarnings(max(...) )