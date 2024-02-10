

# User input --------------------------------------------------------------

get_example_input = function(){
  
  conges = c(
    #2023
    # seq(ymd("2024-01-08"), ymd("2024-01-14"), by="day"),
    seq(ymd("2023-03-20"), ymd("2023-03-24"), by="day"),
    seq(ymd("2023-08-14"), ymd("2023-09-01"), by="day"),
    seq(ymd("2023-10-16"), ymd("2023-10-20"), by="day"),
    
    #2024
    seq(ymd("2024-03-17"), ymd("2024-03-22"), by="day"),
    seq(ymd("2024-06-17"), ymd("2024-06-21"), by="day"),
    seq(ymd("2024-08-12"), ymd("2024-08-30"), by="day"),
    
    NULL
  )
  
  
  rtt = c(
    #2023
    seq(ymd("2023-05-02"), ymd("2023-05-05"), by="day"),
    ymd("2023-08-10"),
    ymd("2023-08-11"),
    seq(ymd("2023-11-24"), ymd("2023-11-28"), by="day"),
    #bad
    ymd("2023-12-22"),
    seq(ymd("2023-12-26"), ymd("2023-12-29"), by="day"),
    #good
    # seq(ymd("2023-12-22"), ymd("2023-12-29"), by="day"),
    
    #2024
    seq(ymd("2024-05-02"), ymd("2024-05-12"), by="day"),
    ymd("2024-02-02"),
    ymd("2024-02-05"),
    ymd("2024-02-16"),
    ymd("2024-02-19"),
    
    NULL
  )
  
  recup_jf = c(
    ymd("2023-05-19"), 
    ymd("2023-10-02"),
    ymd("2024-02-16"),
    
    NULL
  )
  
  congres = c(
    #2023
    seq(ymd("2023-06-21"), ymd("2023-06-23"), by="day"),
    
    #2024
    seq(ymd("2024-05-15"), ymd("2024-05-17"), by="day"),
    seq(ymd("2024-06-12"), ymd("2024-06-14"), by="day"),
    NULL
  )
  
  
  
  
  # Tests -------------------------------------------------------------------
  
  
  rtt = c(
    rtt, 
    ymd("2023-08-17"), #intersect avec congés
    ymd("2023-12-25"), #intersect avec JF
    ymd("2023-10-02"), #intersect avec recup_JF
    
    NULL
  )
  
  recup_jf = c(
    recup_jf, 
    ymd("2024-11-02"), #recupJF un weekend
    ymd("2024-11-04"), #2 récupJF au même moment
    ymd("2024-11-05"), 
    NULL
  )
  
  
  
  # Final -------------------------------------------------------------------
  
  user_input = lst(conges, rtt, recup_jf, congres)
  print(lengths(user_input))
  # saveRDS(user_input, "user_input.rds", )
  
  # user_input = lst()
  user_input
}

