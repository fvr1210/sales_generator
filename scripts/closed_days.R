week_days_null <- function(df, week_day){
  
  col_name <- paste("wday_", week_day, sep = "")
  
  if(col_name %in% names(df))
    
    df %>% mutate(sum = if_else(is.na((.)[col_name]) &  wday == week_day, 0, sum))
  
  else df
  
}

week_days_null_ofr <- function(df){
  
  df %>% 
    mutate(sum = case_when(
      wday == "Sunday" & wday_Sunday == 0 ~ 0,
      wday == "Monday" & wday_Monday == 0 ~ 0,
      wday == "Tuesday" & wday_Tuesday == 0 ~ 0,
      wday == "Wednesday" & wday_Wednesday == 0 ~ 0,
      wday == "Thursday" & wday_Thursday == 0 ~ 0,
      wday == "Friday" & wday_Friday == 0 ~ 0,
      wday == "Saturday" & wday_Saturday == 0 ~ 0,
      TRUE ~ sum
    ))
  
}



holidays_null <- function(df, holiday_name){
  
  col_name <- paste("holiday_", holiday_name, sep = "")
  
  col_name <- gsub(" ", ".", col_name)
  
  if(col_name %in% names(df))
    
    df %>% mutate(sum = if_else(is.na((.)[col_name]) &  holiday == holiday_name, 0, sum))
  
  else df
  
}



holidays_null_ofr <- function(df){
  df %>% 
    mutate(sum = case_when(
      holiday == "Christi Himmelfahrt" & holiday_Christi.Himmelfahrt == 0 ~ 0,
      holiday == "Erster Mai" & holiday_Erster.Mai == 0 ~ 0,
      holiday == "Erster Weihnachtstag" & holiday_Erster.Weihnachtstag == 0 ~ 0,
      holiday == "Karfreitag" & holiday_Karfreitag == 0 ~ 0,
      holiday == "Neujahr" & holiday_Neujahr == 0 ~ 0,
      holiday == "Ostermontag" & holiday_Ostermontag == 0 ~ 0,
      holiday == "Pfingstmontag" & holiday_Pfingstmontag == 0 ~ 0,
      holiday == "Tag der Deutschen Einheit" & holiday_Tag.der.Deutschen.Einheit == 0 ~ 0,
      holiday == "Zweiter Weihnachtstag" & holiday_Zweiter.Weihnachtstag == 0 ~ 0,
      holiday == "Fronleichnam" & holiday_Fronleichnam == 0 ~ 0,
      holiday == "Ostern" & holiday_Ostern == 0 ~ 0,
      holiday == "Allerheiligen" & holiday_Allerheiligen == 0 ~ 0,
      holiday == "Heilige Drei Konige" & holiday_Heilige.Drei.Konige == 0 ~ 0,
      holiday == "Pfingsten" & holiday_Pfingsten == 0 ~ 0,
      TRUE ~ sum
    ))
}

