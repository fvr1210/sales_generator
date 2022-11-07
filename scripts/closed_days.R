week_days_null <- function(df, week_day){
  
  col_name <- paste("wday_", week_day, sep = "")
  
  if(col_name %in% names(df))
    
    df %>% mutate(sum = if_else(is.na((.)[col_name]) &  wday == week_day, 0, sum))
  
  else df
  
}


holidays_null <- function(df, holiday_name){
  
  col_name <- paste("holiday_", holiday_name, sep = "")
  
  col_name <- gsub(" ", ".", col_name)
  
  if(col_name %in% names(df))
    
    df %>% mutate(sum = if_else(is.na((.)[col_name]) &  holiday == holiday_name, 0, sum))
  
  else df
  
}
