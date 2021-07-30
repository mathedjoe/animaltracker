# validate text string as HH:MM:SS

is_valid_time <- function(txt){
  if( grepl("\\d\\d:\\d\\d:\\d\\d", txt) ) {
    hms <- strsplit(txt, ":") %>% unlist %>% as.numeric
    return( hms[1] < 24  && hms[2] < 60 && hms[3] < 60 )
  }
  return(FALSE)
}

shiny::isTruthy(is_valid_time("11:22:33"))

is_valid_time("0")

shiny::isTruthy(is_valid_time("45:22:11"))

shiny::isTruthy("")
shiny::isTruthy(NULL)
