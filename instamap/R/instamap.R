#' US Map
#'
#' This function generates a map of the lower 48 US states
#' from the ggplot2 and maps packages while displaying the
#' variable of your choice as a color gradient.
#'
#' @param df_path The path of your csv file.
#' @param state_label The column label where the US states are in your csv file.
#' @param var_label The column label where your variable of choice is in your csv file.
#' @return A US map as a ggplot plot object.
#' @examples
#' us_map("crimes.csv", "State", "Homicide Rate")
#' us_map("weather.csv", "state", "snowfall")
us_map <- function(df_path, state_label, var_label) {
  states <- map_data("state")
  df <- read.csv(df_path)
  df <- subset(df, select=c(state_label, var_label))
  df[,var_label] <- as.numeric(as.character(df[,var_label]))
  names(df) <- c("region",var_label)
  df[,"region"] <- tolower(df[,"region"])
  mapped_df <- merge(states, df, sort = TRUE, by = "region", all=TRUE)
  plot <- ggplot(mapped_df, aes(long, lat)) +
    geom_polygon(color="white",aes(group=group, fill=mapped_df[,var_label]))
  return(plot)
}

#' State Map
#'
#' This function generates a map of a US state
#' from the ggplot2 and maps packages while displaying
#' county outlines and the variable of your choice as a color gradient.
#'
#' @param df_path The path of your csv file.
#' @param state The US state to be visualized.
#' @param county_label The column label where the state counties are in your csv file.
#' @param var_label The column label where your variable of choice is in your csv file.
#' @return A US state map with counties as a ggplot plot object.
#' @examples
#' state_map("crimes.csv", "New York", "County", "Homicide Rate")
#' state_map("weather.csv", "washington", "county", "Precipitation Rate")
state_map <- function(df_path, state, county_label, var_label) {
  state <- tolower(state)
  states <- map_data("state")
  counties <- map_data("county")
  state_df <- subset(states, region == state)
  state_county_df <- subset(counties, region == state)
  df <- read.csv(df_path)
  df <- subset(df, select=c(county_label, var_label))
  names(df) <- c("subregion", var_label)
  df[,"subregion"] <- tolower(df[,"subregion"])
  df[,var_label] <- as.numeric(as.character(df[,var_label]))
  mapped_df <- inner_join(state_county_df, df, by="subregion")
  plot <- ggplot(data = mapped_df, mapping = aes(x = long, y = lat, group = group)) +
    geom_polygon(data=state_county_df, fill=NA, color="white") +
    geom_polygon(data=mapped_df, aes(fill=mapped_df[,var_label]), color="white")
  return(plot)
}

