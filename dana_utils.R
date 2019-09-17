# Plotting fun
plot_stn_data <- function(stn_data) {
  p_out <- ggplot(
    stn_data,
    aes(x = TIMESTAMP, y = Value)
  ) +
    geom_point(fill = "white") +
    geom_line(colour = "#8C5391") +
    # geom_hline(aes(yintercept = `25%`, col = "25%")) +
    # geom_hline(aes(yintercept = `50%`, col = "50%")) +
    # geom_hline(aes(yintercept = `75%`, col = "75%")) +
    # geom_hline(aes(yintercept = `90%`, col = "90%")) +
    # geom_hline(aes(yintercept = `100%`, col = "100%")) +
    xlab("Timestamp (UTC)") +
    ylab(NULL) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "lightgrey", size = 0.25),
      plot.background = element_rect(fill = "#222222", color = "#222222"),
      panel.background = element_rect(fill = "#4F4E4E",  color = "#4F4E4E"),
      panel.border = element_blank(),
      panel.spacing = unit(2, "lines"),
      axis.text = element_text(size = 14, colour = 'white'),
      axis.title = element_text(size = 18, face = "bold", colour = "white"),
      axis.line = element_line(size = 1.15, colour = "white"),
      strip.background = element_blank(),
      strip.text = element_text(size = 18, face = "bold", colour = "white"),
      strip.placement = "outside"
    )

  # If discharge values present, add as facet
  if (sum(is.na(stn_data$Value[stn_data$Parameter == "Discharge (cms"])) < nrow(stn_data)) {
    p_out <- p_out +
      facet_wrap(
        ~Parameter,
        nrow = 2,
        scales = "free_y",
        strip.position = "left"
      )
  }
  return(p_out)
}

assign_col <- function(wsc_id, gauge_data){
  
  stn_data <- gauge_data %>%
    filter(
      STATION_ID == wsc_id & 
        Parameter == "Water Level (m)"
    ) 
  
  stn_data <- stn_data %>%
    filter(
      TIMESTAMP == max(stn_data$TIMESTAMP)
    )
  
    case_when(
      stn_data$Value > stn_data$`25%` ~ "darkgreen",
      stn_data$Value > stn_data$`50%` ~ "lightgreen",
      stn_data$Value > stn_data$`75%` ~ "orange",
      stn_data$Value > stn_data$`90%` ~ "red",
      TRUE ~ "gray"
    )
}

calc_thresh <- function(wsc_id) {

  # Grab data for station (all parameters)
  station_data <- hy_daily(
    station_number = wsc_id
  )

  if (nrow(station_data)) {
    station_thresh <- station_data %>%
      # Group by parameter type for stat calculations
      group_by(Parameter) %>%
      # Get rid of missing values
      filter(!is.na(Value)) %>%
      summarise(
        "station_number" = wsc_id,
        # Number of values considered
        "n" = n(),
        # Percentiles
        "25%" = quantile(Value, 0.25),
        "50%" = quantile(Value, 0.5),
        "75%" = quantile(Value, 0.75),
        "90%" = quantile(Value, 0.9),
        "100%" = quantile(Value, 1)
      ) %>%
      mutate_if(is.double, round, 3) %>%
      ungroup() %>%
      mutate(
        Parameter = case_when(
          Parameter == "Level" ~ "Water Level (m)",
          Parameter == "Flow" ~ "Discharge (cms)"
        )
      )
  } else {
    station_thresh <- data.frame(
      station_number = c(wsc_id, wsc_id),
      Parmeter = c("Water Level (m)", "Discharge (cms)"),
      n = c(0, 0),
      "25%" = c(NA, NA),
      "50%" = c(NA, NA),
      "75%" = c(NA, NA),
      "90%" = c(NA, NA),
      "100%" = c(NA, NA),
      stringsAsFactors = FALSE
    )
  }

  return(station_thresh)
}

get_min_stn_date <- function(wsc_id) {
  tidyhydat::hy_stn_data_range(
    wsc_id
  ) %>%
    pull(
      Year_from
    ) %>%
    min()
}

update_thresh <- function(wsc_ids, out_path) { 
  
  load(out_path)
  
  hy_vers_date <- max(tidyhydat::hy_version()$Date)
  
  hy_thresh <- purrr::map(
    unique(wsc_ids),
    calc_thresh
  ) %>%
    bind_rows()

  save(hy_vers_date, hy_thresh[1:8], out_path)
}