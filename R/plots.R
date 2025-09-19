#Function used to plot the histogram plot
histogram <- function(col_name, fill = "blue", bins = 25){

  # text = map(paste(
  #   '<b>Frequency:</b>', count,
  #   '<br>',
  #   strong(description[x]), ":", x), HTML)

  plot <- ggplot(nasa_fireball, aes_string(x = col_name)) +
    geom_histogram(bins = bins, fill = fill, color = "black") +
    scale_x_continuous(trans = 'log', labels = scales::number_format(accuracy = 0.1)) +
    theme_bw() +
    labs(
      title = paste0("Histogram of ", description[col_name]),
      x = description[col_name],
      y = NULL
    )


  return(ggplotly(plot))

}
