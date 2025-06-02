# Plot function: obsevations vs entities
plot_entities_vs_obs <- function(data) {

  .data <- data %>%
    count(Entidad, wt = `Cantidad de obs`, name = "Cantidad de preguntas observadas") %>%
    mutate(Entidad = fct_reorder(Entidad, `Cantidad de preguntas observadas`))

  if (nrow(.data) == 0) {
    return(NULL)
  }

  .plot <- .data %>%
    ggplot(aes(Entidad, `Cantidad de preguntas observadas`, fill = `Cantidad de preguntas observadas`)) +
    geom_col(na.rm = TRUE, show.legend = FALSE, alpha = 3/4, color = "black", linewidth = 0.1) +
    theme_classic() +
    labs(y = "Cantidad de preguntas observadas") +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1, size = 6),
      axis.title.x = element_blank()
    ) +
    scale_fill_gradient(low = "white", high = "blue")

  return(ggplotly(.plot, tooltip = c("x", "y")))
}


plot_entities_vs_obs_grid <- function(data, project) {

  .data <- data %>%
    filter(Censo == project) %>%
    count(Entidad, wt = `Cantidad de obs`, name = "Cantidad de preguntas observadas") %>%
    mutate(Entidad = fct_reorder(Entidad, `Cantidad de preguntas observadas`))

  if (nrow(.data) == 0) {
    return(NULL)
  }

  .plot <- .data %>%
    ggplot(aes(Entidad, `Cantidad de preguntas observadas`)) +
    geom_col(
      aes(fill = `Cantidad de preguntas observadas`),
      na.rm = TRUE,
      show.legend = FALSE,
      alpha = 3/4, color = "black", linewidth = 0.1
    ) +
    theme_classic() +
    labs(y = "Cantidad de preguntas observadas") +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1, size = 6),
      axis.title.x = element_blank()
    ) +
    scale_fill_gradient(low = "white", high = "blue")

  return(ggplotly(.plot, tooltip = c("x", "y")))
}
