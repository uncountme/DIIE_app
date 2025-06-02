# Plot function top ten questions by project and module
top_ten_questions <- function(data, project, module) {

  filter_observations <- function(x) {

    d <- .data %>%
      filter(str_detect(Observación, x)) %>%
      transmute(Observaciones = str_extract_all(Observación, regex(str_c(x, ".*"), multiline = TRUE))) %>%
      map(unlist) %>%
      as_tibble() %>%
      mutate(first_word = word(Observaciones, 1)) %>%
      filter(!str_detect(first_word, str_c(x, "\\d"))) %>%
      select(1)

    return(d)
  }

  .data <- data %>%
    filter(Censo == project) %>%
    mutate(Módulo = str_replace_all(str_sub(Folio, 4, 4),
                                    c("1" = "M1",
                                      "2" = "M2",
                                      "3" = "M3",
                                      "4" = "M4",
                                      "5" = "M5",
                                      "6" = "M6",
                                      "7" = "M7"))) %>%
    filter(Módulo == module) %>%
    transmute(Observación, Pregunta = str_extract_all(Observación, "(P\\d+\\.\\d+)|(Complemento\\s\\d+)|(Anexo\\s\\d+)"))

  if (nrow(.data) == 0) {
    return(NULL)
  }

  topten <- .data %>%
    select(2) %>%
    map(unlist) %>%
    as_tibble() %>%
    count(Pregunta) %>%
    rename(`Cantidad de obs` = names(.)[2]) %>%
    arrange(desc(`Cantidad de obs`)) %>%
    head(10)

  topten_observations <- map(topten[[1]], filter_observations) %>%
    reduce(full_join, "Observaciones")

  .plot <- topten %>%
    mutate(Pregunta = fct_inorder(factor(Pregunta))) %>%
    ggplot(aes(Pregunta, `Cantidad de obs`)) +
    geom_col(width = .7, fill = "#a71106") +
    ggtitle(label = str_c(module, project, sep = " ")) +
    labs(y = "Cantidad de observaciones") +
    theme_classic() +
    theme(
      axis.text.x  = element_text(angle = 30, hjust = 1, size = 8),
      axis.title.x = element_blank()
    )

  return(list(plot = ggplotly(.plot), dataframe = topten_observations))
}
