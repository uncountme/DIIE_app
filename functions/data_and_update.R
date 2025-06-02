# Reading and first sorting of data
data_and_update <- function(.file) {

  pre_data <- read_xlsx(.file, skip = 8)

  .update <- pre_data %>%
    tail(2) %>%
    .[[1, 2]]

  .data <- pre_data %>%
    head(-2) %>%
    purrr::discard(is_logical) %>%
    rename(Folio = names(.)[1]) %>%
    mutate(Registro = as.Date(dmy_hms(Registro))) %>%
    mutate(`Contador de días` = parse_double(`Contador de días`)) %>%
    mutate(`Cantidad de obs` = str_count(Observación, "(P\\d+\\.\\d+)|(Complemento\\s\\d+)|(Anexo\\s\\d+)")) %>% # verificar los dígitos en los complementos de los cuestioanrios
    mutate(Entidad = factor(Entidad, levels = levels(pull(entities)))) %>%
    mutate(Censo = str_replace_all(str_sub(Folio, 3, 3),
                                   c("1" = "CNGE",
                                     "2" = "CNSPE",
                                     "3" = "CNSIPEE",
                                     "4" = "CNPJE",
                                     "5" = "CNIJE",
                                     "6" = "CNPLE",
                                     "7" = "CNDHE",
                                     "8" = "CNTAIPPDPE" ))) %>%
    mutate(Censo = factor(Censo, levels = levels(pull(DIIE_dates, name))))

  return(list(.data, .update))
}
