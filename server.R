library(shiny)
library(tidyverse)
library(readxl)
library(plotly)
library(tools)
library(DT)
library(shinydashboard)
library(shinyauthr)
library(shinymanager)


function(input, output, session) {

  res_auth <- secure_server(
    check_credentials = check_credentials(application_user_base),
    timeout           = 20
  )

  auth <- callModule(
    module = auth_server,
    id     = "auth",
    check_credentials = check_credentials(application_user_base)
  )


# CNGAE 2023 --------------------------------------------------------------------

  data <- reactive({
    req(input$file_upload)
    ext <- file_ext(input$file_upload$datapath)
    feedbackDanger("file_upload", ext != "xlsx", "Extensión desconocida")

    if (!identical(ext, "xlsx")) {
      return(NULL)
    }

    req(identical(ext, "xlsx"))
    pre_data <- read_xlsx(input$file_upload$datapath)
    condition <- identical(names(pre_data)[2], "INSTITUTO NACIONAL DE ESTADÍSTICA Y GEOGRAFÍA") &&
      identical(pre_data[[3,2]],  "CNGAE 2024") &&
      identical(pre_data[[7,1]],  "Folio") &&
      identical(pre_data[[7,3]],  "Entidad") &&
      identical(pre_data[[7,5]],  "Usuario") &&
      identical(pre_data[[7,6]],  "Perfil") &&
      identical(pre_data[[7,8]],  "Registro") &&
      identical(pre_data[[7,10]], "Estatus") &&
      identical(pre_data[[7,12]], "Observación") &&
      identical(pre_data[[7,15]], "Contador de días")
    feedbackDanger("file_upload", !condition, "Archivo desconocido")

    if (!condition) {
      return(NULL)
    }

    req(condition)
    id <- showNotification(strong("Leyendo...",
                                  style = "color: #0323f5;font-size: 15px;font-style: italic;"),
                           type = "message", duration = NULL)
    on.exit(removeNotification(id), add = TRUE)

    database_2023     <- data_and_update(input$file_upload$datapath)[[1]]
    update_2023       <- data_and_update(input$file_upload$datapath)[[2]]
    database_obs_2023 <- team_data(reviewer_team, database_2023) %>%
      filter(`Cantidad de obs` > 0)

    return(list(database_2023, database_obs_2023, update_2023))
  })

  output$update <- renderText({
    req(data())
    str_c("Actualización ", data()[[3]])
  })

  hideTab(inputId = "id_navbar_current_year", target = "Observaciones")
  hideTab(inputId = "id_navbar_current_year", target = "Cuestionarios")
  hideTab(inputId = "id_navbar_current_year", target = "Interno")

  observeEvent(is_null(data()), {
    hideTab(inputId = "id_navbar_current_year", target = "Observaciones")
    hideTab(inputId = "id_navbar_current_year", target = "Cuestionarios")
    hideTab(inputId = "id_navbar_current_year", target = "Interno")
  })

  observeEvent(data(), {
    showTab(inputId = "id_navbar_current_year", target = "Observaciones")
    showTab(inputId = "id_navbar_current_year", target = "Cuestionarios")
    showTab(inputId = "id_navbar_current_year", target = "Interno")
  })

  # Info upload "Historial de seguimiento con extensión 'xlsx'".
  observeEvent(input$info_button_file_upload, {
    show_alert(
      session = session,
      title   = "",
      text    = tags$div(
        tags$h3("Información",
                style = "color: #0076C8; font-weight: bold; text-align: center"),
        tags$br(),
        tags$br(),
        tags$h4('Recuerda cargar el historial de seguimiento en formato “xlsx”',
               style = "font-weight: bold; text-align: center"),
        tags$br(),
        style = "text-align: justify;
        margin-left:  auto;
        margin-right: auto;",
        'El reporte',
        tags$b('Historial de seguimiento', style = "color: #0076C8"),
        'lo puedes descargar desde la página de IKTAN siguiendo estos pasos:',
        tags$br(),
        tags$br(),
        tags$ol(
          tags$li('Ingresa tus credenciales.'),
          tags$li('Selecciona tu perfil de acceso.'),
          tags$li('Selecciona la ventana “Reportes”.'),
          tags$br(),
          tags$img(src = "ventana_reportes.png" ,
                   `style` = "display: block;
                                                             margin-left: auto;
                                                             margin-right: auto;
                                                             width: 25%;"
          ),
          tags$br(),
          tags$li('Selecciona el recuadro “Reporte”.'),
          tags$li('Elige el formato “XLSX”.'),
          tags$li('Elige la opción “Historial de seguimiento”.'),
          tags$li('Presiona el botón aceptar.'),
          tags$li('Da clic en el archivo generado para descargarlo.'),
          tags$br(),
          tags$img(src = "select_reportes.png" ,
                   `style` = "display: block;
                                                             margin-left:  -4.3rem;
                                                             margin-right: auto;
                                                             width: 105%;"
          ),
          tags$br(),
          tags$li('En tu explorador de archivos localiza dónde se descargó
        la carpeta comprimida del reporte de seguimiento, generalmente
        la podrás encontrar en la carpeta de descargas.'),
          tags$li('Extrae el archivo de la carpeta comprimida.'),
          tags$li('¡Listo! Ya tienes el archivo que debes cargar en la aplicación.')
        )
      ),
      html  = TRUE,
      width = "55%"
    )
  })


# NAVBARMENU Observaciones ------------------------------------------------

# Top10 -------------------------------------------------------------------

  observeEvent(input$id_top_ten_question_2023, {
    updateTabsetPanel(session, "id_modulo_select_2023", selected = input$id_top_ten_question_2023)
  })

  reactive_top_ten_questions_2023 <- reactive({
    switch(input$id_top_ten_question_2023,
           CNGE   = switch(input$id_CNGE_2023,
                            `Módulo 1` = top_ten_questions(data()[[2]], "CNGE", "M1"),
                            `Módulo 2` = top_ten_questions(data()[[2]], "CNGE", "M2"),
                            `Módulo 3` = top_ten_questions(data()[[2]], "CNGE", "M3"),
                            `Módulo 4` = top_ten_questions(data()[[2]], "CNGE", "M4"),
                            `Módulo 5` = top_ten_questions(data()[[2]], "CNGE", "M5")
           ),
           CNSPE   = switch(input$id_CNSPE_2023,
                            `Módulo 1` = top_ten_questions(data()[[2]], "CNSPE", "M1"),
                            `Módulo 2` = top_ten_questions(data()[[2]], "CNSPE", "M2")
           ),
           CNSIPEE = switch(input$id_CNSIPEE_2023,
                            `Módulo 1` = top_ten_questions(data()[[2]], "CNSIPEE", "M1"),
                            `Módulo 2` = top_ten_questions(data()[[2]], "CNSIPEE", "M2")
           ),
           CNPJE   = switch(input$id_CNPJE_2023,
                            `Módulo 1` = top_ten_questions(data()[[2]], "CNPJE", "M1"),
                            `Módulo 2` = top_ten_questions(data()[[2]], "CNPJE", "M2"),
                            `Módulo 3` = top_ten_questions(data()[[2]], "CNPJE", "M3"),
                            `Módulo 4` = top_ten_questions(data()[[2]], "CNPJE", "M4"),
                            `Módulo 5` = top_ten_questions(data()[[2]], "CNPJE", "M5")
           ),
           CNIJE   = switch(input$id_CNIJE_2023,
                            `Módulo 1` = top_ten_questions(data()[[2]], "CNIJE", "M1"),
                            `Módulo 2` = top_ten_questions(data()[[2]], "CNIJE", "M2"),
                            `Módulo 3` = top_ten_questions(data()[[2]], "CNIJE", "M3"),
                            `Módulo 4` = top_ten_questions(data()[[2]], "CNIJE", "M4"),
                            `Módulo 5` = top_ten_questions(data()[[2]], "CNIJE", "M5"),
                            `Módulo 6` = top_ten_questions(data()[[2]], "CNIJE", "M6"),
                            `Módulo 7` = top_ten_questions(data()[[2]], "CNIJE", "M7")
           ),
           CNPLE = switch(input$id_CNPLE_2023,
                            `Módulo 1` = top_ten_questions(data()[[2]], "CNPLE", "M1"),
                            `Módulo 2` = top_ten_questions(data()[[2]], "CNPLE", "M2")
           ),
           CNDHE = switch(input$id_CNDHE_2023,
                          `Módulo 1` = top_ten_questions(data()[[2]], "CNDHE", "M1"),
                          `Módulo 2` = top_ten_questions(data()[[2]], "CNDHE", "M2")
           ),
           CNTAIPPDPE = switch(input$id_CNTAIPPDPE_2023,
                          `Módulo 1` = top_ten_questions(data()[[2]], "CNTAIPPDPE", "M1"),
                          `Módulo 2` = top_ten_questions(data()[[2]], "CNTAIPPDPE", "M2"),
                          `Módulo 3` = top_ten_questions(data()[[2]], "CNTAIPPDPE", "M3")
           )
    )
  })

  output$plot_top_ten_questions_2023 <- renderPlotly({
    validate(need(reactive_top_ten_questions_2023()[[1]], "Sin observaciones"))
    reactive_top_ten_questions_2023()[[1]]
  })

  output$table_top_ten_questions_2023 <- renderDataTable({
    validate(need(reactive_top_ten_questions_2023()[[1]], ""))
    datatable(
      reactive_top_ten_questions_2023()[[2]],
      rownames = FALSE,
      filter   = list(position = "top"),#"top",
      options  = list(
        dom      = "ltipr",
        language = list(
          url = "https://cdn.datatables.net/plug-ins/2.1.8/i18n/es-MX.json"
        )
      )
    )
  })


# NAVBARMENU Cuestionarios ------------------------------------------------

# Cuestionarios "REVISIÓN OC" ---------------------------------------------

  database_questionnaires <- reactive({

    vector_folios_no_aplica <- DT_folio_no_aplica(data()[[1]]) %>%
      pull()

    .data <- data()[[1]] %>%
      filter(str_detect(Estatus, "Revisión OC"), Perfil == "RESPONSABLE OPERATIVO") %>%
      left_join(working_dates, by = "Registro") %>% # Se modificó "Registro" para considerar solo días hábiles.
      select(-Registro) %>%
      rename(Registro = aux_var) %>%
      filter(!(Folio %in% vector_folios_no_aplica))


    if (nrow(.data) == 0) {
      return(NULL)
    }

    return(.data)
  })


# Cuestionarios enviados a revisión OC --------------------------------------------------------

  output$text_count_questionnaires_2023 <- renderText({
    validate(need(database_questionnaires(), "0"))
    count_arrival_questionnaires_week(database_questionnaires(), input$id_slider_date_questionnaires_2023)
  })

  output$plot_arrival_questionnaires <- renderPlotly({
    validate(need(database_questionnaires(), "Sin información"))
    x <- input$id_slider_date_questionnaires_2023
    plot_arrival_questionnaires_current_year(database_questionnaires(), x)
  })


# Comparativo global 2023 VS 2024 -------------------------------------------------------

  output$text_count_questionnaires_weeks_2023 <- renderText({
    validate(need(database_questionnaires(), "0"))
    x <- input$id_slider_date_questionnaires_weeks
    count_arrival_questionnaires_week(database_questionnaires(), ymd("2024-02-19") + weeks(x)) # (update every year!).
  })

  output$text_count_questionnaires_weeks_2022 <- renderText({
    x <- input$id_slider_date_questionnaires_weeks
    count_arrival_questionnaires_week(database_questionnaires_previous_year, ymd("2023-03-06") + weeks(x)) # (update every year!).
  })

  output$plot_arrival_questionnaires_weeks <- renderPlotly({
    validate(need(database_questionnaires(), "Sin información"))
    x <- input$id_slider_date_questionnaires_weeks
    plot_arrival_questionnaires_current_year(database_questionnaires(), ymd("2024-02-19") + weeks(x), "2024") # (update every year!).
  })

  output$plot_arrival_questionnaires_weeks_previous_year <- renderPlotly({
    x <- input$id_slider_date_questionnaires_weeks
    plot_arrival_questionnaires_previous_year(database_questionnaires_previous_year, ymd("2023-03-06") + weeks(x), "2023") # (update every year!).
  })


# Cuestionarios enviados a OC por entidad -------------------------

  max_questionnaries_day <- reactive({
    req(database_questionnaires())
    database_questionnaires() %>%
      count(Registro, Censo) %>%
      rename(`Cuestionarios enviados a revisión OC` = names(.)[[3]]) %>%
      group_by(Censo) %>%
      summarise(y_max = max(`Cuestionarios enviados a revisión OC`))
  })

  reactive_arrival_questionnaires_entitie <- reactive({
    req(max_questionnaries_day())
    switch (input$id_questionnaires_vs_entities_2023,
            NACIONAL                          = plot_arrival_questionnaires_grid_census_2023(database_questionnaires(), max_questionnaries_day()),
            `AGUASCALIENTES`                  = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[1]),
            `BAJA CALIFORNIA`                 = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[2]),
            `BAJA CALIFORNIA SUR`             = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[3]),
            `CAMPECHE`                        = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[4]),
            `COAHUILA DE ZARAGOZA`            = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[5]),
            `COLIMA`                          = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[6]),
            `CHIAPAS`                         = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[7]),
            `CHIHUAHUA`                       = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[8]),
            `CIUDAD DE MÉXICO`                = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[9]),
            `DURANGO`                         = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[10]),
            `GUANAJUATO`                      = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[11]),
            `GUERRERO`                        = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[12]),
            `HIDALGO`                         = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[13]),
            `JALISCO`                         = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[14]),
            `MÉXICO`                          = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[15]),
            `MICHOACÁN DE OCAMPO`             = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[16]),
            `MORELOS`                         = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[17]),
            `NAYARIT`                         = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[18]),
            `NUEVO LEÓN`                      = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[19]),
            `OAXACA`                          = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[20]),
            `PUEBLA`                          = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[21]),
            `QUERÉTARO`                       = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[22]),
            `QUINTANA ROO`                    = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[23]),
            `SAN LUIS POTOSÍ`                 = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[24]),
            `SINALOA`                         = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[25]),
            `SONORA`                          = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[26]),
            `TABASCO`                         = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[27]),
            `TAMAULIPAS`                      = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[28]),
            `TLAXCALA`                        = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[29]),
            `VERACRUZ DE IGNACIO DE LA LLAVE` = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[30]),
            `YUCATÁN`                         = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[31]),
            `ZACATECAS`                       = plot_arrival_questionnaires_entities_2023(database_questionnaires(), levels(entities[[1]])[32])
    )
  })

  output$plot_arrival_questionnaires_entitie_2023 <- renderPlotly({
    validate(need(reactive_arrival_questionnaires_entitie(), "Sin cuestionarios enviados a revisión OC"))
    reactive_arrival_questionnaires_entitie()
  })


# Actualización ----------------------------------------------

  output$database_original <- renderDataTable({
    datatable((data()[[1]])[, input$id_columns_data, drop = FALSE],
              rownames   = FALSE,
              filter     = "top",
              options    = list(
                pageLength  = 5,
                language    = list(
                  url = "https://cdn.datatables.net/plug-ins/2.1.8/i18n/es-MX.json"
                )
              )
    )
  })


# Cuestionarios "FIRMA Y SELLO" --------------------------------------------

  # Database on questionnaires with status "firma y sello(1)"
  database_firma_sello <- reactive({

    vector_folios_no_aplica <- DT_folio_no_aplica(data()[[1]]) %>%
      pull()

    .data <- data()[[1]] %>%
      filter(str_detect(Estatus, "En proceso de firma y sello \\(1\\)")) %>%
      left_join(working_dates, by = "Registro") %>% # Se modificó "Registro" para considerar solo días hábiles.
      select(-Registro) %>%
      rename(Registro = aux_var) %>%
      filter(!(Folio %in% vector_folios_no_aplica))

    if (nrow(.data) == 0) {
      return(NULL)
    }

    return(.data)
  })


# Cuestionarios en proceso de firma y sello (1) ---------------------------
  # Change control on sidebar panel: DT and plots by "semana" and "día".
  observeEvent(input$id_controller_plot_semana_day, {
    updateTabsetPanel(session, "id_plot_questionnaries_firma_sello_select", input$id_controller_plot_semana_day)
  })

  # Change control on main panel: DT and plots by "semana" and "día".
  observeEvent(input$id_controller_plot_semana_day, {
    updateTabsetPanel(session, "id_plot_DT_questionnaries_firma_sello_select", input$id_controller_plot_semana_day)
  })

  # Observe event plot global and plot by census and DT-
  observeEvent(input$id_questionnaires_firma_sello_census, {
    if (input$id_questionnaires_firma_sello_census == "GLOBAL") {
      return(updateTabsetPanel(session, inputId = "id_plot_DT", selected = "global"))
    }
    return(updateTabsetPanel(session, inputId = "id_plot_DT", selected = "census"))
  })

  # Plot global
  reactive_questionnaires_firma_sello_week_global <- reactive({
    req(database_firma_sello())
    plot_questionnaires_firma_sello_week(database_firma_sello())
  })

  output$plot_questionnaires_firma_sello_week_global <- renderPlotly({
    validate(need(reactive_questionnaires_firma_sello_week_global(),
                  "Sin cuestionarios en proceso de firma y sello (1)"))
    reactive_questionnaires_firma_sello_week_global()
  })

  # Plot and DT by census
  reactive_questionnaires_firma_sello_week_census <- reactive({
    req(database_firma_sello())
    plot_questionnaires_firma_sello_week_project(database_firma_sello(), input$id_questionnaires_firma_sello_census)
  })

  output$plot_questionnaires_firma_sello_week_census <- renderPlotly({
    validate(need(reactive_questionnaires_firma_sello_week_census(),
                  "Sin cuestionarios en proceso de firma y sello (1)"))
    reactive_questionnaires_firma_sello_week_census()
  })

  output$table_questionnaires_set_free_census <- renderDataTable({
    validate(need(reactive_questionnaires_firma_sello_week_census(), ""))
    datatable(
      DT_questionnaires_firma_sello_census(database_firma_sello(), input$id_questionnaires_firma_sello_census),
      rownames = FALSE,
      options  = list(
        pageLength = 5,
        language   = list(
          url = "https://cdn.datatables.net/plug-ins/2.1.8/i18n/es-MX.json"
        )
      )
    )
  })

  # Count set free questionnaires by time range
  output$text_count_firma_sello_census <- renderText({
    count_questionnaires_firma_sello(
      database_firma_sello(),
      .census = input$id_questionnaires_firma_sello_census,
      .max    = dmy_hms(data()[[3]]) %>% as_date()
    )
  })

  # Update sliderInput; change value
  observeEvent(!is_null(data()), {
    updateSliderInput(
      inputId = "id_slider_date_questionnaires_firma_sello",
      max = dmy_hms(data()[[3]]) %>% as_date(),
      value = c(
        DIIE_dates[[3, 2]],
        dmy_hms(data()[[3]]) %>% as_date()
      )
    )
  })

  # Change value of count set free questionnaires by day
  observeEvent(input$id_slider_date_questionnaires_firma_sello, {
    if ((input$id_slider_date_questionnaires_firma_sello)[1] == DIIE_dates[[3, 2]] &&
        (input$id_slider_date_questionnaires_firma_sello)[2] == dmy(word(data()[[3]], 1))) {
      return(updateTabsetPanel(session, inputId = "id_text_questionnaires_firma_sello_range", selected = "accumulated"))
    }
    return(updateTabsetPanel(session, inputId = "id_text_questionnaires_firma_sello_range", selected = "range"))
  })

  output$text_count_firma_sello_accumulated <- renderText({
    count_questionnaires_firma_sello(
      database_firma_sello(),
      .census = "GLOBAL",
      .max    = (input$id_slider_date_questionnaires_firma_sello)[2]
    )
  })

  output$text_count_firma_sello_range <- renderText({
    count_questionnaires_firma_sello(
      database_firma_sello(),
      .census = "GLOBAL",
      .min    = (input$id_slider_date_questionnaires_firma_sello)[1],
      .max    = (input$id_slider_date_questionnaires_firma_sello)[2]
    )
  })

  # Plots on questionnnaries set free everyday.
  output$plot_questionnaires_firma_sello_day <- renderPlotly({
    req(database_firma_sello())
    plot_questionnaires_firma_sello(database_firma_sello())
  })

  output$table_questionnaires_set_free_registro <- renderDataTable({
    req(database_firma_sello())
    datatable(
      DT_questionnaires_firma_sello_registro(database_firma_sello()),
      rownames = FALSE,
      filter   = "top",
      options  = list(
        pageLength = 5,
        language   = list(
          url = "https://cdn.datatables.net/plug-ins/2.1.8/i18n/es-MX.json"
        )
      ),
      escape = FALSE
    )
  })

# Cuestionarios en proceso de firma y sello (1) por entidad ------------
  reactive_questionnaires_firma_sello_entity <- reactive({
    req(database_firma_sello())
    if (input$id_questionnaires_firma_sello_entity == "NACIONAL") {
      return(plot_questionnaires_firma_sello_light(database_firma_sello()))
    }

    plot_questionnaires_firma_sello_light_entities(database_firma_sello(), input$id_questionnaires_firma_sello_entity)
  })

  output$plot_questionnaires_firma_sello_entity <- renderPlotly({
    validate(need(reactive_questionnaires_firma_sello_entity(), "Sin cuestionarios en proceso de firma y sello (1)"))
    reactive_questionnaires_firma_sello_entity()
  })


# NAVBARMENU Evaluación ---------------------------------------------------

  observeEvent(input$id_select_panel_entity, {
    updateTabsetPanel(session, inputId = "id_tab_evaluacion", selected = input$id_select_panel_entity)
  })

  # Data base for DOE analisys.
  database_DOE <- reactive(write_database_DOE(data()[[1]]))

  # Data base "evolución del levantamiento".
  database_evolucion_levantamiento <- reactive(DT_evolucion_levantamiento(database_DOE()))

  # Plot evolución del levantamiento.
  output$plot_evolucion_levantamiento <- renderPlotly({

    switch (input$id_select_panel_entity,
            "Nacional" = plot_DT_evolucion_levantamiento(
              database_evolucion_levantamiento(),
              input$id_census_evaluation_DOE, levels(federal_entities[["Entidad"]])
            )[["p"]],

            "Regional" = plot_DT_evolucion_levantamiento(
              database_evolucion_levantamiento(),
              input$id_census_evaluation_DOE,
              federal_entities %>% filter(Regional == input$id_regional_evaluation_DOE) %>% pull(Entidad) %>% as.character()
            )[["p"]],

            "Estatal" = plot_DT_evolucion_levantamiento(
              database_evolucion_levantamiento(),
              input$id_census_evaluation_DOE,
              input$id_entity_evaluation_DOE
            )[["p"]]
    )

  })

  # Info evolución del levantamiento.
  observeEvent(input$info_button_evolucion_levantamiento, {
    show_alert(
      session = session,
      title   = "",
      text    = tags$div(
        tags$h3("Información",
                style = "color: #0076C8; font-weight: bold; text-align: center"),
        tags$br(),
        tags$br(),
        `style` = "text-align: justify;
        margin-left:  auto;
        margin-right: auto;",
        "Se muestra el avance porcentual de cumplimiento de
        las actividades 3.2.3.1, 3.2.3.2, 3.2.3.3 y 3.2.4; los meses
        parametrizan el porcentaje de cuestionarios que han
        cumplido con cada Actividad (fase).",
        tags$br(),
        tags$br(),
        "La categoría “PENDIENTES” refiere al porcentaje de
        cuestionarios que no han cumplido con la respectiva Actividad (fase).",
        tags$br(),
        tags$br(),
        dataTableOutput("info_DT_relacion_actividad_estatus")
      ),
      html  = TRUE,
      width = "62%"
    )
  })

  output$info_DT_relacion_actividad_estatus <- renderDataTable(
    datatable(
      relacion_actividad_fase %>%
        mutate(
          `Actividades (fases)` = str_c(c("3.2.3.1 ", "3.2.3.2 ", "3.2.3.3 ", "3.2.4   "), `Actividades (fases)`)
        ),
      options = list(
        dom = 't'
      ),
      rownames = FALSE
    )
  )

  # Data table evolución del levantamiento.
  output$table_evolucion_levantamiento <- renderDataTable({

    switch (input$id_select_panel_entity,
            "Nacional" = plot_DT_evolucion_levantamiento(
              database_evolucion_levantamiento(),
              input$id_census_evaluation_DOE,
              levels(federal_entities[["Entidad"]])
            )[["dframe"]] %>%
              datatable(
                options = list(
                  dom      = 't',
                  language = list(
                    url = "https://cdn.datatables.net/plug-ins/2.1.8/i18n/es-MX.json"
                  )
                ),
                rownames = FALSE
              ),

            "Regional" = plot_DT_evolucion_levantamiento(
              database_evolucion_levantamiento(),
              input$id_census_evaluation_DOE,
              federal_entities %>% filter(Regional == input$id_regional_evaluation_DOE) %>% pull(Entidad) %>% as.character()
            )[["dframe"]] %>%
              datatable(
                options = list(
                  dom      = 't',
                  language = list(
                    url = "https://cdn.datatables.net/plug-ins/2.1.8/i18n/es-MX.json"
                  )
                ),
                rownames = FALSE
              ),

            "Estatal" = plot_DT_evolucion_levantamiento(
              database_evolucion_levantamiento(),
              input$id_census_evaluation_DOE,
              input$id_entity_evaluation_DOE
            )[["dframe"]] %>%
              datatable(
                options = list(
                  dom      = 't',
                  language = list(
                    url = "https://cdn.datatables.net/plug-ins/2.1.8/i18n/es-MX.json"
                  )
                ),
                rownames = FALSE
              )
    )

  })

  # Plot "Recuperación oportuna de la información".
  output$plot_recuperacion_informacion_oportuna <- renderPlotly({

    switch (input$id_select_panel_entity,
            "Nacional" = plot_DT_recuperacion_oportuna_informacion(
              database_evolucion_levantamiento(),
              input$id_census_evaluation_DOE,
              levels(federal_entities[["Entidad"]])
            )$plot,

            "Regional" = plot_DT_recuperacion_oportuna_informacion(
              database_evolucion_levantamiento(),
              input$id_census_evaluation_DOE,
              federal_entities %>% filter(Regional == input$id_regional_evaluation_DOE) %>% pull(Entidad) %>% as.character()
            )$plot,

            "Estatal" = plot_DT_recuperacion_oportuna_informacion(
              database_evolucion_levantamiento(),
              input$id_census_evaluation_DOE,
              input$id_entity_evaluation_DOE
            )$plot
    )

  })

  # Info Recuperación oportuna de la información.
  observeEvent(input$info_button_recuperacion_informacion, {
    show_alert(
      session = session,
      title   = "",
      text    = tags$div(
        tags$h3("Información",
                style = "color: #0076C8; font-weight: bold; text-align: center"),
        tags$br(),
        tags$br(),
        `style` = "text-align: justify;
        margin-left:  auto;
        margin-right: auto;",
        "La gráfica escinde el cumplimiento de cada actividad (fase) en tres categorías:
        “En tiempo”, “Fuera de meta” y “PENDIENTES”.  El parámetro de referencia
        para cada censo es la fecha límite de la actividad marcada en la planeación.",
        tags$br(),
        tags$br(),
        "La categoría “PENDIENTES” refiere al porcentaje de
        cuestionarios que no han cumplido con la respectiva Actividad (fase)."
      ),
      html  = TRUE,
      width = "50%"
    )
  })

  # DT "Recuperación oportuna de la información".
  output$table_recuperacion_informacion <- renderDataTable({

    switch (input$id_select_panel_entity,
            "Nacional" = plot_DT_recuperacion_oportuna_informacion(
              database_evolucion_levantamiento(),
              input$id_census_evaluation_DOE,
              levels(federal_entities[["Entidad"]])
            )$df %>%
              datatable(
                rownames = FALSE,
                filter   = "top",
                options  = list(
                  language = list(
                    url = "https://cdn.datatables.net/plug-ins/2.1.8/i18n/es-MX.json"
                  )
                )
              ),

            "Regional" = plot_DT_recuperacion_oportuna_informacion(
              database_evolucion_levantamiento(),
              input$id_census_evaluation_DOE,
              federal_entities %>% filter(Regional == input$id_regional_evaluation_DOE) %>% pull(Entidad) %>% as.character()
            )$df %>%
              datatable(
                rownames = FALSE,
                filter   = "top",
                options  = list(
                  language = list(
                    url = "https://cdn.datatables.net/plug-ins/2.1.8/i18n/es-MX.json"
                  )
                )
              ),

            "Estatal" = plot_DT_recuperacion_oportuna_informacion(
              database_evolucion_levantamiento(),
              input$id_census_evaluation_DOE,
              input$id_entity_evaluation_DOE
            )$df %>%
              datatable(
                rownames = FALSE,
                filter   = "top",
                options  = list(
                  language = list(
                    url = "https://cdn.datatables.net/plug-ins/2.1.8/i18n/es-MX.json"
                  )
                )
              )
    )

  })

  # Plot "Concertación de citas y entrega de cuestionarios".
  output$plot_concertacion_entrega <- renderPlot({

    file_citas_agendadas <- "citas_agendadas/xIktan_20231013114346215_citasAgendadas.xlsx"

    switch (input$id_select_panel_entity,
            "Nacional" = plot_citas_cuestionarios(
              file_citas_agendadas,
              input$id_census_evaluation_DOE,
              levels(federal_entities[["Entidad"]])
            )$plot,

            "Regional" = plot_citas_cuestionarios(
              file_citas_agendadas,
              input$id_census_evaluation_DOE,
              federal_entities %>% filter(Regional == input$id_regional_evaluation_DOE) %>% pull(Entidad) %>% as.character()
            )$plot,

            "Estatal" = plot_citas_cuestionarios(
              file_citas_agendadas,
              input$id_census_evaluation_DOE,
              input$id_entity_evaluation_DOE
            )$plot
    )

  })

  # DT "Concertación de citas y entrega de cuestionarios".
  output$table_concertacion_entrega <- renderDataTable({

    file_citas_agendadas <- "citas_agendadas/xIktan_20231013114346215_citasAgendadas.xlsx"

    switch (input$id_select_panel_entity,
            "Nacional" = plot_citas_cuestionarios(
              file_citas_agendadas,
              input$id_census_evaluation_DOE,
              levels(federal_entities[["Entidad"]])
            )$df %>%
              datatable(
                rownames = FALSE,
                filter   = "top",
                options  = list(
                  language = list(
                    url = "https://cdn.datatables.net/plug-ins/2.1.8/i18n/es-MX.json"
                  )
                )
              ),

            "Regional" = plot_citas_cuestionarios(
              file_citas_agendadas,
              input$id_census_evaluation_DOE,
              federal_entities %>% filter(Regional == input$id_regional_evaluation_DOE) %>% pull(Entidad) %>% as.character()
            )$df %>%
              datatable(
                rownames = FALSE,
                filter   = "top",
                options  = list(
                  language = list(
                    url = "https://cdn.datatables.net/plug-ins/2.1.8/i18n/es-MX.json"
                  )
                )
              ),

            "Estatal" = plot_citas_cuestionarios(
              file_citas_agendadas,
              input$id_census_evaluation_DOE,
              input$id_entity_evaluation_DOE
            )$df %>%
              datatable(
                rownames = FALSE,
                filter   = "top",
                options  = list(
                  language = list(
                    url = "https://cdn.datatables.net/plug-ins/2.1.8/i18n/es-MX.json"
                  )
                )
              )
    )

  })

  # Info Concertación de citas y entrega de cuestionarios.
  observeEvent(input$info_button_citas_cuestionarios, {
    show_alert(
      session = session,
      title   = "",
      text    = tags$div(
        tags$h3("Información",
                style = "color: #0076C8; font-weight: bold; text-align: center"),
        tags$br(),
        tags$br(),
        `style` = "text-align: center;
        margin-left:  auto;
        margin-right: auto;",
        'Análisis generado del reporte "Citas agendadas".',
        tags$br(),
        tags$br(),
        "Corte de información: 13/10/2023 11:43 hrs."
      ),
      html  = TRUE,
      width = "35%"
    )
  })

  # Plot "Intervalos promedio de recuperación".
  output$plot_promedio_recuperacion <- renderPlotly({

    switch (input$id_select_panel_entity,
            "Nacional" = plot_intervalo_recuperacion(
              database_evolucion_levantamiento(),
              input$id_census_evaluation_DOE,
              levels(federal_entities[["Entidad"]])
            ),

            "Regional" = plot_intervalo_recuperacion(
              database_evolucion_levantamiento(),
              input$id_census_evaluation_DOE,
              federal_entities %>% filter(Regional == input$id_regional_evaluation_DOE) %>% pull(Entidad) %>% as.character()
            ),

            "Estatal" = plot_intervalo_recuperacion(
              database_evolucion_levantamiento(),
              input$id_census_evaluation_DOE,
              input$id_entity_evaluation_DOE
            )
    )

  })

  # Info Intervalos promedio de recuperación.
  observeEvent(input$info_button_intervalos_recuperacion, {
    show_alert(
      session = session,
      title   = "",
      text    = tags$div(
        tags$h3("Información",
                style = "color: #0076C8; font-weight: bold; text-align: center"),
        tags$br(),
        tags$br(),
        `style` = "text-align: justify;
        margin-left:  auto;
        margin-right: auto;",
        "Las gráficas muestran por cuestionario el número de días que tomó recuperar la
        información desde el inicio proyectado en la
        planeación de cada censo, hasta el cumplimiento de cada actividad (fase).",
        tags$br(),
        "A nivel regional se promedian los resultados obtenidos en cada módulo por las
        entidades involucradas.",
        tags$br(),
        tags$br(),
        "Abreviaturas:",
        tags$br(),
        "Mín: mínimo de días a nivel nacional que tomó recuperar la información.",
        tags$br(),
        "Máx: máximo de días a nivel nacional que tomó recuperar la información.",
        tags$br(),
        "µ: promedio de días a nivel nacional que tomó recuperar la información."
      ),
      html  = TRUE,
      width = "45%"
    )
  })

  # Plot "Revisiones realizadas a los cuestionarios".
  reactive_revisiones_realizadas <- reactive({
    req(database_DOE())
    switch (input$id_select_panel_entity,
            "Nacional" = plot_revisiones_cuestionarios(
              database_DOE(),
              input$id_census_evaluation_DOE,
              levels(federal_entities[["Entidad"]])
            ),

            "Regional" = plot_revisiones_cuestionarios(
              database_DOE(),
              input$id_census_evaluation_DOE,
              federal_entities %>% filter(Regional == input$id_regional_evaluation_DOE) %>% pull(Entidad) %>% as.character()
            ),

            "Estatal" = plot_revisiones_cuestionarios(
              database_DOE(),
              input$id_census_evaluation_DOE,
              input$id_entity_evaluation_DOE
            )
    )
  })

  output$plot_revisiones_realizadas <- renderPlotly({
    validate(need(reactive_revisiones_realizadas(), "Sin información"))
    reactive_revisiones_realizadas()
  })

  # Info Revisiones realizadas a los cuestionarios.
  observeEvent(input$info_button_revisiones_cuestionarios, {
    show_alert(
      session = session,
      title   = "",
      text    = tags$div(
        tags$h3("Información",
                style = "color: #0076C8; font-weight: bold; text-align: center"),
        tags$br(),
        tags$br(),
        `style` = "text-align: justify;
        margin-left:  auto;
        margin-right: auto;",
        "La gráfica muestra por cuestionario el número de revisiones que se han efectuado.",
        tags$br(),
        "A nivel regional se promedian los resultados obtenidos en cada módulo por las
        entidades involucradas.",
        tags$br(),
        tags$br(),
        "Abreviaturas:",
        tags$br(),
        "Mín: mínimo de revisiones a nivel nacional.",
        tags$br(),
        "Máx: máximo de revisiones a nivel nacional.",
        tags$br(),
        "µ: promedio de revisiones a nivel nacional."
      ),
      html  = TRUE,
      width = "31%"
    )
  })

  # Plot "Duración del levantamiento por etapas".
  output$plot_levantamiento_estapas <- renderPlotly({

    switch (input$id_select_panel_entity,
            "Nacional" = plot_duracion_levantamiento_etapas(
              database_DOE(),
              input$id_census_evaluation_DOE,
              levels(federal_entities[["Entidad"]])
            ),

            "Regional" = plot_duracion_levantamiento_etapas(
              database_DOE(),
              input$id_census_evaluation_DOE,
              federal_entities %>% filter(Regional == input$id_regional_evaluation_DOE) %>% pull(Entidad) %>% as.character()
            ),

            "Estatal" = plot_duracion_levantamiento_etapas(
              database_DOE(),
              input$id_census_evaluation_DOE,
              input$id_entity_evaluation_DOE
            )
    )

  })

  # Info Duración del levantamiento por etapas.
  observeEvent(input$info_button_levantamiento_estapas, {
    show_alert(
      session = session,
      title   = "",
      text    = tags$div(
        tags$h3("Información",
                style = "color: #0076C8; font-weight: bold; text-align: center"),
        tags$br(),
        tags$br(),
        `style` = "text-align: justify;
        margin-left:  auto;
        margin-right: auto;",
        "La gráfica muestra el promedio de días hábiles que duró cada etapa del levantamiento,
        las cuales están conformadas por los estatus del historial de seguimiento.",
        tags$br(),
        "Las barras con valores negativos denotan que el comienzo y cumplimiento
        de la actividad se realizó antes de la fecha de inicio considerada en la planeación.",
        tags$br(),
        tags$br(),
        dataTableOutput("info_DT_levantamiento_estapas")
      ),
      html  = TRUE,
      width = "70%"
    )
  })

  output$info_DT_levantamiento_estapas <- renderDataTable(
    datatable(
      tibble(
        Etapa = c(
          "Proceso de llenado",
          "Revisión ROCE",
          "Validación OC",
          "Aclaración de información OC",
          "Revisión ROCE derivado de OC",
          "Recuperado con firma y sello"
        ),
        Estatus = c(
          "En proceso de llenado, Aclaración de información parcial y Recuperacion parcial",
          "Revisión ROCE y Aclaración de información (Revisión ROCE)",
          "Revisión OC",
          "Aclaración de información OC y Aclaración ROCE derivado de OC",
          "Revisión ROCE derivado de OC",
          "En proceso de firma y sello, Recuperado con firma y sello,
          En proceso de firma y sello por reconsulta, En aclaración por reconsulta y
          Recuperado con firma y sello por reconsulta"
        )
      ),
      options = list(
        dom = 't',
        columnDefs = list(list(width = '250px', targets = c(0)))
      ),
      rownames = FALSE
    )
  )


# INTERNO --------------------------------------------------------------

  credentials <- loginServer(
    id       = "login",
    data     = DIIE_user_base,
    user_col = user,
    pwd_col  = password,
    log_out  = reactive(logout_init())
  )

  logout_init <- logoutServer(
    id     = "logout",
    active = reactive(credentials()$user_auth)
  )

  output$diie_interno <- renderUI({
    req(credentials()$user_auth)
    tabsetPanel(
      type = "pills",
      tabPanel(
        "Revisiones por OC",
        br(),
        br(),
        h4(
          p(strong("Tabulado de revisiones efectuadas por OC")),
          style = "color: #3c8dbc; margin: 0rem; margin-top: -1rem; margin-bottom: 3rem;"
        ),
        p(strong("NA: "), "cuestionario no aplica"),
        p(strong("NR: "), "cuestionario no revisado"),
        br(),
        DTOutput("table_q_aclaracion_oc"),
        br(),
        br(),
        br(),
        br(),
        DTOutput("data_q_aclaracion_oc"),
        br(),
        br(),
        br(),
        br()
      ),
      tabPanel(
        "Ranking de entidades por preguntas observadas",
        br(),
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 2,
            radioButtons(
              "id_obs_vs_census_2023",
              "Nivel de análisis",
              choices = c("GLOBAL", levels(DIIE_dates[[1]]))
            )
          ),
          mainPanel(
            style = "height: 500px",
            width = 10,
            actionBttn(
              inputId = "info_button_obs_enviadas_OC",
              label   = "",
              icon    = icon("info-circle"),
              style   = "jelly"
            ),
            br(),
            br(),
            plotlyOutput(
              "plot_obs_vs_census_2023"
            )
          )
        )
      )
    )
  })

  # Ranking entidades
  reactive_obs_vs_census_2023 <- reactive({
    switch (input$id_obs_vs_census_2023,
            GLOBAL     = plot_entities_vs_obs(data()[[2]]),
            CNGE       = plot_entities_vs_obs_grid(data()[[2]], "CNGE"),
            CNSPE      = plot_entities_vs_obs_grid(data()[[2]], "CNSPE"),
            CNSIPEE    = plot_entities_vs_obs_grid(data()[[2]], "CNSIPEE"),
            CNPJE      = plot_entities_vs_obs_grid(data()[[2]], "CNPJE"),
            CNIJE      = plot_entities_vs_obs_grid(data()[[2]], "CNIJE"),
            CNPLE      = plot_entities_vs_obs_grid(data()[[2]], "CNPLE"),
            CNDHE      = plot_entities_vs_obs_grid(data()[[2]], "CNDHE"),
            CNTAIPPDPE = plot_entities_vs_obs_grid(data()[[2]], "CNTAIPPDPE")
    )
  })

  # Ranking entidades
  output$plot_obs_vs_census_2023 <- renderPlotly({
    req(credentials()$user_auth)
    validate(need(reactive_obs_vs_census_2023(), "Sin observaciones"))
    reactive_obs_vs_census_2023()
  })

  # Info Ranking entidades.
  observeEvent(input$info_button_obs_enviadas_OC, {
    req(credentials()$user_auth)
    show_alert(
      session = session,
      title   = "",
      text    = tags$div(
        tags$h3("Información",
                style = "color: #0076C8; font-weight: bold; text-align: center"),
        tags$br(),
        tags$br(),
        `style` = "text-align: justify;
        margin-left:  auto;
        margin-right: auto;",
        "La gráfica muestra por Censo el ranking de Entidades
        respecto a la cantidad de preguntas que fueron observadas exclusivamente
        por los responsables de revisión de OC."
      ),
      html  = TRUE,
      width = "35%"
    )
  })

  # Table questionnaires aclaracion oc
  output$table_q_aclaracion_oc <- renderDT({
    req(credentials()$user_auth)
    db_q_aclaracion_oc(data()[[1]], c("8101", "8201", "8301"))$datatable
  }, server = FALSE)

  output$data_q_aclaracion_oc = renderDT({
    req(credentials()$user_auth)

    db_q_aclaracion_oc_filter(
      db_q_aclaracion_oc(data()[[1]], c("8101", "8201", "8301"))$data,
      db_q_aclaracion_oc(data()[[1]], c("8101", "8201", "8301"))$table,
      input$table_q_aclaracion_oc_cells_selected
    ) %>%
      datatable(
        rownames   = FALSE,
        selection  = list(target = "cell"),
        extensions = c("Buttons", "FixedHeader"),
        options    = list(
          ordering    = FALSE,
          pageLength  = 10,
          fixedHeader = TRUE,
          dom         = "Blftip",
          buttons     = list(
            list(
              extend           = "colvis",
              text             = "Visibilidad de columnas",
              columns          = c(1:5, 7:9),
              collectionLayout = "fixed columns",
              popoverTitle     = "Control de visibilidad de columnas"
            )
          ),
          language    = list(
            url = "https://cdn.datatables.net/plug-ins/2.1.8/i18n/es-MX.json"
          ),
          columnDefs  = list(
            list(visible = FALSE, targets = c(1:3, 7:9))
          )
        )
      )
  })

}
