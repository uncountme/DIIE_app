library(tidyverse)
library(readxl)
library(plotly)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(shiny)
library(shinyFeedback)
library(DT)
library(shinyfullscreen)
library(shinyauthr)
library(shinymanager)
library(shinythemes)


source("contact/contact.R")

secure_app(
  theme    = shinytheme("flatly"),
  language = "es",
  id       = "auth",
  tags_top = tags$div(
    tags$h4(
      "DIIE app",
      style = "align:center;color:#1e4a75;font-weight:bold;"
    ),
    tags$img(
      src = "logo.png", width = 250
    )
  ),
  tags_bottom = tags$div(
    tags$p(
      "Cualquier pregunta, por favor contacte al ",
      tags$a(
        href = str_c(
          "mailto:", contact_email, "?Subject=DIIE%20app%20autenticación"
        ),
        target ="_top", "administrator"
      )
    )
  ),
  lan = use_language("es"),
dashboardPage(

# dashboard Header --------------------------------------------------------

  dashboardHeader(
    title="DIIE app",
    titleWidth = 130,
    tags$li(
      class = "dropdown",
      actionBttn("page_full", label = "", style = "minimal", icon = icon("maximize"))
    ),
    dropdownMenu(
      type        = "messages",
      badgeStatus = NULL,
      headerText  = "Dudas o sugerencias",
      messageItem(
        from    = contact_name,
        message = contact_email,
        icon    = icon("user-gear"),
        href    = str_c("mailto:", contact_email, "?Subject=DIIE%20app")
      )
    )
  ),


# dashboard Sidebar -------------------------------------------------------

  dashboardSidebar(
    width = 0,
    HTML(str_c("<br><br><br><br><br><br><br>")),
    sidebarMenu(
      menuItem(
        "Analítica",
        tabName = "analysis",
        icon = icon("chart-simple"),
        menuItem("CNGAE 2024", tabName = "CNGAE_analysis") # (update every year!)
      )
    )
  ), # End dashboardSidebar


# Dashboard Body ----------------------------------------------------------

  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper {
        background-color: #FFFFFF;
      }
    "
    ))),
    fullscreen_all(click_id = "page_full"),


# Analítica ----------------------------------------------------------

    tabItems(
      tabItem(
        "CNGAE_analysis",
        navbarPage(
          title       = "Análisis y seguimiento",
          id          = "id_navbar_current_year",
          selected    = "Cargar archivo",
          collapsible = TRUE,
          # Panel Info.
          tabPanel(
            title = "Info",
            icon  = icon("info-circle"),
            tags$div(style = "text-align:   justify;
                              font-size:    20px;
                              color:        #1e4a75;
                              display:      block;
                              margin-left:  auto;
                              margin-right: auto;
                              width:        70%;
                              padding:      4rem;
                              border:       2px solid #ccc" ,

                     h2("INFORMACIÓN GENERAL", style = "text-align: center;"),
                     br(),
                     "Como parte de las actividades de análisis y seguimiento
                     del Departamento de Integración de Información Estadística (DIIE)
                     se presentan indicadores que ponderan varios
                     resultados obtenidos en el CNG de ambito estatal.",
                     br(),
                     strong("Bajo un enfoque informativo,"),
                     "se tiene el objetivo de detectar",
                     strong("áreas de oportunidad "), "de los equipos de
                     trabajo de Oficinas Centrales y los Departamentos Estatales,
                     así como obtener información que ayude a diseñar estrategias
                     focalizadas al próximo levantamiento.",
                     br(),
                     br(),
                     "Los análisis que se presentan en cada uno de los apartados
                     fueron generados a partir dos consideraciones relevantes:",
                     br(),
                     HTML(str_c("<ol>",
                                "<li> Se descartan los folios con estatus final ‘No aplica’, a excepción de los análisis de la pestaña ‘Observaciones’.  </li>",
                                "<li> Las fechas se ajustan, de ser necesario, a días hábiles. Cualquier actividad efectuada en día inhábil se recorre al próximo día hábil. </li>",
                                "</ol>"
                     ))
            )
          ),


# Cargar archivo ----------------------------------------------------------

          tabPanel(
            "Cargar archivo",
            icon = icon("upload"),
            br(),
            fluidRow(
              column(
                width = 4,
                useShinyFeedback(),
                fileInput(
                  "file_upload", 'Historial de seguimiento con extensión "xlsx"',
                  accept = c(".xlsx"), width = "450px", buttonLabel =  "Buscar", placeholder = "Sin archivo"
                )
              ),
              column(
                width = 1,
                actionBttn(
                  inputId = "info_button_file_upload",
                  label   = "",
                  icon    = icon("info-circle"),
                  style   = "jelly"
                )
              )
            )
          ),


# NAVBARMENU Observaciones -----------------------------------------------------------

          navbarMenu(
            "Observaciones",
            icon = icon("chart-column"),
            tabPanel(
              "Top 10",
              h4(
                p(strong("Top 10 preguntas más observadas")),
                style = "color: #3c8dbc; margin: 0rem; margin-top: -1rem; margin-bottom: 3rem;"
              ),
              sidebarLayout(
                sidebarPanel(fluidRow(
                  column(width = 6,
                         selectInput("id_top_ten_question_2023",
                                     label = "Censo",
                                     choices = levels(DIIE_dates[[1]])
                                     )
                         ),
                  column(width = 6,
                         tabsetPanel(
                           id = "id_modulo_select_2023",
                           type = "hidden",
                           tabPanel(
                             "CNGE",
                             selectInput(
                               "id_CNGE_2023",
                               label = "Módulo",
                               choices = str_c(
                                 "Módulo ",
                                 seq(
                                   module_count[module_count$Censo == "CNGE", ][[2]]
                                 )
                               )
                             )
                           ),
                           tabPanel(
                             "CNSPE",
                             selectInput(
                               "id_CNSPE_2023",
                               label = "Módulo",
                               choices = str_c(
                                 "Módulo ",
                                 seq(
                                   module_count[module_count$Censo == "CNSPE", ][[2]]
                                 )
                               )
                             )
                           ),
                           tabPanel(
                             "CNSIPEE",
                             selectInput(
                               "id_CNSIPEE_2023",
                               label = "Módulo",
                               choices = str_c(
                                 "Módulo ",
                                 seq(
                                   module_count[module_count$Censo == "CNSIPEE", ][[2]]
                                 )
                               )
                             )
                           ),
                           tabPanel(
                             "CNPJE",
                             selectInput(
                               "id_CNPJE_2023",
                               "Módulo",
                               choices = str_c(
                                 "Módulo ",
                                 seq(
                                   module_count[module_count$Censo == "CNPJE", ][[2]]
                                 )
                               )
                             )
                           ),
                           tabPanel(
                             "CNIJE",
                             selectInput(
                               "id_CNIJE_2023",
                               label = "Módulo",
                               choices = str_c(
                                 "Módulo ",
                                 seq(
                                   module_count[module_count$Censo == "CNIJE", ][[2]]
                                 )
                               )
                             )
                           ),
                           tabPanel(
                             "CNPLE",
                             selectInput(
                               "id_CNPLE_2023",
                               label = "Módulo",
                               choices = str_c(
                                 "Módulo ",
                                 seq(
                                   module_count[module_count$Censo == "CNPLE", ][[2]]
                                 )
                               )
                             )
                           ),
                           tabPanel(
                             "CNDHE",
                             selectInput(
                               "id_CNDHE_2023",
                               label = "Módulo",
                               choices = str_c(
                                 "Módulo ",
                                 seq(
                                   module_count[module_count$Censo == "CNDHE", ][[2]]
                                 )
                               )
                             )
                           ),
                           tabPanel(
                             "CNTAIPPDPE",
                             selectInput(
                               "id_CNTAIPPDPE_2023",
                               label = "Módulo",
                               choices = str_c(
                                 "Módulo ",
                                 seq(
                                   module_count[module_count$Censo == "CNTAIPPDPE", ][[2]]
                                 )
                               )
                             )
                           )
                         )
                         )
                )
                ),
                mainPanel(
                  width = 12,
                  fluidRow(
                    column(width = 5,
                           plotlyOutput("plot_top_ten_questions_2023")
                           ),
                    column(width = 7,
                           dataTableOutput("table_top_ten_questions_2023")
                           )
                  ),
                  br(),
                  br(),
                  br()
                )
              )
            )
          ),


# NAVBARMENU Cuestionarios -----------------------------------------------------------

          navbarMenu(
            "Cuestionarios",
            icon = icon("chart-column"),
            tabPanel(
              "Revisión OC",
              tabsetPanel(
                type = "pills",
                tabPanel(
                  "Cuestionarios enviados a revisión OC",
                  br(),
                  sidebarLayout(
                    sidebarPanel(
                      width = 12,
                      sliderInput(
                        "id_slider_date_questionnaires_2023",
                        label = "Línea de tiempo por semana",
                        min   = floor_date(DIIE_dates %>% filter(name == "CNSIPEE") %>% select(`start CE`) %>% .[[1]], "week", week_start = 1) + weeks(3), # CNSIPEE start CE
                        max   = ceiling_date(tail(DIIE_dates$diffusion, 1), "week", week_start = 1), # last diffusion
                        value = floor_date(DIIE_dates %>% filter(name == "CNSIPEE") %>% select(`start CE`) %>% .[[1]], "week", week_start = 1) + weeks(3),
                        step  = weeks(1),
                        animate = TRUE
                      ),
                      p(strong("Cantidad de cuestionarios enviados a revisión OC por semana: "),
                        strong(textOutput("text_count_questionnaires_2023", inline = TRUE)),
                        style = "color: #3c8dbc")
                    ),
                    mainPanel(
                      style = "height: 400px",
                      width = 12,
                      plotlyOutput(
                        "plot_arrival_questionnaires",
                        height = "400px"
                      )
                    )
                  ),
                  br()
                ),
                tabPanel(
                  "Comparativo global 2023 vs 2024",
                  br(),
                  sidebarLayout(
                    sidebarPanel(
                      width = 12,
                      sliderInput(
                        "id_slider_date_questionnaires_weeks",
                        label = "Línea de tiempo por semana",
                        min   = 1,
                        max   = 30,
                        value = 1,
                        step  = 1,
                        animate = TRUE
                      ),
                      p(strong("Año 2024, cantidad de cuestionarios enviados a revisión OC por semana: "),
                        strong(textOutput("text_count_questionnaires_weeks_2023", inline = TRUE)),
                        style = "color: #3c8dbc"
                      ),
                      p(strong("Año 2023, cantidad de cuestionarios enviados a revisión OC por semana: "),
                        strong(textOutput("text_count_questionnaires_weeks_2022", inline = TRUE)),
                        style = "color: #5fa4cc"
                      )
                    ),
                    mainPanel(
                      style = "height: 400px",
                      width = 12,
                      fluidRow(
                        column(
                          width = 6,
                          plotlyOutput(
                            "plot_arrival_questionnaires_weeks",
                            height = "400px"
                          )
                        ),
                        column(
                          width = 6,
                          plotlyOutput(
                            "plot_arrival_questionnaires_weeks_previous_year",
                            height = "400px"
                          )
                        )
                      )
                    )
                  ),
                  br()
                ),
                tabPanel(
                  "Cuestionarios enviados a revisión OC por entidad",
                  br(),
                  sidebarLayout(
                    sidebarPanel(
                      width = 3,
                      selectInput(
                        "id_questionnaires_vs_entities_2023",
                        "Entidad",
                        choices = c("NACIONAL", levels(entities[[1]]))
                      )
                    ),
                    mainPanel(
                      width = 12,
                      plotlyOutput(height = "700px",
                        "plot_arrival_questionnaires_entitie_2023"
                      )
                    )
                  )
                )
              )
            ),

# En proceso de firma y sello (1) -----------------------------------------

            tabPanel(
              "En proceso de firma y sello (1)",
              tabsetPanel(
                type = "pills",
                tabPanel(
                  "Cuestionarios en proceso de firma y sello (1)",
                  br(),
                  sidebarLayout(
                    sidebarPanel(
                      width = 12,
                      fluidRow(
                        # Controlador por semana o día
                        column(
                          width = 2,
                          awesomeRadio(
                            inputId = "id_controller_plot_semana_day",
                            label = "Segmentar",
                            choices = c("Por semana", "Por día")
                          )
                        ),
                        # TabsetPanel por semana y por día.
                        column(
                          width = 10,
                          tabsetPanel(
                            id = "id_plot_questionnaries_firma_sello_select",
                            type = "hidden",
                            # Panel por semana
                            tabPanel(
                              "Por semana",
                              selectInput(
                                "id_questionnaires_firma_sello_census",
                                "Censo",
                                choices = c("GLOBAL", levels(DIIE_dates[[1]])),
                                width = "150px"
                              ),
                              p(strong("Acumulado de cuestionarios en proceso de firma y sello (1): "),
                                strong(textOutput("text_count_firma_sello_census", inline = TRUE)),
                                style = "color: #a71106")
                            ),
                            # Panel por día
                            tabPanel(
                              "Por día",
                              sliderInput(
                                "id_slider_date_questionnaires_firma_sello",
                                label = "Línea de tiempo",
                                min   = DIIE_dates[[3, 2]], # CNSIPEE start CE
                                max   = tail(pull(DIIE_dates), 1), # last diffusion
                                value = c(
                                  DIIE_dates[[3, 2]],
                                  tail(pull(DIIE_dates), 1)
                                ),
                                step  = days(1)
                              ),
                              tabsetPanel(
                                id = "id_text_questionnaires_firma_sello_range",
                                type = "hidden",
                                tabPanel(
                                  "accumulated",
                                  p(
                                    strong("Acumulado de cuestionarios en proceso de firma y sello (1): "),
                                    strong(textOutput("text_count_firma_sello_accumulated", inline = TRUE)),
                                    style = "color: #a71106"
                                  )
                                ),
                                tabPanel(
                                  "range",
                                  p(
                                    strong("Rango de cuestionarios en proceso de firma y sello (1): "),
                                    strong(textOutput("text_count_firma_sello_range", inline = TRUE)),
                                    style = "color: #5fa4cc"
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    ),
                    mainPanel(
                      width = 12,
                      tabsetPanel(
                        id = "id_plot_DT_questionnaries_firma_sello_select",
                        type = "hidden",
                        tabPanel(
                          "Por semana",
                          tabsetPanel(
                            id = "id_plot_DT",
                            type = "hidden",
                            tabPanel(
                              "global",
                              plotlyOutput(
                                "plot_questionnaires_firma_sello_week_global"
                              )
                            ),
                            tabPanel(
                              "census",
                              fluidRow(
                                column(
                                  width = 5,
                                  plotlyOutput(
                                    "plot_questionnaires_firma_sello_week_census"
                                  )
                                ),
                                column(
                                  width = 7,
                                  dataTableOutput("table_questionnaires_set_free_census")
                                )
                              )
                            )
                          )
                        ),
                        tabPanel(
                          "Por día",
                          fluidRow(
                            column(
                              width = 6,
                              plotlyOutput(
                                "plot_questionnaires_firma_sello_day"
                              )
                            ),
                            column(
                              width = 6,
                              dataTableOutput("table_questionnaires_set_free_registro")
                            )
                          )
                        )
                      )
                    )
                  ),
                  br(),
                ),
                tabPanel(
                  "Cuestionarios en proceso de firma y sello (1) por entidad",
                  br(),
                  sidebarLayout(
                    sidebarPanel(
                      width = 3,
                      selectInput(
                        "id_questionnaires_firma_sello_entity",
                        "Entidad",
                        choices = c("NACIONAL", levels(entities[[1]]))
                      )
                    ),
                    mainPanel(
                      width = 12,
                      plotlyOutput(height = "700px",
                        "plot_questionnaires_firma_sello_entity"
                      )
                    )
                  )
                )
              )
            )
          ),


# Interno -----------------------------------------------------------------

          tabPanel(
            "Interno",
            icon = icon("chart-column"),
            div(
              class = "pull-right",
              logoutUI(
                id = "logout",
                label = "",
                icon = icon("sign-out-alt")
              )
            ),
            loginUI(
              id = "login",
              title = "",
              user_title = "Usuario",
              pass_title = "Contraseña",
              login_title = "Iniciar sesión",
              error_message = "¡Usuario o contraseña no válidos!",
            ),
            uiOutput("diie_interno")
          ),


# Actualización -----------------------------------------------------------

          tabPanel(
            title = strong(uiOutput("update"), style = "color: #3c8dbc;font-size: 12px;"),
            h4(strong("Historial de seguimiento")),
            br(),
            dropdownButton(
              checkboxGroupInput(
                "id_columns_data",
                "Columnas:",
                choices  = c("Folio", "Entidad", "Usuario", "Perfil", "Registro", "Estatus", "Observación",
                            "Contador de días", "Cantidad de obs", "Censo"),
                selected = c("Folio", "Entidad", "Usuario", "Perfil", "Registro", "Estatus", "Observación",
                             "Contador de días")
              ),
              circle  = TRUE,
              status  = "primary",
              icon    = icon("gear"),
              width   = "300px",
              tooltip = tooltipOptions(title = "Clic para seleccionar columnas")
            ),
            br(),
            dataTableOutput("database_original")
          )
        )
      )
    )
  ) # End dashboardBody
)# End dashboardPage
)
