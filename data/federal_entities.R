
# Federal entities --------------------------------------------------------

# Database on federal entities
entities <- tibble(
  name = c("AGUASCALIENTES",
           "BAJA CALIFORNIA",
           "BAJA CALIFORNIA SUR",
           "CAMPECHE",
           "COAHUILA DE ZARAGOZA",
           "COLIMA",
           "CHIAPAS",
           "CHIHUAHUA",
           "CIUDAD DE MÉXICO",
           "DURANGO",
           "GUANAJUATO",
           "GUERRERO",
           "HIDALGO",
           "JALISCO",
           "MÉXICO",
           "MICHOACÁN DE OCAMPO",
           "MORELOS",
           "NAYARIT",
           "NUEVO LEÓN",
           "OAXACA",
           "PUEBLA",
           "QUERÉTARO",
           "QUINTANA ROO",
           "SAN LUIS POTOSÍ",
           "SINALOA",
           "SONORA",
           "TABASCO",
           "TAMAULIPAS",
           "TLAXCALA",
           "VERACRUZ DE IGNACIO DE LA LLAVE",
           "YUCATÁN",
           "ZACATECAS") %>%
    factor() %>% fct_inorder()
)

# Database on federal entities, names, Regional and ID.
federal_entities <- tibble(
  id_estado = c(
    "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16",
    "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32"
  ) %>% factor() %>% fct_inorder(),
  Abreviatura = c(
    "Ags", "BC", "BCS", "Camp", "Coah", "Col", "Chis", "Chih",
    "CDMX",  "Dgo", "Gto", "Gro", "Hgo", "Jal", "Mex", "Mich",
    "Mor", "Nay", "NL", "Oax", "Pue", "Qro", "Q_Roo", "SLP",
    "Sin", "Son", "Tab", "Tamps", "Tlax", "Ver", "Yuc", "Zac"
  ) %>% factor() %>% fct_inorder(),
  Entidad = c(
    "AGUASCALIENTES", "BAJA CALIFORNIA", "BAJA CALIFORNIA SUR",
    "CAMPECHE", "COAHUILA DE ZARAGOZA", "COLIMA", "CHIAPAS",
    "CHIHUAHUA", "CIUDAD DE MÉXICO", "DURANGO", "GUANAJUATO",
    "GUERRERO", "HIDALGO", "JALISCO", "MÉXICO", "MICHOACÁN DE OCAMPO",
    "MORELOS", "NAYARIT", "NUEVO LEÓN", "OAXACA",
    "PUEBLA", "QUERÉTARO", "QUINTANA ROO", "SAN LUIS POTOSÍ",
    "SINALOA", "SONORA", "TABASCO", "TAMAULIPAS", "TLAXCALA",
    "VERACRUZ DE IGNACIO DE LA LLAVE", "YUCATÁN", "ZACATECAS"
  ) %>% factor() %>% fct_inorder(),
  Regional = c(
    "Centro Norte", "Noroeste", "Noroeste", "Sureste", "Noreste", "Occidente",
    "Sur", "Norte", "Centro", "Norte", "Centro Norte", "Centro Sur",
    "Oriente", "Occidente", "Centro Sur", "Occidente", "Centro Sur", "Occidente",
    "Noreste", "Sur", "Oriente", "Centro Norte", "Sureste", "Centro Norte",
    "Noroeste", "Noroeste", "Sur", "Noreste", "Oriente", "Oriente", "Sureste", "Norte"
  ) %>% factor(levels = c("Centro", "Centro Norte", "Centro Sur", "Noreste",
                          "Noroeste", "Norte", "Occidente", "Oriente", "Sur", "Sureste"))
)
