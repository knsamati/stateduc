library(tidyverse)
library(leaflet)
library(reactable)
library(shiny)
library(knitr)
library(bslib)
server <- function(input, output, session) {
  
  
observeEvent(input$DRE_iepp,updateSelectInput(session, "IEPP_iepp", "IEPP", choices = df_iepp$IEPP[df_iepp$DRE == input$DRE_iepp]))
observeEvent(input$DRE,updateSelectInput(session, "IEPP", "IEPP", choices = df_ecole$IEPP[df_ecole$DRE == input$DRE]))
observeEvent(input$IEPP,updateSelectInput(session, "ecole", "ECOLE",choices = df_ecole$NOM_ETABLISSEMENT[df_ecole$IEPP == input$IEPP & df_ecole$DRE == input$DRE]))
observeEvent(input$DRE_map,updateSelectInput(session, "IEPP_map", "IEPP", choices = df_ecole$IEPP[df_ecole$DRE == input$DRE_map]))
observeEvent(input$annu_dre,updateSelectInput(session, "annu_iepp", "Inspection",choices = df_ecole$IEPP[df_ecole$DRE == input$annu_dre]))
  
  
  observeEvent(input$annuaire, {
    nav_select(id = "navbarID", selected = "stat_pays")
  })
  
  observeEvent(input$tdb, {
    nav_select(id = "navbarID", selected = "tableau_dre")
  })
  
  observeEvent(input$carte, {
    nav_select(id = "navbarID", selected = "carte_sco")
  })
  
  
  # DATA TABLE ---------------------------------------------------------
  
   output$annee <- renderUI({
    pickerInput(
      inputId = "annee_input",
      label = "Année scolaire",
      choices = possible_annee,
      inline = F
    )
  })
  
  
  
  #### data ####
  
df <- reactive({df_ecole})
  
  ecole <- reactive({
    req(input$annee_input)
    plot_data_dre <- df() |>
      dplyr::filter(annee_scolaire == input$annee_input) |>
      dplyr::group_by(DRE,IEPP) |>
      dplyr::summarise(
        Ecole = n(),
        Dur = sum(Dur,na.rm = TRUE),
        Banco = sum(Banco,na.rm = TRUE),
        Autres = sum(Autres,na.rm = TRUE),
        Salle = Dur + Banco + Autres,
        Garcon = sum(Eff_G_tot,na.rm = TRUE),
        Fille = sum(Eff_F_tot,na.rm = TRUE),
        effectif = Garcon + Fille,
        Ens_H = sum(Ens_H,na.rm = TRUE),
        Ens_F = sum(Ens_F,na.rm = TRUE),
        Ens = Ens_H + Ens_F
      )
    
    return(plot_data_dre)
    
  }) |> bindCache(input$annee_input)
  
  output$annutable <- renderReactable({
    req(ecole())
    ecole() |>
      reactable(
        groupBy = "DRE",
        defaultExpanded = TRUE,
        columnGroups = list(
          colGroup(name = "Salle des Classes", columns = c("Dur", "Banco","Autres","Salle")),
          colGroup(name = "Effectif des Elèves", columns = c("Garcon", "Fille","effectif")),
          colGroup(name = "Effectif des Enseignants", columns = c("Ens_H", "Ens_F","Ens"))
        ),
        columns = list(
          IEPP = colDef(name = "IEPP",
                        filterable = TRUE),
          Ecole = colDef(name = 'Ecole',aggregate = 'sum',
                         format = colFormat(separators = TRUE),
                         footer =  JS("function(column, state) {
                         let total = 0
                         state.sortedData.forEach(function(row) {
                         total += row[column.id]})
                         return total }"),
                         footerStyle = htmltools::css(
                           font_weight = 600,
                           border_top = '2px solid black'
                         )
          ),
          Dur = colDef(name = 'Dur',aggregate = 'sum',
                       format = colFormat(separators = TRUE),
                       footer =  JS("function(column, state) {
                         let total = 0
                         state.sortedData.forEach(function(row) {
                         total += row[column.id]})
                         return total }"),
                       footerStyle = htmltools::css(
                         font_weight = 600,
                         border_top = '2px solid black'
                       )
          ),
          Banco = colDef(name = 'Banco',aggregate = 'sum',
                         format = colFormat(separators = TRUE),
                         footer =  JS("function(column, state) {
                         let total = 0
                         state.sortedData.forEach(function(row) {
                         total += row[column.id]})
                         return total }"),
                         footerStyle = htmltools::css(
                           font_weight = 600,
                           border_top = '2px solid black')
          ),
          Autres = colDef(name = 'Autres',aggregate = 'sum',
                          format = colFormat(separators = TRUE),
                          footer =  JS("function(column, state) {
                         let total = 0
                         state.sortedData.forEach(function(row) {
                         total += row[column.id]})
                         return total }"),
                          footerStyle = htmltools::css(
                            font_weight = 600,
                            border_top = '2px solid black')
          ),
          Salle = colDef(name = 'Salle',aggregate = 'sum',
                         format = colFormat(separators = TRUE),
                         footer =  JS("function(column, state) {
                         let total = 0
                         state.sortedData.forEach(function(row) {
                         total += row[column.id]})
                         return total }"),
                         footerStyle = htmltools::css(
                           font_weight = 600,
                           border_top = '2px solid black')
          ),
          Garcon = colDef(name = 'Garcon',aggregate = 'sum',
                          format = colFormat(separators = TRUE),
                          footer =  JS("function(column, state) {
                         let total = 0
                         state.sortedData.forEach(function(row) {
                         total += row[column.id]})
                         return total }"),
                          footerStyle = htmltools::css(
                            font_weight = 600,
                            border_top = '2px solid black')
          ),
          Fille = colDef(name = 'Fille',aggregate = 'sum',
                         format = colFormat(separators = TRUE),
                         footer =  JS("function(column, state) {
                         let total = 0
                         state.sortedData.forEach(function(row) {
                         total += row[column.id]})
                         return total }"),
                         footerStyle = htmltools::css(
                           font_weight = 600,
                           border_top = '2px solid black')
          ),
          effectif = colDef(name = 'Effectif',aggregate = 'sum',
                            format = colFormat(separators = TRUE),
                            footer =  JS("function(column, state) {
                         let total = 0
                         state.sortedData.forEach(function(row) {
                         total += row[column.id]})
                         return total }"),
                            footerStyle = htmltools::css(
                              font_weight = 600,
                              border_top = '2px solid black')
          ),
          Ens_H = colDef(name = 'Ens_H',aggregate = 'sum',
                         footer =  JS("function(column, state) {
                         let total = 0
                         state.sortedData.forEach(function(row) {
                         total += row[column.id]})
                         return total }"),
                         footerStyle = htmltools::css(
                           font_weight = 600,
                           border_top = '2px solid black')
          ),
          Ens_F = colDef(name = 'Ens_F',aggregate = 'sum',
                         format = colFormat(separators = TRUE),
                         footer =  JS("function(column, state) {
                         let total = 0
                         state.sortedData.forEach(function(row) {
                         total += row[column.id]})
                         return total }"),
                         footerStyle = htmltools::css(
                           font_weight = 600,
                           border_top = '2px solid black')
          ),
          Ens = colDef(name = 'Ens',aggregate = 'sum',
                       format = colFormat(separators = TRUE),
                       footer =  JS("function(column, state) {
                         let total = 0
                         state.sortedData.forEach(function(row) {
                         total += row[column.id]})
                         return total }"),
                       footerStyle = htmltools::css(
                         font_weight = 600,
                         border_top = '2px solid black')
          )
        ),
        rowStyle = JS(
          "function(rowInfo) {
          if (rowInfo.level == 0) { // corresponds to row group
          return { 
          background: '#E7EDF3', 
          borderLeft: '2px solid #104E8B',
          fontWeight: 600 }
          }
          }"
        ),
        
      )
    
    
  })
  

  annu_iepp <- reactive({
    req(input$an_annu_iepp)
    #req(input$annu_iepp_dre)
    req(input$annu_iepp)
    data_annu_iepp <- df() |>
      dplyr::filter(IEPP == input$annu_iepp,annee_scolaire == input$an_annu_iepp) |>
      dplyr::mutate(
        Salle = Dur + Banco + Autres,
        effectif = Eff_G_tot + Eff_F_tot,
        Ens = Ens_H + Ens_F,
        Actions = NA) 
    return(data_annu_iepp)
    
  }) |> bindCache(input$an_annu_iepp,input$annu_iepp_dre,input$annu_iepp)
  
  output$annutable_iepp <- renderReactable({
    req(annu_iepp())
    annu_iepp() |>
      select(IEPP,CODE_ETABLISSEMENT,NOM_ETABLISSEMENT,Dur,Banco,Autres,Salle,Eff_G_tot,Eff_F_tot,effectif,Ens_H,Ens_F,Ens,Actions) |> 
      reactable(
        defaultColDef = colDef(
          align = "center",
          minWidth = 80,
          headerStyle = list(background = "#C0949F"),
          footerStyle = list(fontWeight = "bold")
        ),
        groupBy = "IEPP",
        defaultExpanded = TRUE,
        highlight = TRUE,
        striped = TRUE,
        compact = TRUE,
        #bordered = TRUE,
        columnGroups = list(
          colGroup(name = "Ecole", columns = c("CODE_ETABLISSEMENT", "NOM_ETABLISSEMENT")),
          colGroup(name = "Salle des Classes", columns = c("Dur", "Banco","Autres","Salle")),
          colGroup(name = "Effectif des Elèves", columns = c("Eff_G_tot", "Eff_F_tot","effectif")),
          colGroup(name = "Effectif des Enseignants", columns = c("Ens_H", "Ens_F","Ens"))
        ),
        columns = list(
          IEPP = colDef(name = "IEPP",footer = "Total"),
          CODE_ETABLISSEMENT = colDef(name = 'Code',
                                     width = 100,
                                     align = "left",
                                     footerStyle = htmltools::css(
                                       font_weight = 400,
                                       border_top = '2px solid black'
                                     )
          ),
          NOM_ETABLISSEMENT = colDef(name = 'Nom',
                                     align = "left",
                                     filterable = TRUE,
                                     width = 300,
                                     footerStyle = htmltools::css(
                           font_weight = 400,
                           border_top = '2px solid black'
                         )
          ),
          Dur = colDef(name = 'Dur',aggregate = 'sum',
                       width = 75,
                       format = colFormat(separators = TRUE),
                       footer =  JS("function(column, state) {
                         let total = 0
                         state.sortedData.forEach(function(row) {
                         total += row[column.id]})
                         return total }"),
                       footerStyle = htmltools::css(
                         font_weight = 400,
                         border_top = '2px solid black'
                       )
          ),
          Banco = colDef(name = 'Banco',aggregate = 'sum',
                         width = 75,
                         format = colFormat(separators = TRUE),
                         footer =  JS("function(column, state) {
                         let total = 0
                         state.sortedData.forEach(function(row) {
                         total += row[column.id]})
                         return total }"),
                         footerStyle = htmltools::css(
                           font_weight = 400,
                           border_top = '2px solid black')
          ),
          Autres = colDef(name = 'Autres',aggregate = 'sum',
                          width = 75,
                          format = colFormat(separators = TRUE),
                          footer =  JS("function(column, state) {
                         let total = 0
                         state.sortedData.forEach(function(row) {
                         total += row[column.id]})
                         return total }"),
                          footerStyle = htmltools::css(
                            font_weight = 400,
                            border_top = '2px solid black')
          ),
          Salle = colDef(name = 'Salle',aggregate = 'sum',
                         width = 75,
                         format = colFormat(separators = TRUE),
                         footer =  JS("function(column, state) {
                         let total = 0
                         state.sortedData.forEach(function(row) {
                         total += row[column.id]})
                         return total }"),
                         footerStyle = htmltools::css(
                           font_weight = 400,
                           border_top = '2px solid black')
          ),
          Eff_G_tot = colDef(name = 'Garcon',aggregate = 'sum',
                             width = 80,
                          format = colFormat(separators = TRUE),
                          footer =  JS("function(column, state) {
                         let total = 0
                         state.sortedData.forEach(function(row) {
                         total += row[column.id]})
                         return total }"),
                          footerStyle = htmltools::css(
                            font_weight = 400,
                            border_top = '2px solid black')
          ),
          Eff_F_tot = colDef(name = 'Fille',aggregate = 'sum',
                             width = 80,
                             format = colFormat(separators = TRUE),
                             footer =  JS("function(column, state) {
                         let total = 0
                         state.sortedData.forEach(function(row) {
                         total += row[column.id]})
                         return total }"),
                         footerStyle = htmltools::css(
                           font_weight = 400,
                           border_top = '2px solid black')
          ),
          effectif = colDef(name = 'Effectif',aggregate = 'sum',
                            width = 80,
                            format = colFormat(separators = TRUE),
                            footer =  JS("function(column, state) {
                         let total = 0
                         state.sortedData.forEach(function(row) {
                         total += row[column.id]})
                         return total }"),
                            footerStyle = htmltools::css(
                              font_weight = 400,
                              border_top = '2px solid black')
          ),
          Ens_H = colDef(name = 'Homme',aggregate = 'sum',
                         width = 90,
                         format = colFormat(separators = TRUE),
                         footer =  JS("function(column, state) {
                         let total = 0
                         state.sortedData.forEach(function(row) {
                         total += row[column.id]})
                         return total }"),
                         footerStyle = htmltools::css(
                           font_weight = 400,
                           border_top = '2px solid black')
          ),
          Ens_F = colDef(name = 'Femme',aggregate = 'sum',
                         width = 100,
                         format = colFormat(separators = TRUE),
                         footer =  JS("function(column, state) {
                         let total = 0
                         state.sortedData.forEach(function(row) {
                         total += row[column.id]})
                         return total }"),
                         footerStyle = htmltools::css(
                           font_weight = 400,
                           border_top = '2px solid black')
          ),
          Ens = colDef(name = 'Total',aggregate = 'sum',
                       width = 80,
                       format = colFormat(separators = TRUE),
                       footer =  JS("function(column, state) {
                         let total = 0
                         state.sortedData.forEach(function(row) {
                         total += row[column.id]})
                         return total }"),
                       footerStyle = htmltools::css(
                         font_weight = 400,
                         border_top = '2px solid black')
          ),
          Actions = colDef(
            name = "Actions",
            sortable = FALSE,
            cell = JS("function(cellInfo) {
            return '<button class=\"btn btn-primary btn-sm\" onclick=\"Shiny.setInputValue(\\'showModal\\', ' + cellInfo.row.CODE_ETABLISSEMENT + ', {priority: \\'event\\'});\">Détails</button>'
          }"),
            html = TRUE
          )
        ),
        rowStyle = JS(
          "function(rowInfo) {
          if (rowInfo.level == 0) { // corresponds to row group
          return { 
          background: '#C0949F', 
          borderLeft: '2px solid #104E8B',
          fontWeight: 400 }
          }
          }"
        ),
      )
    })
  
  observeEvent(input$showModal, {
    row_id <- as.integer(input$showModal)
    row_data <- annu_iepp()[annu_iepp()$CODE_ETABLISSEMENT == row_id, ]
    modalAnnu(df = row_data)

  }) 
  
  
  
#### Tableau des indicateurs
  
  dre <- reactive({
    req(input$DRE_dre)
    req(input$an_sco_dre)
    data_dre <- df_dre |>
      dplyr::filter(DRE == input$DRE_dre,annee_scolaire == input$an_sco_dre)
    return(data_dre)  
  }) |> bindCache(input$DRE_dre,input$an_sco_dre)
  
  iepp <- reactive({
    req(input$IEPP_iepp)
    req(input$an_sco_iepp)
    data_iepp <- df_iepp |>
      dplyr::filter(IEPP == input$IEPP_iepp,annee_scolaire == input$an_sco_iepp)
    return(data_iepp)  
  }) |> bindCache(input$IEPP_iepp,input$an_sco_iepp)
  
  
  ecole_eta <- reactive({
    req(input$ecole)
    req(input$an_sco)
    data_ecole <- df() |>
      dplyr::filter(NOM_ETABLISSEMENT == input$ecole,annee_scolaire == input$an_sco) |> 
      ungroup()
    return(data_ecole)
  }) |> bindCache(input$ecole,input$an_sco)
  
  # Value box dre ----
  
  output$eleve_dre <- renderText({
    n_eleve <- dre() |> select(Eff_tot) |> pull()
    validate(need(n_eleve, "No data"))
    scales::number(n_eleve, big.mark = ",")
  })
  output$ens_dre <- renderText({
    n_ens <- dre() |> select(Eff_Ens) |> pull()
    validate(need(n_ens, "No data"))
    scales::number(n_ens, big.mark = ",")
  })
  
  output$fille_dre <- renderText({
    Pourc_fille <- dre() |> select(PrcF_Tot) |> pull()
    validate(need(Pourc_fille, "No data"))
    scales::percent(Pourc_fille)
  })
  
  output$redoub_dre <- renderText({
    taux_redoublement <- dre() |> select(TRed_T) |> pull()
    validate(need(taux_redoublement, "No data"))
    scales::percent(taux_redoublement)
  })
  
  
  output$contexte_dre <- renderUI({
    tags$table(class = "table table-sm table-responsive table-stripe table-hover table-condensed",
               tags$tbody(
                 tags$tr(
                   tags$td(align = "left",strong("# Ecoles") ),
                   tags$td(align = "left", dre() |> select(Nombre_ecole) |> pull())
                 ),
                 tags$tr(
                   tags$td(align = "left",strong("# Ecoles Publiques") ),
                   tags$td(align = "left", dre() |> select(public) |> pull())
                 ),
                 tags$tr(
                   tags$td(align = "left",strong("% urbain") ),
                   tags$td(align = "left", dre() |> mutate(urb = urbain/Nombre_ecole) |> select(urb) |> pull() |> scales::percent()),
                 ),
                 tags$tr(
                   tags$td(align = "left",strong("% Electricité") ),
                   tags$td(align = "left", dre() |> mutate(elect = elec/Nombre_ecole) |> select(elect) |> pull() |> scales::percent())
                 ),
                 tags$tr(
                   tags$td(align = "left",strong("% Eau Potable") ),
                   tags$td(align = "left", dre() |> mutate(eau = eau/Nombre_ecole) |> select(eau) |> pull() |> scales::percent())
                 ),
                 tags$tr(
                   tags$td(align = "left",strong("% Classes en dur") ),
                   tags$td(align = "left", dre() |> mutate(dur = Dur/(Dur+Banco+Autres)) |> select(dur) |> pull() |> scales::percent())
                 ),
                 tags$tr(
                   tags$td(align = "left",strong("% Accessible") ),
                   tags$td(align = "left", dre() |> mutate(access = acces/Nombre_ecole) |> select(access) |> pull() |> scales::percent())
                 ),
                 tags$tr(
                   tags$td(align = "left",strong("% Cantine") ),
                   tags$td(align = "left", dre() |> mutate(cant = cantine/Nombre_ecole) |> select(cant) |> pull()|> scales::percent())
                 ),
                 tags$tr(
                   tags$td(align = "left",strong("% Cogep") ),
                   tags$td(align = "left", dre() |> mutate(cogep = cogep/Nombre_ecole) |> select(cogep) |> pull()|> scales::percent())
                 ),
                 tags$tr(
                   tags$td(align = "left",strong("Lave-main") ),
                   tags$td(align = "left", dre() |> mutate(lav = lave_main/Nombre_ecole) |> select(lav) |> pull()|> scales::percent())
                 ),
               )
    )
    
  })
  
  output$effetif_dre <- renderUI({
    tags$table(class = "table table-sm table-responsive table-stripe table-hover table-condensed",
               tags$tbody(
                 tags$tr(
                   tags$th("Niveau"),
                   tags$th("# Section"),
                   tags$th("# Elève"),
                   tags$th("% fille"),
                   tags$th("Tx de redoublants"),
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("CP1")),
                   tags$td(align = "left", dre() |> select(An2_NB_CP1) |> pull()),
                   tags$td(align = "left", dre() |> select(eff_CP1) |> pull()),
                   tags$td(align = "left", dre() |> select(PrcF_CP1) |> pull() |> scales::percent()),
                   tags$td(align = "left", dre() |> select(TRed_CP1) |> pull() |> scales::percent())
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("CP2")),
                   tags$td(align = "left", dre() |> select(An2_NB_CP2) |> pull()),
                   tags$td(align = "left", dre() |> select(eff_CP2) |> pull()),
                   tags$td(align = "left", dre() |> select(PrcF_CP2) |> pull() |> scales::percent()),
                   tags$td(align = "left", dre() |> select(TRed_CP2) |> pull() |> scales::percent())
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("CE1")),
                   tags$td(align = "left", dre() |> select(An2_NB_CE1) |> pull()),
                   tags$td(align = "left", dre() |> select(eff_CE1) |> pull()),
                   tags$td(align = "left", dre() |> select(PrcF_CE1) |> pull() |> scales::percent()),
                   tags$td(align = "left", dre() |> select(TRed_CE1) |> pull() |> scales::percent())
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("CE2")),
                   tags$td(align = "left", dre() |> select(An2_NB_CE2) |> pull()),
                   tags$td(align = "left", dre() |> select(eff_CE2) |> pull()),
                   tags$td(align = "left", dre() |> select(PrcF_CE2) |> pull() |> scales::percent()),
                   tags$td(align = "left", dre() |> select(TRed_CE2) |> pull() |> scales::percent())
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("CM1")),
                   tags$td(align = "left", dre() |> select(An2_NB_CM1) |> pull()),
                   tags$td(align = "left", dre() |> select(eff_CM1) |> pull()),
                   tags$td(align = "left", dre() |> select(PrcF_CM1) |> pull() |> scales::percent()),
                   tags$td(align = "left", dre() |> select(TRed_CM1) |> pull() |> scales::percent())
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("CM2")),
                   tags$td(align = "left", dre() |> select(An2_NB_CM2) |> pull()),
                   tags$td(align = "left", dre() |> select(eff_CM2) |> pull()),
                   tags$td(align = "left", dre() |> select(PrcF_CM2) |> pull() |> scales::percent()),
                   tags$td(align = "left", dre() |> select(TRed_CM2) |> pull() |> scales::percent())
                 ),
                 tags$tr(class="table-success fw-bold",
                         tags$td(align = "left", strong("Total")),
                         tags$td(align = "left", dre() |> select(an2_nb) |> pull()),
                         tags$td(align = "left", dre() |> select(Eff_tot) |> pull()),
                         tags$td(align = "left", dre() |> select(PrcF_Tot) |> pull() |> scales::percent()),
                         tags$td(align = "left", dre() |> select(TRed_T) |> pull() |> scales::percent())
                 )
               )
    )
  })
  
  output$humain_dre <- renderUI({
    tags$table(class = "table table-sm table-responsive table-stripe table-hover table-condensed",
               tags$tbody(
                 tags$tr(
                   tags$th("Statut"),
                   tags$th("# Homme"),
                   tags$th("# Femme"),
                   tags$th("# Total"),
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("Fonctionnaires")),
                   tags$td(align = "left", dre() |> select(FoncM) |> pull()),
                   tags$td(align = "left", dre() |> select(FoncF) |> pull()),
                   tags$td(align = "left", dre() |> select(FoncT) |> pull()),
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("Volontaires")),
                   tags$td(align = "left", dre() |> select(VolM) |> pull()),
                   tags$td(align = "left", dre() |> select(VolF) |> pull()),
                   tags$td(align = "left", dre() |> select(VolT) |> pull()),
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("Privés")),
                   tags$td(align = "left", dre() |> select(PrivM) |> pull()),
                   tags$td(align = "left", dre() |> select(PrivF) |> pull()),
                   tags$td(align = "left", dre() |> select(PrivT) |> pull()),
                 ),
                 tags$tr(class="table-success fw-bold",
                         tags$td(align = "left", strong("REM")),
                         tags$td(align = "left", "-"),
                         tags$td(align = "left", "-"),
                         tags$td(align = "left", dre() |> select(REM) |> pull()),
                 ),
                 
               )
               
    )
  })
  
  output$materiel_dre <- renderUI({
    tags$table(class = "table table-sm table-responsive table-stripe table-hover table-condensed",
               tags$tbody(
                 tags$tr(
                   tags$th("Niveau"),
                   tags$th("# Lecture"),
                   tags$th("Ratio Man/Elv FR"),
                   tags$th("# Math"),
                   tags$th("Ratio Man/Elv Math"),
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("CP1")),
                   tags$td(align = "left", dre() |> select(Lecture_CP1) |> pull()),
                   tags$td(align = "left", dre() |> select(RMFE_CP1) |> pull()),
                   tags$td(align = "left", dre() |> select(Calcul_CP1) |> pull()),
                   tags$td(align = "left", dre() |> select(RMME_CP1) |> pull()),
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("CP2")),
                   tags$td(align = "left", dre() |> select(Lecture_CP2) |> pull()),
                   tags$td(align = "left", dre() |> select(RMFE_CP2) |> pull()),
                   tags$td(align = "left", dre() |> select(Calcul_CP2) |> pull()),
                   tags$td(align = "left", dre() |> select(RMME_CP2) |> pull()),
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("CE1")),
                   tags$td(align = "left", dre() |> select(Lecture_CE1) |> pull()),
                   tags$td(align = "left", dre() |> select(RMFE_CE1) |> pull()),
                   tags$td(align = "left", dre() |> select(Calcul_CE1) |> pull()),
                   tags$td(align = "left", dre() |> select(RMME_CE1) |> pull()),
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("CE2")),
                   tags$td(align = "left", dre() |> select(Lecture_CE2) |> pull()),
                   tags$td(align = "left", dre() |> select(RMFE_CE2) |> pull()),
                   tags$td(align = "left", dre() |> select(Calcul_CE2) |> pull()),
                   tags$td(align = "left", dre() |> select(RMME_CE2) |> pull()),
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("CM1")),
                   tags$td(align = "left", dre() |> select(Lecture_CM1) |> pull()),
                   tags$td(align = "left", dre() |> select(RMFE_CM1) |> pull()),
                   tags$td(align = "left", dre() |> select(Calcul_CM1) |> pull()),
                   tags$td(align = "left", dre() |> select(RMME_CM1) |> pull()),
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("CM2")),
                   tags$td(align = "left", dre() |> select(Lecture_CM2) |> pull()),
                   tags$td(align = "left", dre() |> select(RMFE_CM2) |> pull()),
                   tags$td(align = "left", dre() |> select(Calcul_CM2) |> pull()),
                   tags$td(align = "left", dre() |> select(RMME_CM2) |> pull()),
                 ),
                 tags$tr(class="table-success fw-bold",
                         tags$td(align = "left", strong("Total")),
                         tags$td(align = "left", dre() |> select(ManF_T) |> pull()),
                         tags$td(align = "left", dre() |> select(RMFE_T) |> pull()),
                         tags$td(align = "left", dre() |> select(ManM_T) |> pull() ),
                         tags$td(align = "left", dre() |> select(RMME_T) |> pull() ),
                 )
               )
    )
  })
  
  
  # Value box iepp ----
  
  output$eleve_iepp <- renderText({
    n_eleve <- iepp() |> select(Eff_tot) |> pull()
    validate(need(n_eleve, "No data"))
    scales::number(n_eleve, big.mark = ",")
  })
  output$ens_iepp <- renderText({
    n_ens <- iepp() |> select(Eff_Ens) |> pull()
    validate(need(n_ens, "No data"))
    scales::number(n_ens, big.mark = ",")
  })
  
  output$fille_iepp <- renderText({
    Pourc_fille <- iepp() |> select(PrcF_Tot) |> pull()
    validate(need(Pourc_fille, "No data"))
    scales::percent(Pourc_fille)
  })
  
  output$redoub_iepp <- renderText({
    taux_redoublement <- iepp() |> select(TRed_T) |> pull()
    validate(need(taux_redoublement, "No data"))
    scales::percent(taux_redoublement)
  })
 
  output$contexte_iepp <- renderUI({
    tags$table(class = "table table-sm table-responsive table-stripe table-hover table-condensed",
               tags$tbody(
                 tags$tr(
                   tags$td(align = "left",strong("# Ecoles") ),
                   tags$td(align = "left", iepp() |> select(Nombre_ecole) |> pull())
                 ),
                 tags$tr(
                   tags$td(align = "left",strong("# Ecoles Publiques") ),
                   tags$td(align = "left", iepp() |> select(public) |> pull())
                 ),
                 tags$tr(
                   tags$td(align = "left",strong("% urbain") ),
                   tags$td(align = "left", iepp() |> mutate(urb = urbain/Nombre_ecole) |> select(urb) |> pull() |> scales::percent()),
                 ),
                 tags$tr(
                   tags$td(align = "left",strong("% Electricité") ),
                   tags$td(align = "left", iepp() |> mutate(elect = elec/Nombre_ecole) |> select(elect) |> pull() |> scales::percent())
                 ),
                 tags$tr(
                   tags$td(align = "left",strong("% Eau Potable") ),
                   tags$td(align = "left", iepp() |> mutate(eau = eau/Nombre_ecole) |> select(eau) |> pull() |> scales::percent())
                 ),
                 tags$tr(
                   tags$td(align = "left",strong("% Classes en dur") ),
                   tags$td(align = "left", iepp() |> mutate(dur = Dur/(Dur+Banco+Autres)) |> select(dur) |> pull() |> scales::percent())
                 ),
                 tags$tr(
                   tags$td(align = "left",strong("% Accessible") ),
                   tags$td(align = "left", iepp() |> mutate(access = acces/Nombre_ecole) |> select(access) |> pull() |> scales::percent())
                 ),
                 tags$tr(
                   tags$td(align = "left",strong("% Cantine") ),
                   tags$td(align = "left", iepp() |> mutate(cant = cantine/Nombre_ecole) |> select(cant) |> pull()|> scales::percent())
                 ),
                 tags$tr(
                   tags$td(align = "left",strong("% Cogep") ),
                   tags$td(align = "left", iepp() |> mutate(cogep = cogep/Nombre_ecole) |> select(cogep) |> pull()|> scales::percent())
                 ),
                 tags$tr(
                   tags$td(align = "left",strong("Lave-main") ),
                   tags$td(align = "left", iepp() |> mutate(lav = lave_main/Nombre_ecole) |> select(lav) |> pull()|> scales::percent())
                 ),
               )
    )
    
  })
  
  output$effetif_iepp <- renderUI({
    tags$table(class = "table table-sm table-responsive table-stripe table-hover table-condensed",
               tags$tbody(
                 tags$tr(
                   tags$th("Niveau"),
                   tags$th("# Section"),
                   tags$th("# Elève"),
                   tags$th("% fille"),
                   tags$th("Tx de redoublants"),
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("CP1")),
                   tags$td(align = "left", iepp() |> select(An2_NB_CP1) |> pull()),
                   tags$td(align = "left", iepp() |> select(eff_CP1) |> pull()),
                   tags$td(align = "left", iepp() |> select(PrcF_CP1) |> pull() |> scales::percent()),
                   tags$td(align = "left", iepp() |> select(TRed_CP1) |> pull() |> scales::percent())
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("CP2")),
                   tags$td(align = "left", iepp() |> select(An2_NB_CP2) |> pull()),
                   tags$td(align = "left", iepp() |> select(eff_CP2) |> pull()),
                   tags$td(align = "left", iepp() |> select(PrcF_CP2) |> pull() |> scales::percent()),
                   tags$td(align = "left", iepp() |> select(TRed_CP2) |> pull() |> scales::percent())
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("CE1")),
                   tags$td(align = "left", iepp() |> select(An2_NB_CE1) |> pull()),
                   tags$td(align = "left", iepp() |> select(eff_CE1) |> pull()),
                   tags$td(align = "left", iepp() |> select(PrcF_CE1) |> pull() |> scales::percent()),
                   tags$td(align = "left", iepp() |> select(TRed_CE1) |> pull() |> scales::percent())
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("CE2")),
                   tags$td(align = "left", iepp() |> select(An2_NB_CE2) |> pull()),
                   tags$td(align = "left", iepp() |> select(eff_CE2) |> pull()),
                   tags$td(align = "left", iepp() |> select(PrcF_CE2) |> pull() |> scales::percent()),
                   tags$td(align = "left", iepp() |> select(TRed_CE2) |> pull() |> scales::percent())
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("CM1")),
                   tags$td(align = "left", iepp() |> select(An2_NB_CM1) |> pull()),
                   tags$td(align = "left", iepp() |> select(eff_CM1) |> pull()),
                   tags$td(align = "left", iepp() |> select(PrcF_CM1) |> pull() |> scales::percent()),
                   tags$td(align = "left", iepp() |> select(TRed_CM1) |> pull() |> scales::percent())
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("CM2")),
                   tags$td(align = "left", iepp() |> select(An2_NB_CM2) |> pull()),
                   tags$td(align = "left", iepp() |> select(eff_CM2) |> pull()),
                   tags$td(align = "left", iepp() |> select(PrcF_CM2) |> pull() |> scales::percent()),
                   tags$td(align = "left", iepp() |> select(TRed_CM2) |> pull() |> scales::percent())
                 ),
                 tags$tr(class="table-success fw-bold",
                         tags$td(align = "left", strong("Total")),
                         tags$td(align = "left", iepp() |> select(an2_nb) |> pull()),
                         tags$td(align = "left", iepp() |> select(Eff_tot) |> pull()),
                         tags$td(align = "left", iepp() |> select(PrcF_Tot) |> pull() |> scales::percent()),
                         tags$td(align = "left", iepp() |> select(TRed_T) |> pull() |> scales::percent())
                 )
               )
    )
  })
  
  output$humain_iepp <- renderUI({
    tags$table(class = "table table-sm table-responsive table-stripe table-hover table-condensed",
               tags$tbody(
                 tags$tr(
                   tags$th("Statut"),
                   tags$th("# Homme"),
                   tags$th("# Femme"),
                   tags$th("# Total"),
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("Fonctionnaires")),
                   tags$td(align = "left", iepp() |> select(FoncM) |> pull()),
                   tags$td(align = "left", iepp() |> select(FoncF) |> pull()),
                   tags$td(align = "left", iepp() |> select(FoncT) |> pull()),
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("Volontaires")),
                   tags$td(align = "left", iepp() |> select(VolM) |> pull()),
                   tags$td(align = "left", iepp() |> select(VolF) |> pull()),
                   tags$td(align = "left", iepp() |> select(VolT) |> pull()),
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("Privés")),
                   tags$td(align = "left", iepp() |> select(PrivM) |> pull()),
                   tags$td(align = "left", iepp() |> select(PrivF) |> pull()),
                   tags$td(align = "left", iepp() |> select(PrivT) |> pull()),
                 ),
                 tags$tr(class="table-success fw-bold",
                         tags$td(align = "left", strong("REM")),
                         tags$td(align = "left", "-"),
                         tags$td(align = "left", "-"),
                         tags$td(align = "left", iepp() |> select(REM) |> pull()),
                 ),
                 
               )
               
    )
  })
  
  output$materiel_iepp <- renderUI({
    tags$table(class = "table table-sm table-responsive table-stripe table-hover table-condensed",
               tags$tbody(
                 tags$tr(
                   tags$th("Niveau"),
                   tags$th("# Lecture"),
                   tags$th("Ratio Man/Elv FR"),
                   tags$th("# Math"),
                   tags$th("Ratio Man/Elv Math"),
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("CP1")),
                   tags$td(align = "left", iepp() |> select(Lecture_CP1) |> pull()),
                   tags$td(align = "left", iepp() |> select(RMFE_CP1) |> pull()),
                   tags$td(align = "left", iepp() |> select(Calcul_CP1) |> pull()),
                   tags$td(align = "left", iepp() |> select(RMME_CP1) |> pull()),
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("CP2")),
                   tags$td(align = "left", iepp() |> select(Lecture_CP2) |> pull()),
                   tags$td(align = "left", iepp() |> select(RMFE_CP2) |> pull()),
                   tags$td(align = "left", iepp() |> select(Calcul_CP2) |> pull()),
                   tags$td(align = "left", iepp() |> select(RMME_CP2) |> pull()),
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("CE1")),
                   tags$td(align = "left", iepp() |> select(Lecture_CE1) |> pull()),
                   tags$td(align = "left", iepp() |> select(RMFE_CE1) |> pull()),
                   tags$td(align = "left", iepp() |> select(Calcul_CE1) |> pull()),
                   tags$td(align = "left", iepp() |> select(RMME_CE1) |> pull()),
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("CE2")),
                   tags$td(align = "left", iepp() |> select(Lecture_CE2) |> pull()),
                   tags$td(align = "left", iepp() |> select(RMFE_CE2) |> pull()),
                   tags$td(align = "left", iepp() |> select(Calcul_CE2) |> pull()),
                   tags$td(align = "left", iepp() |> select(RMME_CE2) |> pull()),
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("CM1")),
                   tags$td(align = "left", iepp() |> select(Lecture_CM1) |> pull()),
                   tags$td(align = "left", iepp() |> select(RMFE_CM1) |> pull()),
                   tags$td(align = "left", iepp() |> select(Calcul_CM1) |> pull()),
                   tags$td(align = "left", iepp() |> select(RMME_CM1) |> pull()),
                 ),
                 tags$tr(
                   tags$td(align = "left", strong("CM2")),
                   tags$td(align = "left", iepp() |> select(Lecture_CM2) |> pull()),
                   tags$td(align = "left", iepp() |> select(RMFE_CM2) |> pull()),
                   tags$td(align = "left", iepp() |> select(Calcul_CM2) |> pull()),
                   tags$td(align = "left", iepp() |> select(RMME_CM2) |> pull()),
                 ),
                 tags$tr(class="table-success fw-bold",
                         tags$td(align = "left", strong("Total")),
                         tags$td(align = "left", iepp() |> select(ManF_T) |> pull()),
                         tags$td(align = "left", iepp() |> select(RMFE_T) |> pull()),
                         tags$td(align = "left", iepp() |> select(ManM_T) |> pull() ),
                         tags$td(align = "left", iepp() |> select(RMME_T) |> pull() ),
                 )
               )
    )
  })
  
  
  
  # Value box ecoles ----
  
  output$eleve_ecole <- renderText({
    n_eleve <- ecole_eta() |> select(Eff_tot) |> pull()
    validate(need(n_eleve, "No data"))
    scales::number(n_eleve, big.mark = ",")
  })
  output$ens_ecole <- renderText({
    n_ens <- ecole_eta() |> select(Eff_Ens) |> pull()
    validate(need(n_ens, "No data"))
    scales::number(n_ens, big.mark = ",")
  })
  
  output$fille_ecole <- renderText({
    Pourc_fille <- ecole_eta() |> select(PrcF_Tot) |> pull()
    validate(need(Pourc_fille, "No data"))
    scales::percent(Pourc_fille)
  })
  
  output$redoub_ecole <- renderText({
    taux_redoublement <- ecole_eta() |> select(TRed_T) |> pull()
    validate(need(taux_redoublement, "No data"))
    scales::percent(taux_redoublement)
  })
  
output$contexte_ecole <- renderUI({
  tags$table(class = "table table-sm table-responsive table-stripe table-hover table-condensed",
             tags$tbody(
               tags$tr(
                 tags$td(align = "left",strong("Statut") ),
                 tags$td(align = "left", ecole_eta() |> select(LIBELLE_TYPE_STATUT_ETAB) |> pull())
               ),
               tags$tr(
                 tags$td(align = "left",strong("Milieu") ),
                 tags$td(align = "left", ecole_eta() |> select(LIBELLE_TYPE_MILIEU) |> pull())
               ),
               tags$tr(
                 tags$td(align = "left",strong("Electricité") ),
                 tags$td(align = "left", ecole_eta() |> mutate(elect = ifelse(EXISTE_ELECT == 1,"Oui","Non")) |> select(elect) |> pull())
               ),
               tags$tr(
                 tags$td(align = "left",strong("Eau Potable") ),
                 tags$td(align = "left", ecole_eta() |> mutate(eau = ifelse(CODE_TYPE_SOURCE_EAU == 1,"Oui","Non")) |> select(eau) |> pull())
               ),
               tags$tr(
                 tags$td(align = "left",strong("Terrain Sport") ),
                 tags$td(align = "left", ecole_eta() |> mutate(sport = ifelse(EXISTE_ESPACE_EPS == 1,"Oui","Non")) |> select(sport) |> pull())
               ),
               tags$tr(
                 tags$td(align = "left",strong("Accessibilité") ),
                 tags$td(align = "left", ecole_eta() |> mutate(access = ifelse(ACCES_TOUTE_SAISON == 1,"Oui","Non")) |> select(access) |> pull())
               ),
               tags$tr(
                 tags$td(align = "left",strong("Cantine") ),
                 tags$td(align = "left", ecole_eta() |> mutate(cant = ifelse(CANTINE_FONCT == 1,"Oui","Non")) |> select(cant) |> pull())
               ),
               tags$tr(
                 tags$td(align = "left",strong("Cogep") ),
                 tags$td(align = "left", ecole_eta() |> mutate(cogep = ifelse(EXISTE_COGE == 1,"Oui","Non")) |>  select(cogep) |> pull())
               ),
               tags$tr(
                 tags$td(align = "left",strong("Lave-main") ),
                 tags$td(align = "left", ecole_eta() |> mutate(lav = ifelse(LAVE_MAIN_FONCT == 1,"Oui","Non")) |> select(lav) |> pull())
               ),
             )
  )

  })

output$effetif_ecole <- renderUI({
  tags$table(class = "table table-sm table-responsive table-stripe table-hover table-condensed",
             tags$tbody(
               tags$tr(
                 tags$th("Niveau"),
                 tags$th("# Section"),
                 tags$th("# Elève"),
                 tags$th("% fille"),
                 tags$th("Tx de redoublants"),
               ),
               tags$tr(
                 tags$td(align = "left", strong("CP1")),
                 tags$td(align = "left", ecole_eta() |> select(An2_NB_CP1) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(eff_CP1) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(PrcF_CP1) |> pull() |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> select(TRed_CP1) |> pull() |> scales::percent())
               ),
               tags$tr(
                 tags$td(align = "left", strong("CP2")),
                 tags$td(align = "left", ecole_eta() |> select(An2_NB_CP2) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(eff_CP2) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(PrcF_CP2) |> pull() |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> select(TRed_CP2) |> pull() |> scales::percent())
               ),
               tags$tr(
                 tags$td(align = "left", strong("CE1")),
                 tags$td(align = "left", ecole_eta() |> select(An2_NB_CE1) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(eff_CE1) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(PrcF_CE1) |> pull() |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> select(TRed_CE1) |> pull() |> scales::percent())
               ),
               tags$tr(
                 tags$td(align = "left", strong("CE2")),
                 tags$td(align = "left", ecole_eta() |> select(An2_NB_CE2) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(eff_CE2) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(PrcF_CE2) |> pull() |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> select(TRed_CE2) |> pull() |> scales::percent())
               ),
               tags$tr(
                 tags$td(align = "left", strong("CM1")),
                 tags$td(align = "left", ecole_eta() |> select(An2_NB_CM1) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(eff_CM1) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(PrcF_CM1) |> pull() |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> select(TRed_CM1) |> pull() |> scales::percent())
               ),
               tags$tr(
                 tags$td(align = "left", strong("CM2")),
                 tags$td(align = "left", ecole_eta() |> select(An2_NB_CM2) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(eff_CM2) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(PrcF_CM2) |> pull() |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> select(TRed_CM2) |> pull() |> scales::percent())
               ),
               tags$tr(class="table-success fw-bold",
                 tags$td(align = "left", strong("Total")),
                 tags$td(align = "left", ecole_eta() |> select(an2_nb) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(Eff_tot) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(PrcF_Tot) |> pull() |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> select(TRed_T) |> pull() |> scales::percent())
               )
             )
  )
})


output$humain_ecole <- renderUI({
  tags$table(class = "table table-sm table-responsive table-stripe table-hover table-condensed",
             tags$tbody(
               tags$tr(
                 tags$th("Statut"),
                 tags$th("# Homme"),
                 tags$th("# Femme"),
                 tags$th("# Total"),
               ),
               tags$tr(
                 tags$td(align = "left", strong("Fonctionnaires")),
                 tags$td(align = "left", ecole_eta() |> select(FoncM) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(FoncF) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(FoncT) |> pull()),
               ),
               tags$tr(
                 tags$td(align = "left", strong("Volontaires")),
                 tags$td(align = "left", ecole_eta() |> select(VolM) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(VolF) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(VolT) |> pull()),
               ),
               tags$tr(
                 tags$td(align = "left", strong("Privés")),
                 tags$td(align = "left", ecole_eta() |> select(PrivM) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(PrivF) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(PrivT) |> pull()),
               ),
               tags$tr(class="table-success fw-bold",
                 tags$td(align = "left", strong("REM")),
                 tags$td(align = "left", "-"),
                 tags$td(align = "left", "-"),
                 tags$td(align = "left", ecole_eta() |> select(REM) |> pull()),
               ),
               
             )
             
  )
})

output$materiel_ecole <- renderUI({
  tags$table(class = "table table-sm table-responsive table-stripe table-hover table-condensed",
             tags$tbody(
               tags$tr(
                 tags$th("Niveau"),
                 tags$th("# Lecture"),
                 tags$th("Ratio Man/Elv FR"),
                 tags$th("# Math"),
                 tags$th("Ratio Man/Elv Math"),
               ),
               tags$tr(
                 tags$td(align = "left", strong("CP1")),
                 tags$td(align = "left", ecole_eta() |> select(Lecture_CP1) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(RMFE_CP1) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(Calcul_CP1) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(RMME_CP1) |> pull()),
               ),
               tags$tr(
                 tags$td(align = "left", strong("CP2")),
                 tags$td(align = "left", ecole_eta() |> select(Lecture_CP2) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(RMFE_CP2) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(Calcul_CP2) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(RMME_CP2) |> pull()),
               ),
               tags$tr(
                 tags$td(align = "left", strong("CE1")),
                 tags$td(align = "left", ecole_eta() |> select(Lecture_CE1) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(RMFE_CE1) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(Calcul_CE1) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(RMME_CE1) |> pull()),
               ),
               tags$tr(
                 tags$td(align = "left", strong("CE2")),
                 tags$td(align = "left", ecole_eta() |> select(Lecture_CE2) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(RMFE_CE2) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(Calcul_CE2) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(RMME_CE2) |> pull()),
               ),
               tags$tr(
                 tags$td(align = "left", strong("CM1")),
                 tags$td(align = "left", ecole_eta() |> select(Lecture_CM1) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(RMFE_CM1) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(Calcul_CM1) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(RMME_CM1) |> pull()),
               ),
               tags$tr(
                 tags$td(align = "left", strong("CM2")),
                 tags$td(align = "left", ecole_eta() |> select(Lecture_CM2) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(RMFE_CM2) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(Calcul_CM2) |> pull()),
                 tags$td(align = "left", ecole_eta() |> select(RMME_CM2) |> pull()),
               ),
               tags$tr(class="table-success fw-bold",
                       tags$td(align = "left", strong("Total")),
                       tags$td(align = "left", ecole_eta() |> select(ManF_T) |> pull()),
                       tags$td(align = "left", ecole_eta() |> select(RMFE_T) |> pull()),
                       tags$td(align = "left", ecole_eta() |> select(ManM_T) |> pull() ),
                       tags$td(align = "left", ecole_eta() |> select(RMME_T) |> pull() ),
               )
             )
  )
})

output$cepd <- renderUI({
  tags$table(class = "table table-sm table-responsive table-stripe table-hover table-condensed",
             tags$tbody(
               tags$tr(
                 tags$th("Discipline"),
                 tags$th("Moyenne des notes"),
                 tags$th("% des filles note >10"),
                 tags$th("% des Garcon note >10"),
                 tags$th("% Ens. note >10"),
               ),
               tags$tr(
                 tags$td(align = "left", strong("Français")),
                 tags$td(align = "left", ecole_eta() |> select(note_moyFr) |> pull() |> round(1)),
                 tags$td(align = "left", ecole_eta() |> mutate(Prc_fr_F = Prc_fr_F/100) |> select(Prc_fr_F) |> pull()|> round(1) |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> mutate(Prc_fr_G = Prc_fr_G/100) |> select(Prc_fr_G) |> pull() |> round(1) |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> select(Prc_fr) |> pull() |> scales::percent())
               ),
               tags$tr(
                 tags$td(align = "left", strong("Math")),
                 tags$td(align = "left", ecole_eta() |> select(note_moyMath) |> pull()|> round(1)),
                 tags$td(align = "left", ecole_eta() |> mutate(Prc_math_F = Prc_math_F/100) |> select(Prc_math_F) |> pull() |> round(1) |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> mutate(Prc_math_M = Prc_math_M/100) |> select(Prc_math_M) |> pull() |> round(1) |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> select(Prc_math) |> pull() |> scales::percent())
               ),
               tags$tr(
                 tags$td(align = "left", strong("Lecons")),
                 tags$td(align = "left", ecole_eta() |> select(note_moyLecon) |> pull()|> round(1)),
                 tags$td(align = "left", ecole_eta() |> mutate(Prc_lecon_F = Prc_lecon_F/100) |>  select(Prc_lecon_F) |> pull() |> round(1) |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> mutate(Prc_lecon_M = Prc_lecon_M/100) |> select(Prc_lecon_M) |> pull() |> round(1) |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> select(Prc_lecon) |> pull() |> scales::percent())
               ),
               tags$tr(
                 tags$td(align = "left", strong("Total")),
                 tags$td(align = "left", ecole_eta() |> select(note_moy) |> pull() |> round(1)),
                 tags$td(align = "left", ecole_eta() |> mutate(Prc_tot_F = Prc_tot_F/100) |> select(Prc_tot_F) |> pull() |> round(1) |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> mutate(Prc_tot_M = Prc_tot_M/100) |>  select(Prc_tot_M) |> pull() |> round(1) |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> select(Prc_tot) |> pull() |> scales::percent())
               ),
             )
  )
})

output$autres_indic <- renderUI({
  tags$table(class = "table table-sm table-responsive table-stripe table-hover table-condensed",
             tags$tbody(
               tags$tr(
                 tags$th("Niveau"),
                 tags$th("Tx Promotion G"),
                 tags$th("Tx Promotion F"),
                 tags$th("Tx Promotion T"),
                 tags$th("Tx Retention G"),
                 tags$th("Tx Retention F"),
                 tags$th("Tx Retention T"),
               ),
               tags$tr(
                 tags$td(align = "left", strong("CP1")),
                 tags$td(align = "left", "-"),
                 tags$td(align = "left", "-"),
                 tags$td(align = "left", "-"),
                 tags$td(align = "left", "-"),
                 tags$td(align = "left", "-"),
                 tags$td(align = "left", "-"),
               ),
               tags$tr(
                 tags$td(align = "left", strong("CP2")),
                 tags$td(align = "left", ecole_eta() |> select(TP_G_CP2) |> pull()|> round(1) |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> select(TP_F_CP2) |> pull()|> round(1) |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> select(TP_CP2) |> pull()|> round(1) |> scales::percent()),
                 tags$td(align = "left", "-"),
                 tags$td(align = "left", "-"),
                 tags$td(align = "left", "-"),
               ),
               tags$tr(
                 tags$td(align = "left", strong("CE1")),
                 tags$td(align = "left", ecole_eta() |> select(TP_G_CE1) |> pull()|> round(1) |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> select(TP_F_CE1) |> pull()|> round(1) |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> select(TP_CE1) |> pull()|> round(1) |> scales::percent()),
                 tags$td(align = "left", "-"),
                 tags$td(align = "left", "-"),
                 tags$td(align = "left", "-"),
               ),
               tags$tr(
                 tags$td(align = "left", strong("CE2")),
                 tags$td(align = "left", ecole_eta() |> select(TP_G_CE2) |> pull()|> round(1) |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> select(TP_F_CE2) |> pull()|> round(1) |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> select(TP_CE2) |> pull()|> round(1) |> scales::percent()),
                 tags$td(align = "left", "-"),
                 tags$td(align = "left", "-"),
                 tags$td(align = "left", "-"),
               ),
               tags$tr(
                 tags$td(align = "left", strong("CM1")),
                 tags$td(align = "left", ecole_eta() |> select(TP_G_CM1) |> pull()|> round(1) |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> select(TP_F_CM1) |> pull()|> round(1) |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> select(TP_CM1) |> pull()|> round(1) |> scales::percent()),
                 tags$td(align = "left", "-"),
                 tags$td(align = "left", "-"),
                 tags$td(align = "left", "-"),
               ),
               tags$tr(
                 tags$td(align = "left", strong("CM2")),
                 tags$td(align = "left", ecole_eta() |> select(TP_G_CM2) |> pull()|> round(1) |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> select(TP_F_CM2) |> pull()|> round(1) |> scales::percent()),
                 tags$td(align = "left", ecole_eta() |> select(TP_CM2) |> pull()|> round(1) |> scales::percent()),
                 tags$td(align = "left", "-"),
                 tags$td(align = "left", "-"),
                 tags$td(align = "left", "-"),
               ),
               tags$tr(class="table-success fw-bold",
                       tags$td(align = "left", strong("Total")),
                       tags$td(align = "left", "-"),
                       tags$td(align = "left", "-"),
                       tags$td(align = "left", "-" ),
                       tags$td(align = "left", ecole_eta() |> select("TR_G") |> pull()|> round(1) |> scales::percent() ),
                       tags$td(align = "left", ecole_eta() |> select("TR_F") |> pull()|> round(1) |> scales::percent() ),
                       tags$td(align = "left", ecole_eta() |> select("TR_T") |> pull()|> round(1) |> scales::percent() ),
               )
             )
  )
})


geo_data <- reactive({
  geo_etab |> 
    filter(IEPP == req(input$IEPP_map),CODE_TYPE_ANNEE == 16) |> 
    mutate(couleur = case_when(
      LIBELLE_TYPE_STATUT_ETAB=="Privé Laïc" ~ "#7F163D",
      LIBELLE_TYPE_STATUT_ETAB=="Public" ~ "#355070",
      LIBELLE_TYPE_STATUT_ETAB=="Privé Protestant" ~ "#eaac8b",
      LIBELLE_TYPE_STATUT_ETAB=="Privé Catholique" ~ "#b56576",
      LIBELLE_TYPE_STATUT_ETAB=="Privé Islamique" ~ "#e56b6f",
      LIBELLE_TYPE_STATUT_ETAB=="Communautaire" ~ "#e88c7d")
      )
   # mutate(color = ifelse(NOM_ETABLISSEMENT == input$ecole_map,"#7F163D","#4895ef"))
  }) #|> bindCache(input$IEPP_map) geo_data

output$map <- renderLeaflet({
  #df <- geo_data()
  pal <- colorFactor(c("#b56576","#7F163D","#e56b6f","#eaac8b","#e88c7d","#355070"), geo_data()$LIBELLE_TYPE_STATUT_ETAB)
  leaflet() |> 
    addTiles() |>
    addCircleMarkers(data = geo_data(),lat = ~lat,lng = ~lon,label = ~ NOM_ETABLISSEMENT, layerId = ~ CODE_ETABLISSEMENT,fillOpacity = 0.9,fillColor = pal(geo_data()$LIBELLE_TYPE_STATUT_ETAB),stroke = F) |>
    addLegend(position = "topright", pal = pal, values = geo_data()$LIBELLE_TYPE_STATUT_ETAB) |> 
    setView(mean(geo_data()$lon,na.rm = TRUE),mean(geo_data()$lat,na.rm = TRUE),zoom = 12)
}) 


# Gestion des clics sur les marqueurs
observeEvent(input$map_marker_click, {
  click <- req(input$map_marker_click)
  point <- reactive({
    geo_etab |> 
      dplyr::filter(CODE_ETABLISSEMENT == click$id,CODE_TYPE_ANNEE == 16)
    })
  
  if (!is.null(click)) {
    
    modalEcole(df = point)

  }
})


}