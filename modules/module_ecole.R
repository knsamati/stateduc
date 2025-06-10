library(shiny)
library(ggplot2)


modalUI <- function(id) {
  # Unique variable name
  ns <- NS(id)
  page_sidebar(
    sidebar = sidebar(
      selectInput(ns("DRE"), "Direction Regional de l'Education", choices = unique(df_ecole$DRE)),
      selectInput(ns("IEPP"), "Inspection", choices = ""),
      selectInput(ns("ecole"), "Ecole", choices = ""),
      selectInput(ns("an_sco"), "Selectionner l'année scolaire", choices = unique(df_ecole$CODE_TYPE_ANNEE)),
    ),
    layout_column_wrap(
      width = 1 / 4,
      value_box(
        title = "# Elèves",
        value = textOutput("eleve_ecole"),
        theme = "bg-gradient-red-cyan",
        showcase = fontawesome::fa_i("people-roof")
      ),
      value_box(
        title = "# Enseignants",
        value = textOutput("ens_ecole"),
        theme = "bg-gradient-red-cyan",
        showcase = fontawesome::fa_i("person-chalkboard")
      ),
      value_box(
        title = "% filles",
        value = textOutput("fille_ecole"),
        theme = "bg-gradient-red-cyan",
        showcase = fontawesome::fa_i("children")
      ),
      value_box(
        title = "Tx redoublement",
        value = textOutput("redoub_ecole"),
        theme = "bg-gradient-red-cyan",
        showcase = fontawesome::fa_i("user-graduate")
      )
    ),
    layout_columns(
      card(
        card_header(h3("Contexte de l'école")),
        uiOutput(ns("contexte_ecole"),inline = TRUE,class = "stats")
      ),
      card(
        card_header(h3("Effectif de l'école")),
        uiOutput(ns("effetif_ecole"))
      )
    ),
    layout_columns(
      card(
        card_header(h3("Moyen humain")),
        uiOutput(ns("humain_ecole"),inline = TRUE,class = "stats")
      ),
      card(
        card_header(h3("Moyen matériel")),
        uiOutput(ns("materiel_ecole"))
      )
    )
    
  )
  
}



modalServer <- function(df){
  moduleServer(
    id = id,
    module = function(input, output, session){
      
      output$eleve_ecole <- renderText({
        n_eleve <- df() |> select(Eff_tot) |> pull()
        validate(need(n_eleve, "No data"))
        scales::number(n_eleve, big.mark = ",")
      })
      output$ens_ecole <- renderText({
        n_ens <- df() |> select(Eff_Ens) |> pull()
        validate(need(n_ens, "No data"))
        scales::number(n_ens, big.mark = ",")
      })
      
      output$fille_ecole <- renderText({
        Pourc_fille <- df() |> select(PrcF_Tot) |> pull()
        validate(need(Pourc_fille, "No data"))
        scales::percent(Pourc_fille)
      })
      
      output$redoub_ecole <- renderText({
        taux_redoublement <- df() |> select(TRed_T) |> pull()
        validate(need(taux_redoublement, "No data"))
        scales::percent(taux_redoublement)
      })
      
      output$contexte_ecole <- renderUI({
        tags$table(class = "table table-sm table-responsive table-stripe table-hover table-condensed",
                   tags$tbody(
                     tags$tr(
                       tags$td(align = "left",strong("Statut") ),
                       tags$td(align = "left", df() |> select(LIBELLE_TYPE_STATUT_ETAB) |> pull())
                     ),
                     tags$tr(
                       tags$td(align = "left",strong("Milieu") ),
                       tags$td(align = "left", df() |> select(LIBELLE_TYPE_MILIEU) |> pull())
                     ),
                     tags$tr(
                       tags$td(align = "left",strong("Electricité") ),
                       tags$td(align = "left", df() |> mutate(elect = ifelse(EXISTE_ELECT == 1,"Oui","Non")) |> select(elect) |> pull())
                     ),
                     tags$tr(
                       tags$td(align = "left",strong("Eau Potable") ),
                       tags$td(align = "left", df() |> mutate(eau = ifelse(CODE_TYPE_SOURCE_EAU == 1,"Oui","Non")) |> select(eau) |> pull())
                     ),
                     tags$tr(
                       tags$td(align = "left",strong("Terrain Sport") ),
                       tags$td(align = "left", df() |> mutate(sport = ifelse(EXISTE_ESPACE_EPS == 1,"Oui","Non")) |> select(sport) |> pull())
                     ),
                     tags$tr(
                       tags$td(align = "left",strong("Accessibilité") ),
                       tags$td(align = "left", df() |> mutate(access = ifelse(ACCES_TOUTE_SAISON == 1,"Oui","Non")) |> select(access) |> pull())
                     ),
                     tags$tr(
                       tags$td(align = "left",strong("Cantine") ),
                       tags$td(align = "left", df() |> mutate(cant = ifelse(CANTINE_FONCT == 1,"Oui","Non")) |> select(cant) |> pull())
                     ),
                     tags$tr(
                       tags$td(align = "left",strong("Cogep") ),
                       tags$td(align = "left", df() |> mutate(cogep = ifelse(EXISTE_COGE == 1,"Oui","Non")) |>  select(cogep) |> pull())
                     ),
                     tags$tr(
                       tags$td(align = "left",strong("Lave-main") ),
                       tags$td(align = "left", df() |> mutate(lav = ifelse(LAVE_MAIN_FONCT == 1,"Oui","Non")) |> select(lav) |> pull())
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
                       tags$td(align = "left", df() |> select(An2_NB_CP1) |> pull()),
                       tags$td(align = "left", df() |> select(eff_CP1) |> pull()),
                       tags$td(align = "left", df() |> select(PrcF_CP1) |> pull() |> scales::percent()),
                       tags$td(align = "left", df() |> select(TRed_CP1) |> pull() |> scales::percent())
                     ),
                     tags$tr(
                       tags$td(align = "left", strong("CP2")),
                       tags$td(align = "left", df() |> select(An2_NB_CP2) |> pull()),
                       tags$td(align = "left", df() |> select(eff_CP2) |> pull()),
                       tags$td(align = "left", df() |> select(PrcF_CP2) |> pull() |> scales::percent()),
                       tags$td(align = "left", df() |> select(TRed_CP2) |> pull() |> scales::percent())
                     ),
                     tags$tr(
                       tags$td(align = "left", strong("CE1")),
                       tags$td(align = "left", df() |> select(An2_NB_CE1) |> pull()),
                       tags$td(align = "left", df() |> select(eff_CE1) |> pull()),
                       tags$td(align = "left", df() |> select(PrcF_CE1) |> pull() |> scales::percent()),
                       tags$td(align = "left", df() |> select(TRed_CE1) |> pull() |> scales::percent())
                     ),
                     tags$tr(
                       tags$td(align = "left", strong("CE2")),
                       tags$td(align = "left", df() |> select(An2_NB_CE2) |> pull()),
                       tags$td(align = "left", df() |> select(eff_CE2) |> pull()),
                       tags$td(align = "left", df() |> select(PrcF_CE2) |> pull() |> scales::percent()),
                       tags$td(align = "left", df() |> select(TRed_CE2) |> pull() |> scales::percent())
                     ),
                     tags$tr(
                       tags$td(align = "left", strong("CM1")),
                       tags$td(align = "left", df() |> select(An2_NB_CM1) |> pull()),
                       tags$td(align = "left", df() |> select(eff_CM1) |> pull()),
                       tags$td(align = "left", df() |> select(PrcF_CM1) |> pull() |> scales::percent()),
                       tags$td(align = "left", df() |> select(TRed_CM1) |> pull() |> scales::percent())
                     ),
                     tags$tr(
                       tags$td(align = "left", strong("CM2")),
                       tags$td(align = "left", df() |> select(An2_NB_CM2) |> pull()),
                       tags$td(align = "left", df() |> select(eff_CM2) |> pull()),
                       tags$td(align = "left", df() |> select(PrcF_CM2) |> pull() |> scales::percent()),
                       tags$td(align = "left", df() |> select(TRed_CM2) |> pull() |> scales::percent())
                     ),
                     tags$tr(class="table-success fw-bold",
                             tags$td(align = "left", strong("Total")),
                             tags$td(align = "left", df() |> select(an2_nb) |> pull()),
                             tags$td(align = "left", df() |> select(Eff_tot) |> pull()),
                             tags$td(align = "left", df() |> select(PrcF_Tot) |> pull() |> scales::percent()),
                             tags$td(align = "left", df() |> select(TRed_T) |> pull() |> scales::percent())
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
                       tags$td(align = "left", df() |> select(FoncM) |> pull()),
                       tags$td(align = "left", df() |> select(FoncF) |> pull()),
                       tags$td(align = "left", df() |> select(FoncT) |> pull()),
                     ),
                     tags$tr(
                       tags$td(align = "left", strong("Volontaires")),
                       tags$td(align = "left", df() |> select(VolM) |> pull()),
                       tags$td(align = "left", df() |> select(VolF) |> pull()),
                       tags$td(align = "left", df() |> select(VolT) |> pull()),
                     ),
                     tags$tr(
                       tags$td(align = "left", strong("Privés")),
                       tags$td(align = "left", df() |> select(PrivM) |> pull()),
                       tags$td(align = "left", df() |> select(PrivF) |> pull()),
                       tags$td(align = "left", df() |> select(PrivT) |> pull()),
                     ),
                     tags$tr(class="table-success fw-bold",
                             tags$td(align = "left", strong("REM")),
                             tags$td(align = "left", "-"),
                             tags$td(align = "left", "-"),
                             tags$td(align = "left", df() |> select(REM) |> pull()),
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
                       tags$td(align = "left", df() |> select(Lecture_CP1) |> pull()),
                       tags$td(align = "left", df() |> select(RMFE_CP1) |> pull()),
                       tags$td(align = "left", df() |> select(Calcul_CP1) |> pull()),
                       tags$td(align = "left", df() |> select(RMME_CP1) |> pull()),
                     ),
                     tags$tr(
                       tags$td(align = "left", strong("CP2")),
                       tags$td(align = "left", df() |> select(Lecture_CP2) |> pull()),
                       tags$td(align = "left", df() |> select(RMFE_CP2) |> pull()),
                       tags$td(align = "left", df() |> select(Calcul_CP2) |> pull()),
                       tags$td(align = "left", df() |> select(RMME_CP2) |> pull()),
                     ),
                     tags$tr(
                       tags$td(align = "left", strong("CE1")),
                       tags$td(align = "left", df() |> select(Lecture_CE1) |> pull()),
                       tags$td(align = "left", df() |> select(RMFE_CE1) |> pull()),
                       tags$td(align = "left", df() |> select(Calcul_CE1) |> pull()),
                       tags$td(align = "left", df() |> select(RMME_CE1) |> pull()),
                     ),
                     tags$tr(
                       tags$td(align = "left", strong("CE2")),
                       tags$td(align = "left", df() |> select(Lecture_CE2) |> pull()),
                       tags$td(align = "left", df() |> select(RMFE_CE2) |> pull()),
                       tags$td(align = "left", df() |> select(Calcul_CE2) |> pull()),
                       tags$td(align = "left", df() |> select(RMME_CE2) |> pull()),
                     ),
                     tags$tr(
                       tags$td(align = "left", strong("CM1")),
                       tags$td(align = "left", df() |> select(Lecture_CM1) |> pull()),
                       tags$td(align = "left", df() |> select(RMFE_CM1) |> pull()),
                       tags$td(align = "left", df() |> select(Calcul_CM1) |> pull()),
                       tags$td(align = "left", df() |> select(RMME_CM1) |> pull()),
                     ),
                     tags$tr(
                       tags$td(align = "left", strong("CM2")),
                       tags$td(align = "left", df() |> select(Lecture_CM2) |> pull()),
                       tags$td(align = "left", df() |> select(RMFE_CM2) |> pull()),
                       tags$td(align = "left", df() |> select(Calcul_CM2) |> pull()),
                       tags$td(align = "left", df() |> select(RMME_CM2) |> pull()),
                     ),
                     tags$tr(class="table-success fw-bold",
                             tags$td(align = "left", strong("Total")),
                             tags$td(align = "left", df() |> select(ManF_T) |> pull()),
                             tags$td(align = "left", df() |> select(RMFE_T) |> pull()),
                             tags$td(align = "left", df() |> select(ManM_T) |> pull() ),
                             tags$td(align = "left", df() |> select(RMME_T) |> pull() ),
                     )
                   )
        )
      })
      
    }
  )
}


