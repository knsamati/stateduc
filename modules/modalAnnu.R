
modalAnnu <- function(df){
  showModal(modalDialog(
    title = paste("Détails du point: ", df$NOM_ETABLISSEMENT),
    tagList(
      h3(df$NOM_ETABLISSEMENT),
      h3(paste0("Localité: ", df$IEPP)),
      hr(),
      page_fillable(
        layout_column_wrap(
          width = 1/2,
          height = 300,
          card(full_screen = TRUE, 
               card_header("Effectif de l'école"), 
               tags$table(class = "table table-sm table-responsive table-stripe table-hover table-condensed",
                          tags$tbody(
                            tags$tr(
                              tags$td(align = "left",strong("Statut") ),
                              tags$td(align = "left", df |> select(LIBELLE_TYPE_STATUT_ETAB) |> pull())
                            ),
                            tags$tr(
                              tags$td(align = "left",strong("Milieu") ),
                              tags$td(align = "left", df |> select(LIBELLE_TYPE_MILIEU) |> pull())
                            ),
                            tags$tr(
                              tags$td(align = "left",strong("Electricité") ),
                              tags$td(align = "left", df |> mutate(elect = ifelse(EXISTE_ELECT == 1,"Oui","Non")) |> select(elect) |> pull())
                            ),
                            tags$tr(
                              tags$td(align = "left",strong("Eau Potable") ),
                              tags$td(align = "left", df |> mutate(eau = ifelse(CODE_TYPE_SOURCE_EAU == 1,"Oui","Non")) |> select(eau) |> pull())
                            ),
                            tags$tr(
                              tags$td(align = "left",strong("Terrain Sport") ),
                              tags$td(align = "left", df |> mutate(sport = ifelse(EXISTE_ESPACE_EPS == 1,"Oui","Non")) |> select(sport) |> pull())
                            ),
                            tags$tr(
                              tags$td(align = "left",strong("Accessibilité") ),
                              tags$td(align = "left", df |> mutate(access = ifelse(ACCES_TOUTE_SAISON == 1,"Oui","Non")) |> select(access) |> pull())
                            ),
                            tags$tr(
                              tags$td(align = "left",strong("Cantine") ),
                              tags$td(align = "left", df |> mutate(cant = ifelse(CANTINE_FONCT == 1,"Oui","Non")) |> select(cant) |> pull())
                            ),
                            tags$tr(
                              tags$td(align = "left",strong("Cogep") ),
                              tags$td(align = "left", df |> mutate(cogep = ifelse(EXISTE_COGE == 1,"Oui","Non")) |>  select(cogep) |> pull())
                            ),
                            tags$tr(
                              tags$td(align = "left",strong("Lave-main") ),
                              tags$td(align = "left", df |> mutate(lav = ifelse(LAVE_MAIN_FONCT == 1,"Oui","Non")) |> select(lav) |> pull())
                            ),
                          )
               )
               
          ),
          card(full_screen = TRUE, 
               card_header("Resultat au CEPD"), 
               tags$table(class = "table table-sm table-responsive table-stripe table-hover table-condensed",
                          tags$tbody(
                            tags$tr(
                              tags$th("Discipline"),
                              tags$th("Moyenne des notes"),
                              tags$th("% des files note >10"),
                              tags$th("% des Garcon note >10"),
                              tags$th("% Ens. note >10"),
                            ),
                            tags$tr(
                              tags$td(align = "left", strong("Français")),
                              tags$td(align = "left", df |> select(An2_NB_CP1) |> pull()),
                              tags$td(align = "left", df |> select(eff_CP1) |> pull()),
                              tags$td(align = "left", df |> select(PrcF_CP1) |> pull() |> scales::percent()),
                              tags$td(align = "left", df |> select(TRed_CP1) |> pull() |> scales::percent())
                            ),
                            tags$tr(
                              tags$td(align = "left", strong("Math")),
                              tags$td(align = "left", df |> select(An2_NB_CP2) |> pull()),
                              tags$td(align = "left", df |> select(eff_CP2) |> pull()),
                              tags$td(align = "left", df |> select(PrcF_CP2) |> pull() |> scales::percent()),
                              tags$td(align = "left", df |> select(TRed_CP2) |> pull() |> scales::percent())
                            ),
                            tags$tr(
                              tags$td(align = "left", strong("Lecons")),
                              tags$td(align = "left", df |> select(An2_NB_CE1) |> pull()),
                              tags$td(align = "left", df |> select(eff_CE1) |> pull()),
                              tags$td(align = "left", df |> select(PrcF_CE1) |> pull() |> scales::percent()),
                              tags$td(align = "left", df |> select(TRed_CE1) |> pull() |> scales::percent())
                            ),
                            tags$tr(
                              tags$td(align = "left", strong("CE2")),
                              tags$td(align = "left", df |> select(An2_NB_CE2) |> pull()),
                              tags$td(align = "left", df |> select(eff_CE2) |> pull()),
                              tags$td(align = "left", df |> select(PrcF_CE2) |> pull() |> scales::percent()),
                              tags$td(align = "left", df |> select(TRed_CE2) |> pull() |> scales::percent())
                            ),
                            tags$tr(
                              tags$td(align = "left", strong("Total")),
                              tags$td(align = "left", df |> select(An2_NB_CM1) |> pull()),
                              tags$td(align = "left", df |> select(eff_CM1) |> pull()),
                              tags$td(align = "left", df |> select(PrcF_CM1) |> pull() |> scales::percent()),
                              tags$td(align = "left", df |> select(TRed_CM1) |> pull() |> scales::percent())
                            ),
                          )
               )
          )
        )
        
      ),
      page_fillable(
        layout_column_wrap(
          width = 1/2,
          height = 300,
          card(full_screen = TRUE,
               card_header("A filling plot"),
               p(paste("Latitude:", round(df$TRed_CM1, 4))),
          ),
          card(full_screen = TRUE, 
               card_header("A filling map"), 
               p(paste("Longitude:", round(df$TRed_CM2, 4)))
          )
        )
        
      )
    ),
    easyClose = TRUE,
    footer = modalButton("Fermer"),
    size = "xl"
  ))
}
