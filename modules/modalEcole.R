
modalEcole <- function(df){
  showModal(modalDialog(
    title = paste("Détails du point: ", df()$NOM_ETABLISSEMENT),
    tagList(
      h3(df()$NOM_ETABLISSEMENT),
      h3(paste0("Localité: ", df()$Village)),
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
          ),
          card(full_screen = TRUE, 
               card_header("Resultat au CEPD"), 
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
                              tags$td(align = "left", df() |> select(note_moyFr) |> pull() |> round(1)),
                              tags$td(align = "left", df() |> mutate(Prc_fr_F = Prc_fr_F/100) |> select(Prc_fr_F) |> pull()|> round(1) |> scales::percent()),
                              tags$td(align = "left", df() |> mutate(Prc_fr_G = Prc_fr_G/100) |> select(Prc_fr_G) |> pull() |> round(1) |> scales::percent()),
                              tags$td(align = "left", df() |> select(Prc_fr) |> pull() |> scales::percent())
                            ),
                            tags$tr(
                              tags$td(align = "left", strong("Math")),
                              tags$td(align = "left", df() |> select(note_moyMath) |> pull()|> round(1)),
                              tags$td(align = "left", df() |> mutate(Prc_math_F = Prc_math_F/100) |> select(Prc_math_F) |> pull() |> round(1) |> scales::percent()),
                              tags$td(align = "left", df() |> mutate(Prc_math_M = Prc_math_M/100) |> select(Prc_math_M) |> pull() |> round(1) |> scales::percent()),
                              tags$td(align = "left", df() |> select(Prc_math) |> pull() |> scales::percent())
                            ),
                            tags$tr(
                              tags$td(align = "left", strong("Lecons")),
                              tags$td(align = "left", df() |> select(note_moyLecon) |> pull()|> round(1)),
                              tags$td(align = "left", df() |> mutate(Prc_lecon_F = Prc_lecon_F/100) |>  select(Prc_lecon_F) |> pull() |> round(1) |> scales::percent()),
                              tags$td(align = "left", df() |> mutate(Prc_lecon_M = Prc_lecon_M/100) |> select(Prc_lecon_M) |> pull() |> round(1) |> scales::percent()),
                              tags$td(align = "left", df() |> select(Prc_lecon) |> pull() |> scales::percent())
                            ),
                            tags$tr(
                              tags$td(align = "left", strong("Total")),
                              tags$td(align = "left", df() |> select(note_moy) |> pull() |> round(1)),
                              tags$td(align = "left", df() |> mutate(Prc_tot_F = Prc_tot_F/100) |> select(Prc_tot_F) |> pull() |> round(1) |> scales::percent()),
                              tags$td(align = "left", df() |> mutate(Prc_tot_M = Prc_tot_M/100) |>  select(Prc_tot_M) |> pull() |> round(1) |> scales::percent()),
                              tags$td(align = "left", df() |> select(Prc_tot) |> pull() |> scales::percent())
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
               card_header("Ressources Materielles"),
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
          ),
          card(full_screen = TRUE, 
               card_header("Ressources Humaines"), 
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
               
          )
        )
        
      )
    ),
    easyClose = TRUE,
    footer = modalButton("Fermer"),
    size = "xl"
  ))
}
