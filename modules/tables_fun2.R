

tab_contexte <- function(df){
  tags$table(class = "table table-sm table-responsive table-stripe table-hover table-condensed",
             tags$tbody(
               tags$tr(
                 tags$td(align = "left",strong("# Ecoles") ),
                 tags$td(align = "left", df |> select(Nombre_ecole) |> pull())
               ),
               tags$tr(
                 tags$td(align = "left",strong("# Ecoles Publiques") ),
                 tags$td(align = "left", df |> select(public) |> pull())
               ),
               tags$tr(
                 tags$td(align = "left",strong("% urbain") ),
                 tags$td(align = "left", df |> mutate(urb = urbain/Nombre_ecole) |> select(urb) |> pull() |> scales::percent()),
               ),
               tags$tr(
                 tags$td(align = "left",strong("% Electricité") ),
                 tags$td(align = "left", df |> mutate(elect = elec/Nombre_ecole) |> select(elect) |> pull() |> scales::percent())
               ),
               tags$tr(
                 tags$td(align = "left",strong("% Eau Potable") ),
                 tags$td(align = "left", df |> mutate(eau = eau/Nombre_ecole) |> select(eau) |> pull() |> scales::percent())
               ),
               tags$tr(
                 tags$td(align = "left",strong("% Classes en dur") ),
                 tags$td(align = "left", df |> mutate(dur = Dur/(Dur+Banco+Autres)) |> select(dur) |> pull() |> scales::percent())
               ),
               tags$tr(
                 tags$td(align = "left",strong("% Accessible") ),
                 tags$td(align = "left", df |> mutate(access = acces/Nombre_ecole) |> select(access) |> pull() |> scales::percent())
               ),
               tags$tr(
                 tags$td(align = "left",strong("% Cantine") ),
                 tags$td(align = "left", df |> mutate(cant = cantine/Nombre_ecole) |> select(cant) |> pull()|> scales::percent())
               ),
               tags$tr(
                 tags$td(align = "left",strong("% Cogep") ),
                 tags$td(align = "left", df |> mutate(cogep = cogep/Nombre_ecole) |> select(cogep) |> pull()|> scales::percent())
               ),
               tags$tr(
                 tags$td(align = "left",strong("Lave-main") ),
                 tags$td(align = "left", df |> mutate(lav = lave_main/Nombre_ecole) |> select(lav) |> pull()|> scales::percent())
               ),
             )
  )
}


tab_effectif <- function(df){
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
                 tags$td(align = "left", df |> select(An2_NB_CP1) |> pull()),
                 tags$td(align = "left", df |> select(eff_CP1) |> pull()),
                 tags$td(align = "left", df |> select(PrcF_CP1) |> pull() |> scales::percent()),
                 tags$td(align = "left", df |> select(TRed_CP1) |> pull() |> scales::percent())
               ),
               tags$tr(
                 tags$td(align = "left", strong("CP2")),
                 tags$td(align = "left", df |> select(An2_NB_CP2) |> pull()),
                 tags$td(align = "left", df |> select(eff_CP2) |> pull()),
                 tags$td(align = "left", df |> select(PrcF_CP2) |> pull() |> scales::percent()),
                 tags$td(align = "left", df |> select(TRed_CP2) |> pull() |> scales::percent())
               ),
               tags$tr(
                 tags$td(align = "left", strong("CE1")),
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
                 tags$td(align = "left", strong("CM1")),
                 tags$td(align = "left", df |> select(An2_NB_CM1) |> pull()),
                 tags$td(align = "left", df |> select(eff_CM1) |> pull()),
                 tags$td(align = "left", df |> select(PrcF_CM1) |> pull() |> scales::percent()),
                 tags$td(align = "left", df |> select(TRed_CM1) |> pull() |> scales::percent())
               ),
               tags$tr(
                 tags$td(align = "left", strong("CM2")),
                 tags$td(align = "left", df |> select(An2_NB_CM2) |> pull()),
                 tags$td(align = "left", df |> select(eff_CM2) |> pull()),
                 tags$td(align = "left", df |> select(PrcF_CM2) |> pull() |> scales::percent()),
                 tags$td(align = "left", df |> select(TRed_CM2) |> pull() |> scales::percent())
               ),
               tags$tr(class="table-success fw-bold",
                       tags$td(align = "left", strong("Total")),
                       tags$td(align = "left", df |> select(an2_nb) |> pull()),
                       tags$td(align = "left", df |> select(Eff_tot) |> pull()),
                       tags$td(align = "left", df |> select(PrcF_Tot) |> pull() |> scales::percent()),
                       tags$td(align = "left", df |> select(TRed_T) |> pull() |> scales::percent())
               )
             )
  )
}

tab_humain <- function(df){
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
                 tags$td(align = "left", df |> select(FoncM) |> pull()),
                 tags$td(align = "left", df |> select(FoncF) |> pull()),
                 tags$td(align = "left", df |> select(FoncT) |> pull()),
               ),
               tags$tr(
                 tags$td(align = "left", strong("Volontaires")),
                 tags$td(align = "left", df |> select(VolM) |> pull()),
                 tags$td(align = "left", df |> select(VolF) |> pull()),
                 tags$td(align = "left", df |> select(VolT) |> pull()),
               ),
               tags$tr(
                 tags$td(align = "left", strong("Privés")),
                 tags$td(align = "left", df |> select(PrivM) |> pull()),
                 tags$td(align = "left", df |> select(PrivF) |> pull()),
                 tags$td(align = "left", df |> select(PrivT) |> pull()),
               ),
               tags$tr(class="table-success fw-bold",
                       tags$td(align = "left", strong("REM")),
                       tags$td(align = "left", "-"),
                       tags$td(align = "left", "-"),
                       tags$td(align = "left", df |> select(REM) |> pull()),
               ),
               
             )
             
  )
}

tab_materiel <- function(df){
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
                 tags$td(align = "left", df |> select(Lecture_CP1) |> pull()),
                 tags$td(align = "left", df |> select(RMFE_CP1) |> pull()),
                 tags$td(align = "left", df |> select(Calcul_CP1) |> pull()),
                 tags$td(align = "left", df |> select(RMME_CP1) |> pull()),
               ),
               tags$tr(
                 tags$td(align = "left", strong("CP2")),
                 tags$td(align = "left", df |> select(Lecture_CP2) |> pull()),
                 tags$td(align = "left", df |> select(RMFE_CP2) |> pull()),
                 tags$td(align = "left", df |> select(Calcul_CP2) |> pull()),
                 tags$td(align = "left", df |> select(RMME_CP2) |> pull()),
               ),
               tags$tr(
                 tags$td(align = "left", strong("CE1")),
                 tags$td(align = "left", df |> select(Lecture_CE1) |> pull()),
                 tags$td(align = "left", df |> select(RMFE_CE1) |> pull()),
                 tags$td(align = "left", df |> select(Calcul_CE1) |> pull()),
                 tags$td(align = "left", df |> select(RMME_CE1) |> pull()),
               ),
               tags$tr(
                 tags$td(align = "left", strong("CE2")),
                 tags$td(align = "left", df |> select(Lecture_CE2) |> pull()),
                 tags$td(align = "left", df |> select(RMFE_CE2) |> pull()),
                 tags$td(align = "left", df |> select(Calcul_CE2) |> pull()),
                 tags$td(align = "left", df |> select(RMME_CE2) |> pull()),
               ),
               tags$tr(
                 tags$td(align = "left", strong("CM1")),
                 tags$td(align = "left", df |> select(Lecture_CM1) |> pull()),
                 tags$td(align = "left", df |> select(RMFE_CM1) |> pull()),
                 tags$td(align = "left", df |> select(Calcul_CM1) |> pull()),
                 tags$td(align = "left", df |> select(RMME_CM1) |> pull()),
               ),
               tags$tr(
                 tags$td(align = "left", strong("CM2")),
                 tags$td(align = "left", df |> select(Lecture_CM2) |> pull()),
                 tags$td(align = "left", df |> select(RMFE_CM2) |> pull()),
                 tags$td(align = "left", df |> select(Calcul_CM2) |> pull()),
                 tags$td(align = "left", df |> select(RMME_CM2) |> pull()),
               ),
               tags$tr(class="table-success fw-bold",
                       tags$td(align = "left", strong("Total")),
                       tags$td(align = "left", df |> select(ManF_T) |> pull()),
                       tags$td(align = "left", df |> select(RMFE_T) |> pull()),
                       tags$td(align = "left", df |> select(ManM_T) |> pull() ),
                       tags$td(align = "left", df |> select(RMME_T) |> pull() ),
               )
             )
  )
}


