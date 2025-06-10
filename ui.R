library(shiny)
library(leaflet)
library(reactable)
library(bslib)



ui <- function(req) {shinyUI(
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", 
                type = "text/css", 
                href = "styles-demo.css"),
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"),
      tags$style(HTML("
      .modal-lg {
        max-width: 80% !important;
      }
      .modal-content {
        padding: 20px;
      }
    ")),
      tags$script(src = "script.js"),
      tags$html(lang = "fr")
    ),
     page_navbar(
       id = "navbarID",
       title = HTML(paste0("StatEduc", " | " , fontawesome::fa_i("book"))),
       window_title = "StatEduc",
       theme = bs_theme(
        bootswatch = "united", version = 5,
        #"nav = justify-content-end",
        "navbar-bg" = "#7F163D",
        base_font = font_google("Poppins"),
        collapsible = TRUE
      ),
      tags$style(type = "text/css",
                 ".shiny-notification-content { visibility: hidden; }",
                 ".shiny-notification-panel { visibility: hidden; }", 
                 ".shiny-notification-error {visibility: hidden;}"
      ),
      nav_spacer(),
      nav_panel(title = "Accueil", value = "home", icon = fontawesome::fa_i("home"),
                tags$section(
                  class = "hero",
                  div(
                    class = "container",
                    h1("Découvrez notre application Shiny la diffusion des données du SIGE"),
                    p("Une solution complète pour analyser les données du SIGE avec une interface intuitive et des fonctionnalités puissantes."),
                    tags$a(href = "#features", class = "cta-button", "En savoir plus"),
                    div(id = "shinyAppContainer", style = "margin-top: 40px;")
                  )
                ),
                
                # Section Features
                tags$section(
                  class = "features",
                  id = "features",
                  div(
                    class = "container",
                    div(
                      class = "section-title",
                      h2("Fonctionnalités clés"),
                      p("Découvrez ce qui rend notre application Shiny unique et puissante")
                    ),
                    div(
                      class = "features-grid",
                      # Carte Fonctionnalité 1
                      div(
                        class = "feature-card",
                        div(class = "feature-icon", tags$i(class = "fas fa-chart-line")),
                        h3("Annuaire des données"),
                        p("Création des tableaux de bord personnalisés pour explorer ess données sous tous les angles (National jusqu'au niveau établissement)."),
                        br(),
                        actionButton(inputId = "annuaire", label = "Annuaire Statistique")
                      ),
                      # Carte Fonctionnalité 2
                      div(
                        class = "feature-card",
                        div(class = "feature-icon", tags$i(class = "fas fa-database")),
                        h3("Tableau de bord"),
                        p("Des tableaux de bord des indicateurs de pilotage pour les différents niveaux du système éducatif"),
                        br(),
                        actionButton("tdb", label = "Tableau de bord")
                      ),
                      # Carte Fonctionnalité 3
                      div(
                        class = "feature-card",
                        div(class = "feature-icon", tags$i(class = "fas fa-map")),
                        h3("Carte interactive"),
                        p("Des cartes interactives pour visualiser la distribution des établissements et des indicateurs."),
                        br(),
                        actionButton("carte", label = "Carte Scolaire")
                      )
                    )
                  )
                ),
                br(),
                # Section CTA
                tags$section(
                  class = "cta-section",
                  id = "contact",
                  div(
                    class = "container",
                    h2("Prêt à transformer votre analyse de données ?"),
                    p("Essayez notre application Shiny dès aujourd'hui et découvrez comment elle peut vous faire gagner du temps et vous fournir des insights précieux."),
                    div(
                      class = "cta-buttons",
                      tags$a(href = "#", class = "cta-button", id = "shinyDemo", "Voir la démo"),
                      tags$a(href = "#footer", class = "cta-button outline", "Contactez-nous")
                    )
                  )
                ),
                
                br(),
                
                tags$footer(
                  class = "footer",
                  id = "footer",
                  div(
                    class = "container",
                    div(
                      class = "footer-content",
                      # Colonne 1 - À propos
                      div(
                        class = "footer-column",
                        h3("ShinyApp"),
                        p("Une solution puissante pour l'analyse et la visualisation de données, intégrée dans une interface intuitive."),
                        div(
                          class = "social-links",
                          tags$a(href = "https://x.com/RSAMATI",target = "_blank", tags$i(class = "fab fa-twitter")),
                          tags$a(href = "https://www.linkedin.com/in/knsamati/",target = "_blank", tags$i(class = "fab fa-linkedin")),
                          tags$a(href = "https://github.com/knsamati", target = "_blank",tags$i(class = "fab fa-github"))
                        )
                      ),
                      # Colonne 2 - Liens rapides
                      div(
                        class = "footer-column",
                        h3("Liens rapides"),
                        tags$ul(
                          tags$li(tags$a(href = "#features", "Fonctionnalités")),
                          tags$li(tags$a(href = "#contact", "Contact")),
                          tags$li(tags$a(href = "#", "Documentation"))
                        )
                      ),
                      
                      # Colonne 3 - Ressources
                      div(
                        class = "footer-column",
                        h3("Ressources"),
                        tags$ul(
                          tags$li(tags$a(href = "#", "Blog")),
                          tags$li(tags$a(href = "#", "Tutoriels")),
                          tags$li(tags$a(href = "#", "FAQ")),
                          tags$li(tags$a(href = "#", "Support"))
                        )
                      ),
                      
                      # Colonne 4 - Contact
                      div(
                        class = "footer-column",
                        h3("Contact"),
                        tags$ul(
                          tags$li(tags$i(class = "fas fa-envelope"), " kn.samati@gmail.com"),
                          tags$li(tags$i(class = "fas fa-phone"), " +228 91850628"),
                          tags$li(tags$i(class = "fas fa-map-marker-alt"), " Lomé, Togo")
                        )
                      )
                    ),
                    
                    # Footer bottom
                    div(
                      class = "footer-bottom",
                      p("© 2023 ShinyApp. Tous droits réservés.")
                    )
                  )
                ),
                ),
      
      nav_menu(title = "Statistiques", value = "annu_stat",icon = fontawesome::fa_i("book"),
               nav_panel("National",value = "stat_pays",
                page_sidebar(
                   sidebar = sidebar(
                   uiOutput("annee")
                  ),
                  card(
                    full_screen = TRUE,
                    card_header(h3("Annuaire Statistique")),
                    reactableOutput("annutable")
                  )
                )),
               nav_panel("Inspection",
                         page_sidebar(
                           sidebar = sidebar(
                             selectInput("annu_dre", "Direction Regional de l'Education", choices = unique(df_ecole$DRE)),
                             selectInput("annu_iepp", "IEPP", choices = ""),
                             selectInput("an_annu_iepp", "Selectionner l'année scolaire", choices = unique(df_ecole$annee_scolaire)),
                             
                           ),
                           card(
                             full_screen = TRUE,
                             card_header(h3("Annuaire Statistique")),
                             reactableOutput("annutable_iepp")
                           )
                         )
                   )
               ),
      nav_menu(
        title = "Indicateurs",value = "tableau",icon = fontawesome::fa_i("table-columns"),
        nav_panel("Region", value = "tableau_dre",
                  height = "100%",
                  page_sidebar(
                  sidebar = sidebar(
                    selectInput("DRE_dre", "Direction Regional de l'Education", choices = unique(df_dre$DRE)),
                    selectInput("an_sco_dre", "Selectionner l'année scolaire", choices = unique(df_dre$annee_scolaire)),
                  ),
                  layout_column_wrap(
                    width = 1 / 4,
                    value_box(
                      title = "Elèves",
                      value = textOutput("eleve_dre"),
                      theme = "bg-gradient-teal-pink",
                      showcase = fontawesome::fa_i("people-roof")
                    ),
                    value_box(
                      title = "Enseignants",
                      value = textOutput("ens_dre"),
                      theme = "bg-gradient-teal-pink",
                      showcase = fontawesome::fa_i("person-chalkboard")
                    ),
                    value_box(
                      title = "% filles",
                      value = textOutput("fille_dre"),
                      theme = "bg-gradient-teal-pink",
                      showcase = fontawesome::fa_i("children")
                    ),
                    value_box(
                      title = "Tx redoublement",
                      value = textOutput("redoub_dre"),
                      theme = "bg-gradient-teal-pink",
                      showcase = fontawesome::fa_i("user-graduate")
                    )
                  ),
                  layout_columns(
                    
                    card(
                      full_screen = TRUE,
                      card_header(h3("Contexte de la DRE")),
                      uiOutput("contexte_dre",inline = TRUE,class = "stats")
                    ),
                    card(
                      full_screen = TRUE,
                      card_header(h3("Effectif de la DRE")),
                      uiOutput("effetif_dre")
                    )
                  ),
                  layout_columns(
                    card(
                      full_screen = TRUE,
                      card_header(h3("Moyen humain")),
                      uiOutput("humain_dre",inline = TRUE,class = "stats")
                    ),
                    card(
                      full_screen = TRUE,
                      card_header(h3("Moyen matériel")),
                      uiOutput("materiel_dre")
                    )
                  )
                  )
        ),
        nav_panel("Inspection", value = "tableau_iepp",
                  page_sidebar(
                  sidebar = sidebar(
                    selectInput("DRE_iepp", "Direction Regional de l'Education", choices = unique(df_iepp$DRE)),
                    selectInput("IEPP_iepp", "Inspection", choices = ""),
                    selectInput("an_sco_iepp", "Selectionner l'année scolaire", choices = unique(df_iepp$annee_scolaire)),
                  ),
                  layout_column_wrap(
                    width = 1 / 4,
                    value_box(
                      title = "# Elèves",
                      value = textOutput("eleve_iepp"),
                      theme = "bg-gradient-orange-red",
                      showcase = fontawesome::fa_i("people-roof")
                    ),
                    value_box(
                      title = "# Enseignants",
                      value = textOutput("ens_iepp"),
                      theme = "bg-gradient-orange-red",
                      showcase = fontawesome::fa_i("person-chalkboard")
                    ),
                    value_box(
                      title = "% filles",
                      value = textOutput("fille_iepp"),
                      theme = "bg-gradient-orange-red",
                      showcase = fontawesome::fa_i("children")
                    ),
                    value_box(
                      title = "Tx redoublement",
                      value = textOutput("redoub_iepp"),
                      theme = "bg-gradient-orange-red",
                      showcase = fontawesome::fa_i("user-graduate")
                    )
                  ),
                  layout_columns(
                    card(
                      card_header(h3("Contexte de l'inspection")),
                      uiOutput("contexte_iepp",inline = TRUE,class = "stats")
                    ),
                    card(
                      card_header(h3("Effectif de l'Inspection")),
                      uiOutput("effetif_iepp")
                    )
                  ),
                  layout_columns(
                    card(
                      card_header(h3("Moyen humain")),
                      uiOutput("humain_iepp",inline = TRUE,class = "stats")
                    ),
                    card(
                      card_header(h3("Moyen matériel")),
                      uiOutput("materiel_iepp")
                    )
                  )
                  )
        ),
        nav_panel("Ecole", value = "ecole",
                page_sidebar(
                  sidebar = sidebar(
                    selectInput("DRE", "Direction Regional de l'Education", choices = unique(df_ecole$DRE)),
                    selectInput("IEPP", "Inspection", choices = ""),
                    selectInput("ecole", "Ecole", choices = ""),
                    selectInput("an_sco", "Selectionner l'année scolaire", choices = unique(df_ecole$annee_scolaire)),
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
                      uiOutput("contexte_ecole",inline = TRUE,class = "stats")
                    ),
                    card(
                      card_header(h3("Effectif de l'école")),
                      uiOutput("effetif_ecole")
                    )
                  ),
                  layout_columns(
                    card(
                      card_header(h3("Moyen humain")),
                      uiOutput("humain_ecole",inline = TRUE,class = "stats")
                    ),
                    card(
                      card_header(h3("Moyen matériel")),
                      uiOutput("materiel_ecole")
                    )
                  ),
                  layout_columns(
                    card(
                      card_header(h3("Resultats au CEPD (pour 2015 et 2016)")),
                      uiOutput("cepd",inline = TRUE,class = "stats")
                    ),
                    card(
                      card_header(h3("Autres indicateurs")),
                      uiOutput("autres_indic")
                    )
                  )
                  
                )
                ),

      ),
      nav_panel("Carte", value = "carte_sco",icon = fontawesome::fa_i("map"),
                page_sidebar(
                  sidebar = sidebar(
                    selectInput("DRE_map", "Direction Regional de l'Education", choices = unique(df_ecole$DRE)),
                    selectInput("IEPP_map", "Inspection", choices = ""),
                    p("Il faut tourner le téléphone en mode paysage pour faire apparaitre la carte"),
                    p("Cliquer sur les points pour afficher les indicateurs des établissement")
                  ),
                  card(
                    full_screen = TRUE,
                    card_header("La repartition des écoles de l'inspection"),
                    card_body(class = "p-0",
                    leafletOutput("map", width = "100%", height = "100%")
                  ))
                  )

      ),
      nav_menu(
        title = "Analyse",icon = fontawesome::fa_i("chart-simple"),
      nav_panel("Analyse1", value = "adm1",
                p("Third page content.")
      ),
      nav_panel("Analyse2", value = "adm2",
                p("Third page content.")
      )
      )
      
    )

 )
)
}