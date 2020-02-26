#library(shiny)
#library(shinydashboard)

# Define UI for application that draws a histogram


uusi_peli <- dashboardBody(




  tags$head(
    tags$style(
      HTML("
           #myScrollBox{
           overflow-y: scroll;
           overflow-x: hidden;
           height:740px;
           }
           ")
      )
    ,

    tags$style(type = "text/css", "
               .irs-slider {width: 30px; height: 30px; top: 22px;}
               ")


    ),
  tabItems(
    #  source("./scripts/ui/ui_uusi_peli.R",local = TRUE)$value,
    #  source("./scripts/ui/ui_tallenna_peli.R",local = TRUE)$value,
    source("./scripts/ui/ui_game_setup.R",local = TRUE)$value,
    source("./scripts/ui/ui_add_track.R",local = TRUE)$value,
    source("./scripts/ui/ui_start_positions.R",local = TRUE)$value,
    source("./scripts/ui/ui_deal_cards.R",local = TRUE)$value,
    source("./scripts/ui/ui_play_card.R",local = TRUE)$value,
    source("./scripts/ui/ui_input_other_moves.R",local = TRUE)$value
    # source("./scripts/ui/ui_pakkaupload.R",local = TRUE)$value,
    # source("./scripts/ui/ui_saavutusasetukset.R",local = TRUE)$value,
    # source("./scripts/ui/ui_boosterit.R",local = TRUE)$value,
    # source("./scripts/ui/ui_decks.R",local = TRUE)$value,
    # source("./scripts/ui/ui_deck_lists.R",local = TRUE)$value
    #  source("./scripts/ui/ui_life_counter.R",local = TRUE)$value,

  ))

#SIDEBAR
sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebarmenu",

              menuItem("Game setup", icon = icon("beer"), tabName = "tab_game_setup"),
              menuItem("Add custom track", icon = icon("trophy"), tabName = "tab_add_track"),
              menuItem("Start positions",icon = icon("bar-chart"),tabName = "tab_start_positions"),
              menuItem("Deal cards",icon = icon("bullseye"),tabName = "tab_deal_cards"),
              menuItem('Play card', icon = icon("tasks") ,tabName = 'tab_play_card'),
              menuItem('Input human moves',  icon = icon("sliders-h"),tabName = 'input_other_moves')

  )


)

#RUNKO
dashboardPage( title = "flAImme Rouge",

               #dashboardHeader(title = paste0("run_mode = ", GLOBAL_test_mode, " ", textOutput('blow_timer')),
               #  dashboardHeader(title = textOutput('blow_timer'),
               #                 titleWidth = 450),

               dashboardHeader(title = textOutput('Username')),

               sidebar,
               uusi_peli
)





