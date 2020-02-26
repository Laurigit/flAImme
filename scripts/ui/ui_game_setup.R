tabItem(tabName = "tab_game_setup",
        fluidPage(
          fluidRow(
            column(6, uiOutput("select_track")),
            column(6, actionButton("go_to_add_track_tab",
                                   label = "Add custom track"))
          )
        )
)
