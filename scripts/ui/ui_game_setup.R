tabItem(tabName = "tab_game_setup",
        fluidPage(
          fluidRow(
            column(6, uiOutput("select_track")),
            column(6, actionButton("go_to_add_track_tab",
                                   label = "Add custom track"))
          ),
          fluidRow(
            column(6,
          fluidRow(

                   radioGroupButtons(inputId = "blue_setup",
                                         label = "Blue team",
                                         choices = c("Human", "AI", "AI autocards", "Not playing"),
                                         selected = "Human",
                                         direction = "horizontal"
                                         )
                   ),
          fluidRow( radioGroupButtons(inputId = "red_setup",
                                      label = "Red team",
                                      choices = c("Human", "AI", "AI autocards", "Not playing"),
                                      selected = "Human",
                                      direction = "horizontal"
                    )
          ),
          fluidRow( radioGroupButtons(inputId = "green_setup",
                                      label = "Green team",
                                      choices = c("Human", "AI", "AI autocards", "Not playing"),
                                      selected = "Human",
                                      direction = "horizontal"
          )
          )),
          column(6,
          fluidRow( radioGroupButtons(inputId = "black_setup",
                                      label = "Black team",
                                      choices = c("Human", "AI", "AI autocards", "Not playing"),
                                      selected = "Human",
                                      direction = "horizontal"
          )
          ),
          fluidRow( radioGroupButtons(inputId = "white_setup",
                                      label = "White team",
                                      choices = c("Human", "AI", "AI autocards", "Not playing"),
                                      selected = "Human",
                                      direction = "horizontal"
          )
          ),
          fluidRow( radioGroupButtons(inputId = "purple_setup",
                                      label = "Purple team",
                                      choices = c("Human", "AI", "AI autocards", "Not playing"),
                                      selected = "Human",
                                      direction = "horizontal"
          )
          )
        )
          ),
        fluidRow(actionButton(inputId = "go_to_start_position_tab",
                              label = "Continue",
                              width = "100%"))


    )


)

