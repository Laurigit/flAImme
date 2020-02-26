tabItem(tabName = "tab_manage_deck",
        fluidPage(
          fluidRow(
                  column(6, uiOutput("exhaust_numeric_input")),
                  column(6,   uiOutput("peloton_numeric_input"))
                )
        ),
        fluidRow(
          actionButton("save_and_start", "Save settings and start game", width = "100%")
        )

)
