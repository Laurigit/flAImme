tabItem(tabName = "tab_start_positions",
fluidPage(
        column(width = 3, h2("Peloton"), uiOutput("cyclersInput", style = "min-height:200px;background-color:white;")),
        column(width = 3, h2("Breakaway"), uiOutput("cyclersPeloton", style = "min-height:200px;background-color:white;")),
        column(width = 3, h2("Move ready here"), uiOutput("ready", style = "min-height:200px;background-color:white;")),
        actionButton("continue_to_deck_handling", "Continue", width = "100%")

),
dragula(c("cyclersInput","cyclersPeloton", "ready"), id = "dragula")
)
