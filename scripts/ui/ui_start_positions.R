tabItem(tabName = "tab_start_positions",
fluidPage(
        column(width = 3, h2("Start grid"), uiOutput("cyclersInput", style = "min-height:200px;background-color:white;")),
        column(width = 3, h2("Peloton"), uiOutput("cyclersPeloton", style = "min-height:200px;background-color:white;")),
        actionButton("save_start_position", "Save grid setup")

),
dragula(c("cyclersInput","cyclersPeloton"), id = "dragula")
)
