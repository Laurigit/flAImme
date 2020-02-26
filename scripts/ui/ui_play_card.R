tabItem(tabName = "tab_play_card",
        fluidPage(fluidRow(h3(textOutput("which_cycler_playing"))),
                  fluidRow(h3(div(id = "show_card_text", textOutput("play_card_text")))),
                  actionButton(inputId = "show_card",
                               label = "Reveal card",
                               onclick="Shiny.onInputChange('stopThis',true)"))

)
