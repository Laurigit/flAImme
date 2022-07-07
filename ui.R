#library(shiny)
#library(shinydashboard)

# Define UI for application that draws a histogram


uusi_peli <- dashboardBody(

  useShinyjs(),
  tags$script("$(\"input:radio[name='blue_setup'][value='Human']\").parent().css('background-color', '#DE6B63');"),

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
    tabItem(tabName = "tab_server",
            fluidPage(
                  uiOutput({output_id = "join_tournament"}),
                  uiOutput("bot_status")

            )
    )
  ))

#SIDEBAR
sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebarmenu",

              menuItem("Server", icon = icon("beer"), tabName = "tab_server")

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





