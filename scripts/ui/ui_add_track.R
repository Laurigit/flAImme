tabItem(tabName = "tab_add_track",
        fluidPage(textInput(inputId = "track_letters",
                            label = "Type track piece codes",
                            value = "",
                            placeholder = "AebQRNHPcgikDFsLojmtu"),
                  textInput(inputId = "new_track_name",
                            label = "Type track name"),
                  actionButton("save_custom_track",
                               label = "Save custom track"))
)
