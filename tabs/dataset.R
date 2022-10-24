dataset_tab <- fluidRow(
        column(
            6,
            offset=4,
            sidebarLayout(
                fileInput("file", "Choose CSV file", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                mainPanel(
                    tableOutput("data")
                )
            ),
            # add margin
            style = "margin-top: 10rem;"
        ),
)

