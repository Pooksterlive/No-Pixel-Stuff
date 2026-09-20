source("R/global.R")
source("R/constants.R")
source("R/csv_data.R")
source("R/validation.R")
source("R/mod_inventory.R")
source("R/mod_employee.R")
source("R/mod_pos.R")

initialize_csv_files()

ui <- fluidPage(
  titlePanel("Hong Kong Pawn"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "page",
        "Navigation",
        choices = c(
          "Point of Sale" = "pos",
          "Inventory" = "inventory",
          "Employees" = "employee"
        ),
        selected = "pos"
      ),
      uiOutput("employee_selector"),
      tags$hr(),
      p(
        class = "demo-note",
        "Demo only — no real payments or sensitive data."
      )
    ),
    mainPanel(
      uiOutput("page_content")
    )
  )
)

server <- function(input, output, session) {
  inventory_changed <- reactiveVal(0)
  employee_changed <- reactiveVal(0)
  employees <- reactive({
    employee_changed()
    read_employees()
  })

  output$employee_selector <- renderUI({
    data <- employees()
    active <- data[data$Active == TRUE, , drop = FALSE]
    choices <- setNames(
      active$State_ID,
      paste(active$Name, "—", active$Role)
    )

    selectInput(
      "State_ID",
      "Current employee",
      choices = choices,
      selected = if (nrow(active)) active$State_ID[1]
    )
  })

  output$page_content <- renderUI({
    switch(
      input$page,
      pos = mod_pos_ui("pos"),
      inventory = mod_inventory_ui("inventory"),
      employee = mod_employee_ui("employee")
    )
  })

  mod_pos_server(
    "pos",
    State_ID = reactive(input$State_ID),
    changed = inventory_changed
  )
  mod_inventory_server("inventory", changed = inventory_changed)
  mod_employee_server("employee", changed = employee_changed)
}

shinyApp(ui, server)
