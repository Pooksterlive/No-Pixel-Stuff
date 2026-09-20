source("R/global.R")
source("R/constants.R")
source("R/helpers.R")
source("R/csv_data.R")
source("R/validation.R")
source("R/mod_inventory.R")
source("R/mod_employee.R")
source("R/mod_pos.R")

initialize_csv_files()

ui <- page_sidebar(
  title = "Hong Kong Pawn",
  sidebar = sidebar(
    radioButtons("page", "Navigation", c("Point of Sale" = "pos", "Inventory" = "inventory", "Employees" = "employee"), selected = "pos"),
    uiOutput("employee_selector"),
    p(class = "demo-note", "Demo only — no real payments or sensitive data.")
  ),
  uiOutput("page_content")
)

server <- function(input, output, session) {
  inventory_changed <- reactiveVal(0)
  employee_changed <- reactiveVal(0)
  employees <- reactive({ employee_changed(); read_employees() })

  output$employee_selector <- renderUI({
    data <- employees()
    choices <- setNames(data$employee_id, paste(data$first_name, data$last_name, "—", data$role))
    selectInput("employee_id", "Current employee", choices = choices, selected = if (nrow(data)) data$employee_id[1])
  })

  output$page_content <- renderUI({
    switch(input$page,
      pos = mod_pos_ui("pos"),
      inventory = mod_inventory_ui("inventory"),
      employee = mod_employee_ui("employee")
    )
  })

  mod_pos_server("pos", employee_id = reactive(input$employee_id), changed = inventory_changed)
  mod_inventory_server("inventory", changed = inventory_changed)
  mod_employee_server("employee", changed = employee_changed)
}

shinyApp(ui, server)
