mod_employee_ui <- function(id) {
  ns <- NS(id)
  tagList(h2("Employee Management"), actionButton(ns("add"), "Add employee", class = "btn-primary"), br(), br(), DTOutput(ns("table")))
}

mod_employee_server <- function(id, changed = reactiveVal(0)) {
  moduleServer(id, function(input, output, session) {
    employees <- reactive({ changed(); read_employees() })
    output$table <- renderDT(datatable(employees(), rownames = FALSE, options = list(pageLength = 8)))

    observeEvent(input$add, {
      showModal(modalDialog(
        textInput(session$ns("name"), "Name"),
        selectInput(session$ns("role"), "Role", EMPLOYEE_ROLES),
        checkboxInput(session$ns("active"), "Active", TRUE),
        dateInput(session$ns("hire_date"), "Hire date", value = Sys.Date()),
        footer = tagList(modalButton("Cancel"), actionButton(session$ns("save"), "Save", class = "btn-primary"))
      ))
    })

    observeEvent(input$save, {
      err <- validate_employee(input)
      if (!is.null(err)) return(showNotification(err, type = "error"))
      data <- read_employees()
      row <- data.frame(State_ID = next_id(data, "State_ID"), Name = input$name, Role = input$role, Active = isTRUE(input$active), Hire_Date = as.character(input$hire_date), stringsAsFactors = FALSE)
      write_csv(rbind(data, row), file_paths$employees)
      removeModal()
      changed(changed() + 1)
      showNotification("Employee added.")
    })
  })
}
