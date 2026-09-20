mod_employee_ui <- function(id) tagList(h2("Employee Management"), actionButton(NS(id, "add"), "Add employee", class = "btn-primary"), br(), br(), DTOutput(NS(id, "table")))
mod_employee_server <- function(id, changed = reactiveVal(0)) {
  moduleServer(id, function(input, output, session) {
    employees <- reactive({ changed(); read_employees() })
    output$table <- renderDT(datatable(employees()[, c("employee_id", "employee_number", "first_name", "last_name", "role", "email", "active"), drop = FALSE], rownames = FALSE))
    observeEvent(input$add, showModal(modalDialog(textInput(session$ns("first_name"), "First name"), textInput(session$ns("last_name"), "Last name"), selectInput(session$ns("role"), "Role", EMPLOYEE_ROLES), textInput(session$ns("email"), "Email"), footer = tagList(modalButton("Cancel"), actionButton(session$ns("save"), "Save", class = "btn-primary"))))
    observeEvent(input$save, { err <- validate_employee(input); if (!is.null(err)) return(showNotification(err, type = "error")); data <- read_employees(); row <- data.frame(employee_id = next_id(data, "employee_id"), employee_number = new_number("EMP-"), first_name = input$first_name, last_name = input$last_name, role = input$role, email = input$email, active = TRUE, created_at = as.character(Sys.time())); write_csv(rbind(data, row), file_paths$employees); removeModal(); changed(changed() + 1); showNotification("Employee added.") })
  })
}
