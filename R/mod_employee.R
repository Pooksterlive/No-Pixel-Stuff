mod_employee_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Employee Management"),
    actionButton(ns("add"), "Add employee", class = "btn-primary"),
    actionButton(ns("edit"), "Edit selected employee", class = "btn-secondary"),
    br(),
    br(),
    DTOutput(ns("table"))
  )
}

mod_employee_server <- function(id, changed = reactiveVal(0)) {
  moduleServer(id, function(input, output, session) {
    employees <- reactive({
      changed()
      read_employees()
    })

    output$table <- renderDT({
      datatable(
        employees(),
        rownames = FALSE,
        selection = "single",
        options = list(pageLength = 8)
      )
    })

    observeEvent(input$add, {
      showModal(modalDialog(
        textInput(session$ns("name"), "Name"),
        selectInput(session$ns("role"), "Role", EMPLOYEE_ROLES),
        checkboxInput(session$ns("active"), "Active", TRUE),
        dateInput(session$ns("hire_date"), "Hire date", value = Sys.Date()),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("save"), "Save", class = "btn-primary")
        )
      ))
    })

    observeEvent(input$save, {
      err <- validate_employee(input)
      if (!is.null(err)) {
        return(showNotification(err, type = "error"))
      }

      data <- read_employees()
      row <- data.frame(
        State_ID = next_id(data, "State_ID"),
        Name = input$name,
        Role = input$role,
        Active = isTRUE(input$active),
        Hire_Date = as.character(input$hire_date),
        stringsAsFactors = FALSE
      )
      write_csv(rbind(data, row), file_paths$employees)
      removeModal()
      changed(changed() + 1)
      showNotification("Employee added.")
    })

    observeEvent(input$edit, {
      selected <- input$table_rows_selected
      data <- employees()

      if (is.null(selected) || !length(selected) || selected[1] > nrow(data)) {
        return(showNotification("Select an employee to edit.", type = "warning"))
      }

      employee <- data[selected[1], , drop = FALSE]
      showModal(modalDialog(
        title = paste("Edit employee:", employee$Name),
        selectInput(
          session$ns("edit_role"),
          "Role",
          choices = EMPLOYEE_ROLES,
          selected = employee$Role
        ),
        checkboxInput(
          session$ns("edit_active"),
          "Active",
          value = isTRUE(employee$Active)
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("save_edit"), "Save changes", class = "btn-primary")
        )
      ))
    })

    observeEvent(input$save_edit, {
      selected <- input$table_rows_selected
      data <- read_employees()

      if (is.null(selected) || !length(selected) || selected[1] > nrow(employees())) {
        removeModal()
        return(showNotification("Select an employee to edit.", type = "warning"))
      }

      selected_id <- employees()$State_ID[selected[1]]
      row_index <- which(as.character(data$State_ID) == as.character(selected_id))[1]
      if (is.na(row_index)) {
        removeModal()
        return(showNotification("The selected employee could not be found.", type = "error"))
      }

      data$Role[row_index] <- input$edit_role
      data$Active[row_index] <- isTRUE(input$edit_active)
      write_csv(data, file_paths$employees)
      removeModal()
      changed(changed() + 1)
      showNotification("Employee updated.")
    })
  })
}
