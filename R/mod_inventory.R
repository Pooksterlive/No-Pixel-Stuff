mod_inventory_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Inventory Management"),
    div(
      class = "page-card",
      p("Edit an item name or price directly in the table. Changes are saved to data/inventory.csv."),
      div(
        actionButton(ns("add"), "Add item", class = "btn-primary"),
        actionButton(ns("remove"), "Remove selected item", class = "btn-danger")
      )
    ),
    div(
      class = "page-card",
      DTOutput(ns("table"))
    )
  )
}

mod_inventory_server <- function(id, changed = reactiveVal(0)) {
  moduleServer(id, function(input, output, session) {
    items <- reactive({
      changed()
      read_inventory()
    })

    output$table <- renderDT({
      datatable(
        items(),
        rownames = FALSE,
        selection = "single",
        editable = list(
          target = "cell",
          disable = list(columns = 0)
        ),
        options = list(
          pageLength = 10,
          dom = "tip",
          columnDefs = list(
            list(
              targets = which(names(items()) == "price") - 1,
              className = "inventory-price-cell"
            )
          )
        )
      )
    }, server = FALSE)

    observeEvent(input$table_cell_edit, {
      edit <- input$table_cell_edit
      data <- read_inventory()
      visible_data <- items()

      if (!nrow(data) || edit$row < 1 || edit$row > nrow(visible_data)) return()
      displayed_row <- visible_data[edit$row, , drop = FALSE]
      item_index <- which(as.character(data$item_id) == as.character(displayed_row$item_id))[1]
      if (is.na(item_index)) return()

      column_name <- names(visible_data)[edit$col + 1]
      if (!column_name %in% c("name", "price")) return()

      if (column_name == "name") {
        value <- trimws(as.character(edit$value))
        if (!nzchar(value)) {
          showNotification("Item name cannot be empty.", type = "error")
          return()
        }
        data[item_index, column_name] <- value
      }

      if (column_name == "price") {
        value <- suppressWarnings(as.numeric(edit$value))
        if (is.na(value) || value < 0) {
          showNotification("Price must be a non-negative number.", type = "error")
          return()
        }
        data[item_index, column_name] <- value
      }

      write_csv(data, file_paths$inventory)
      changed(changed() + 1)
      showNotification("Inventory changes saved.", type = "message")
    })

    observeEvent(input$add, {
      showModal(modalDialog(
        textInput(session$ns("new_name"), "Item name"),
        numericInput(session$ns("new_price"), "Price", value = 0, min = 0),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("save_new"), "Add item", class = "btn-primary")
        )
      ))
    })

    observeEvent(input$save_new, {
      item_name <- trimws(input$new_name %||% "")
      item_price <- suppressWarnings(as.numeric(input$new_price))

      if (!nzchar(item_name)) {
        showNotification("Item name is required.", type = "error")
        return()
      }
      if (is.na(item_price) || item_price < 0) {
        showNotification("Price must be a non-negative number.", type = "error")
        return()
      }

      data <- read_inventory()
      new_item <- data.frame(
        item_id = next_id(data, "item_id"),
        name = item_name,
        price = item_price,
        stringsAsFactors = FALSE
      )
      write_csv(rbind(data, new_item), file_paths$inventory)
      removeModal()
      changed(changed() + 1)
      showNotification("Inventory item added.", type = "message")
    })

    observeEvent(input$remove, {
      selected_row <- input$table_rows_selected
      data <- read_inventory()
      visible_data <- items()

      if (is.null(selected_row) || !length(selected_row) || !nrow(visible_data)) {
        showNotification("Select an inventory item to remove.", type = "warning")
        return()
      }

      selected_item <- visible_data[selected_row[1], , drop = FALSE]
      showModal(modalDialog(
        paste0("Remove ", selected_item$name, " from inventory?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_remove"), "Remove item", class = "btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_remove, {
      selected_row <- input$table_rows_selected
      data <- read_inventory()
      visible_data <- items()

      if (is.null(selected_row) || !length(selected_row) || !nrow(visible_data)) {
        removeModal()
        return()
      }

      selected_item_id <- visible_data$item_id[selected_row[1]]
      data <- data[as.character(data$item_id) != as.character(selected_item_id), , drop = FALSE]
      write_csv(data, file_paths$inventory)
      removeModal()
      changed(changed() + 1)
      showNotification("Inventory item removed.", type = "message")
    })
  })
}

mod_employee_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Employee Management"),
    div(
      class = "page-card",
      actionButton(ns("add"), "Add employee", class = "btn-primary"),
      actionButton(ns("edit"), "Edit selected employee", class = "btn-secondary")
    ),
    div(
      class = "page-card",
      DTOutput(ns("table"))
    )
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

mod_transactions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Transaction History"),
    div(
      class = "page-card",
      DTOutput(ns("transactions_table"))
    ),
    div(
      class = "page-card",
      uiOutput(ns("transaction_details"))
    )
  )
}

mod_transactions_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    transaction_data <- reactive({
      transactions <- read_transactions()
      if (!nrow(transactions)) return(data.frame())

      inventory <- read_inventory()
      details <- read_csv(file_paths$transaction_items)
      employees <- read_employees()

      rows <- lapply(seq_len(nrow(transactions)), function(i) {
        tx <- transactions[i, , drop = FALSE]
        tx_items <- details[details$transaction_id == tx$transaction_id, , drop = FALSE]
        employee <- employees[
          as.character(employees$State_ID) == as.character(tx$employee_State_ID),
          , drop = FALSE
        ]
        employee_name <- if (nrow(employee)) employee$Name[1] else "Unknown"

        if (!nrow(tx_items)) {
          return(data.frame(
            transaction_id = tx$transaction_id,
            transaction_number = tx$transaction_number,
            created_at = tx$created_at,
            employee_State_ID = tx$employee_State_ID,
            employee_name = employee_name,
            seller_name = tx$seller_name,
            seller_State_ID = tx$seller_State_ID,
            payment_method = tx$payment_method,
            item_name = NA_character_,
            unit_price = NA_real_,
            total = tx$total,
            stringsAsFactors = FALSE
          ))
        }

        data.frame(
          transaction_id = tx$transaction_id,
          transaction_number = tx$transaction_number,
          created_at = tx$created_at,
          employee_State_ID = tx$employee_State_ID,
          employee_name = employee_name,
          seller_name = tx$seller_name,
          seller_State_ID = tx$seller_State_ID,
          payment_method = tx$payment_method,
          item_name = inventory$name[match(tx_items$item_id, inventory$item_id)],
          unit_price = tx_items$unit_price,
          total = tx$total,
          stringsAsFactors = FALSE
        )
      })

      do.call(rbind, rows)
    })

    output$transactions_table <- renderDT({
      data <- transaction_data()
      if (!nrow(data)) {
        return(datatable(
          data.frame(message = "No transactions found."),
          rownames = FALSE,
          options = list(dom = "t")
        ))
      }

      summary <- data[!duplicated(data$transaction_id), c(
        "transaction_number",
        "seller_name",
        "employee_name",
        "total"
      ), drop = FALSE]
      names(summary) <- c("Sale ID", "Seller Name", "Employee Name", "Total")

      datatable(
        summary,
        rownames = FALSE,
        selection = "single",
        options = list(pageLength = 20, dom = "tip")
      )
    }, server = FALSE)

    output$transaction_details <- renderUI({
      selected <- input$transactions_table_rows_selected
      data <- transaction_data()

      if (!nrow(data) || is.null(selected) || !length(selected)) return(NULL)

      summary <- data[!duplicated(data$transaction_id), , drop = FALSE]
      if (selected[1] > nrow(summary)) return(NULL)
      transaction_id <- summary$transaction_id[selected[1]]
      details <- data[data$transaction_id == transaction_id, , drop = FALSE]

      employee <- read_employees()
      employee <- employee[
        as.character(employee$State_ID) == as.character(details$employee_State_ID[1]),
        , drop = FALSE
      ]
      employee_label <- if (nrow(employee)) {
        paste(employee$Name[1], "(", employee$Role[1], ")")
      } else {
        "Unknown"
      }

      tagList(
        h3(paste("Transaction details —", details$transaction_number[1])),
        fluidRow(
          column(4, strong("Sale ID"), br(), details$transaction_number[1]),
          column(4, strong("Date"), br(), details$created_at[1]),
          column(4, strong("Payment method"), br(), details$payment_method[1])
        ),
        fluidRow(
          column(4, strong("Current employee"), br(), employee_label),
          column(4, strong("Employee State ID"), br(), details$employee_State_ID[1]),
          column(4, strong("Seller name"), br(), details$seller_name[1])
        ),
        fluidRow(
          column(4, strong("Seller State ID"), br(), details$seller_State_ID[1])
        ),
        br(),
        renderTable({
          details[, c("item_name", "unit_price"), drop = FALSE]
        }, rownames = FALSE),
        strong(paste("Total:", currency(details$total[1])))
      )
    })
  })
}
