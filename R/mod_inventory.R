mod_inventory_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Inventory Management"),
    p("Edit an item name or price directly in the table. Changes are saved to data/inventory.csv."),
    div(
      actionButton(ns("add"), "Add item", class = "btn-primary"),
      actionButton(ns("remove"), "Remove selected item", class = "btn-danger"),
      style = "margin-bottom: 15px;"
    ),
    DTOutput(ns("table"))
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
