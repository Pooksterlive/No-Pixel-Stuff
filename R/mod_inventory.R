mod_inventory_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Inventory Management"),
    p("Edit an item name or price directly in the table. Changes are saved to data/inventory.csv."),
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
        editable = list(
          target = "cell",
          disable = list(columns = 0)
        ),
        options = list(pageLength = 10, dom = "tip")
      )
    }, server = FALSE)

    observeEvent(input$table_cell_edit, {
      edit <- input$table_cell_edit
      data <- read_inventory()

      if (!nrow(data) || edit$row < 1 || edit$row > nrow(items())) return()
      displayed_row <- items()[edit$row, , drop = FALSE]
      item_index <- which(as.character(data$item_id) == as.character(displayed_row$item_id))[1]
      if (is.na(item_index)) return()

      column_name <- names(items())[edit$col + 1]
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
  })
}
