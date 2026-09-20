mod_inventory_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Inventory Management"),
    fluidRow(valueBoxOutput(ns("item_count"), 6), valueBoxOutput(ns("inventory_value"), 6)),
    wellPanel(fluidRow(column(8, textInput(ns("search"), "Search inventory", placeholder = "Item ID or name...")), column(4, br(), actionButton(ns("add"), "Add item", class = "btn-primary")))),
    DTOutput(ns("table"))
  )
}

mod_inventory_server <- function(id, changed = reactiveVal(0)) {
  moduleServer(id, function(input, output, session) {
    items <- reactive({
      changed()
      data <- read_inventory()
      term <- tolower(trimws(input$search %||% ""))
      if (nzchar(term) && nrow(data)) data <- data[grepl(term, tolower(paste(data$item_id, data$name)), fixed = TRUE), , drop = FALSE]
      data
    })

    output$table <- renderDT(datatable(items(), rownames = FALSE, options = list(pageLength = 8)))
    output$item_count <- renderValueBox(valueBox(nrow(items()), "Items", icon = icon("boxes")))
    output$inventory_value <- renderValueBox(valueBox(currency(sum(items()$price)), "Inventory value", icon = icon("dollar-sign")))

    observeEvent(input$add, {
      showModal(modalDialog(
        textInput(session$ns("name"), "Item name"),
        numericInput(session$ns("price"), "Price", 0, min = 0),
        footer = tagList(modalButton("Cancel"), actionButton(session$ns("save"), "Save", class = "btn-primary"))
      ))
    })

    observeEvent(input$save, {
      req(input$name)
      err <- validate_item(input)
      if (!is.null(err)) return(showNotification(err, type = "error"))
      data <- read_inventory()
      row <- data.frame(item_id = next_id(data, "item_id"), name = input$name, price = input$price, stringsAsFactors = FALSE)
      write_csv(rbind(data, row), file_paths$inventory)
      removeModal()
      changed(changed() + 1)
      showNotification("Inventory item added.")
    })
  })
}
