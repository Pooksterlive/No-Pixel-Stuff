mod_pos_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Point of Sale"),
    h3("Seller information"),
    fluidRow(column(6, textInput(ns("seller_name"), "Seller name", placeholder = "Full name")), column(6, textInput(ns("seller_State_ID"), "Seller State ID", placeholder = "State identification number"))),
    textInput(ns("search"), "Find inventory", placeholder = "Search by item ID or name"),
    DTOutput(ns("items")),
    h3("Cart"), tableOutput(ns("cart")), strong(textOutput(ns("total"))),
    selectInput(ns("payment"), "Payment method", PAYMENT_METHODS),
    numericInput(ns("tendered"), "Amount tendered", 0, min = 0), textOutput(ns("change")),
    actionButton(ns("checkout"), "Complete sale", class = "btn-success"), actionButton(ns("clear"), "Clear cart")
  )
}

mod_pos_server <- function(id, State_ID, changed = reactiveVal(0)) {
  moduleServer(id, function(input, output, session) {
    cart <- reactiveVal(data.frame(item_id = integer(), name = character(), price = numeric(), stringsAsFactors = FALSE))
    available <- reactive({
      changed()
      data <- read_inventory()
      term <- tolower(trimws(input$search %||% ""))
      if (nzchar(term) && nrow(data)) data <- data[grepl(term, tolower(paste(data$item_id, data$name)), fixed = TRUE), , drop = FALSE]
      data
    })
    output$items <- renderDT(datatable(available(), selection = "single", rownames = FALSE))
    observeEvent(input$items_rows_selected, { row <- available()[input$items_rows_selected, , drop = FALSE]; if (nrow(row)) cart(rbind(cart(), row[, c("item_id", "name", "price"), drop = FALSE])) })
    output$cart <- renderTable(cart())
    total <- reactive(sum(cart()$price))
    output$total <- renderText(paste("Total:", currency(total())))
    output$change <- renderText(paste("Change:", currency(max(0, input$tendered - total()))))
    observeEvent(input$clear, cart(data.frame(item_id = integer(), name = character(), price = numeric(), stringsAsFactors = FALSE)))

    observeEvent(input$checkout, {
      req(nrow(cart()) > 0, State_ID())
      seller_name <- trimws(input$seller_name %||% "")
      seller_state_id <- trimws(input$seller_State_ID %||% "")
      if (!nzchar(seller_name)) return(showNotification("Seller name is required.", type = "error"))
      if (!nzchar(seller_state_id)) return(showNotification("Seller State ID is required.", type = "error"))
      employees <- read_employees()
      employee <- employees[as.character(employees$State_ID) == as.character(State_ID()) & employees$Active == TRUE, , drop = FALSE]
      if (!nrow(employee)) return(showNotification("Select an active employee.", type = "error"))
      if (input$tendered < total()) return(showNotification("Amount tendered is insufficient.", type = "error"))
      transactions <- read_transactions()
      tx_id <- next_id(transactions, "transaction_id")
      tx_number <- new_number("SALE-")
      tx <- data.frame(transaction_id = tx_id, transaction_number = tx_number, employee_State_ID = as.integer(State_ID()), seller_name = seller_name, seller_State_ID = seller_state_id, total = total(), payment_method = input$payment, created_at = as.character(Sys.time()), stringsAsFactors = FALSE)
      write_csv(rbind(transactions, tx), file_paths$transactions)
      details <- read_csv(file_paths$transaction_items)
      new_details <- data.frame(transaction_item_id = next_id(details, "transaction_item_id") + seq_len(nrow(cart())) - 1L, transaction_id = tx_id, item_id = cart()$item_id, unit_price = cart()$price)
      write_csv(rbind(details, new_details), file_paths$transaction_items)
      showNotification(paste("Sale completed:", tx_number))
      cart(data.frame(item_id = integer(), name = character(), price = numeric(), stringsAsFactors = FALSE))
      updateTextInput(session, "seller_name", value = "")
      updateTextInput(session, "seller_State_ID", value = "")
      changed(changed() + 1)
    })
  })
}
