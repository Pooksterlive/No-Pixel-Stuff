mod_pos_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Point of Sale"),
    h3("Seller information"),
    fluidRow(
      column(6, textInput(ns("seller_name"), "Seller name", placeholder = "Full name")),
      column(6, textInput(ns("seller_State_ID"), "Seller State ID", placeholder = "State identification number"))
    ),
    DTOutput(ns("items")),
    h3("Cart"),
    tableOutput(ns("cart")),
    selectInput(
      ns("discount"),
      "Discount",
      choices = setNames(seq(0, 100, by = 5), paste0(seq(0, 100, by = 5), "%")),
      selected = 0
    ),
    strong(textOutput(ns("discounted_total"))),
    selectInput(ns("payment"), "Payment method", PAYMENT_METHODS),
    numericInput(ns("tendered"), "Amount tendered", 0, min = 0),
    textOutput(ns("change")),
    actionButton(ns("checkout"), "Complete sale", class = "btn-success"),
    actionButton(ns("clear"), "Clear cart")
  )
}

mod_pos_server <- function(id, State_ID, changed = reactiveVal(0)) {
  moduleServer(id, function(input, output, session) {
    cart <- reactiveVal(data.frame(
      item_id = integer(),
      name = character(),
      original_price = numeric(),
      stringsAsFactors = FALSE
    ))

    available <- reactive({
      changed()
      data <- read_inventory()

      data
    })

    output$items <- renderDT(
      datatable(available(), selection = "single", rownames = FALSE)
    )

    observeEvent(input$items_rows_selected, {
      row <- available()[input$items_rows_selected, , drop = FALSE]
      if (nrow(row)) {
        cart(rbind(
          cart(),
          data.frame(
            item_id = row$item_id,
            name = row$name,
            original_price = row$price,
            stringsAsFactors = FALSE
          )
        ))
      }
    })

    discounted_cart <- reactive({
      data <- cart()
      if (!nrow(data)) {
        data$price <- numeric()
        return(data)
      }

      discount_rate <- as.numeric(input$discount %||% 0) / 100
      data$price <- round(data$original_price * (1 - discount_rate), 2)
      data[, c("item_id", "name", "original_price", "price"), drop = FALSE]
    })

    output$cart <- renderTable(discounted_cart())

    total <- reactive(sum(discounted_cart()$price))

    output$discounted_total <- renderText({
      paste0(
        "Total after ", input$discount %||% 0,
        "% discount: ", currency(total())
      )
    })

    output$change <- renderText({
      paste("Change:", currency(max(0, input$tendered - total())))
    })

    observeEvent(input$clear, {
      cart(data.frame(
        item_id = integer(),
        name = character(),
        original_price = numeric(),
        stringsAsFactors = FALSE
      ))
      updateSelectInput(session, "discount", selected = 0)
    })

    observeEvent(input$checkout, {
      req(nrow(discounted_cart()) > 0, State_ID())

      seller_name <- trimws(input$seller_name %||% "")
      seller_state_id <- trimws(input$seller_State_ID %||% "")
      if (!nzchar(seller_name)) {
        return(showNotification("Seller name is required.", type = "error"))
      }
      if (!nzchar(seller_state_id)) {
        return(showNotification("Seller State ID is required.", type = "error"))
      }

      employees <- read_employees()
      employee <- employees[
        as.character(employees$State_ID) == as.character(State_ID()) &
          employees$Active == TRUE,
        , drop = FALSE
      ]
      if (!nrow(employee)) {
        return(showNotification("Select an active employee.", type = "error"))
      }
      if (input$tendered < total()) {
        return(showNotification("Amount tendered is insufficient.", type = "error"))
      }

      transactions <- read_transactions()
      tx_id <- next_id(transactions, "transaction_id")
      tx_number <- new_number("SALE-")
      tx <- data.frame(
        transaction_id = tx_id,
        transaction_number = tx_number,
        employee_State_ID = as.integer(State_ID()),
        seller_name = seller_name,
        seller_State_ID = seller_state_id,
        total = total(),
        payment_method = input$payment,
        created_at = as.character(Sys.time()),
        stringsAsFactors = FALSE
      )
      write_csv(rbind(transactions, tx), file_paths$transactions)

      details <- read_csv(file_paths$transaction_items)
      cart_data <- discounted_cart()
      new_details <- data.frame(
        transaction_item_id = next_id(details, "transaction_item_id") + seq_len(nrow(cart_data)) - 1L,
        transaction_id = tx_id,
        item_id = cart_data$item_id,
        unit_price = cart_data$price,
        stringsAsFactors = FALSE
      )
      write_csv(rbind(details, new_details), file_paths$transaction_items)

      showNotification(paste("Sale completed:", tx_number))
      cart(data.frame(
        item_id = integer(),
        name = character(),
        original_price = numeric(),
        stringsAsFactors = FALSE
      ))
      updateTextInput(session, "seller_name", value = "")
      updateTextInput(session, "seller_State_ID", value = "")
      updateSelectInput(session, "discount", selected = 0)
      changed(changed() + 1)
    })
  })
}
