mod_pos_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Point of Sale"),
    fluidRow(
      column(
        width = 7,
        div(
          class = "pos-card",
          h3("Seller information"),
          fluidRow(
            column(6, textInput(ns("seller_name"), "Seller name", placeholder = "Full name")),
            column(6, textInput(ns("seller_State_ID"), "Seller State ID", placeholder = "State identification number"))
          )
        ),
        div(
          class = "pos-card",
          h3("Inventory"),
          textInput(ns("search"), "Find inventory", placeholder = "Search by item name"),
          numericInput(ns("quantity"), "Quantity", value = 1, min = 1, step = 1),
          p("Select an item below to add the selected quantity to the cart."),
          DTOutput(ns("items"))
        )
      ),
      column(
        width = 5,
        div(
          class = "pos-card",
          h3("Cart & payment"),
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
      )
    )
  )
}

mod_pos_server <- function(id, State_ID, changed = reactiveVal(0)) {
  moduleServer(id, function(input, output, session) {
    empty_cart <- function() data.frame(
      item_id = integer(),
      name = character(),
      original_price = numeric(),
      quantity = integer(),
      stringsAsFactors = FALSE
    )

    cart <- reactiveVal(empty_cart())

    available <- reactive({
      changed()
      data <- read_inventory()
      term <- tolower(trimws(input$search %||% ""))
      if (nzchar(term) && nrow(data)) {
        data <- data[grepl(term, tolower(data$name), fixed = TRUE), , drop = FALSE]
      }
      data
    })

    output$items <- renderDT(
      datatable(
        available()[, c("name", "price"), drop = FALSE],
        selection = "single",
        rownames = FALSE
      )
    )

    observeEvent(input$items_rows_selected, {
      row <- available()[input$items_rows_selected, , drop = FALSE]
      quantity <- as.integer(input$quantity %||% 1)
      if (!nrow(row)) return()
      if (is.na(quantity) || quantity < 1) {
        return(showNotification("Quantity must be at least 1.", type = "error"))
      }

      current <- cart()
      matching <- which(current$item_id == row$item_id)
      if (length(matching)) {
        current$quantity[matching[1]] <- current$quantity[matching[1]] + quantity
        cart(current)
      } else {
        cart(rbind(
          current,
          data.frame(
            item_id = row$item_id,
            name = row$name,
            original_price = row$price,
            quantity = quantity,
            stringsAsFactors = FALSE
          )
        ))
      }
    })

    discounted_cart <- reactive({
      data <- cart()
      if (!nrow(data)) {
        data$price <- numeric()
        data$line_total <- numeric()
        return(data[, c("name", "quantity", "original_price", "price", "line_total"), drop = FALSE])
      }

      discount_rate <- as.numeric(input$discount %||% 0) / 100
      data$price <- round(data$original_price * (1 - discount_rate), 2)
      data$line_total <- round(data$price * data$quantity, 2)
      data[, c("name", "quantity", "original_price", "price", "line_total"), drop = FALSE]
    })

    output$cart <- renderTable(discounted_cart())

    total <- reactive(sum(discounted_cart()$line_total))

    observeEvent(total(), {
      updateNumericInput(session, "tendered", value = total())
    }, ignoreInit = FALSE)

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
      cart(empty_cart())
      updateSelectInput(session, "discount", selected = 0)
      updateNumericInput(session, "quantity", value = 1)
    })

    observeEvent(input$checkout, {
      req(nrow(discounted_cart()) > 0, State_ID())

      seller_name <- trimws(input$seller_name %||% "")
      seller_state_id <- trimws(input$seller_State_ID %||% "")
      if (!nzchar(seller_name)) {
        return(showNotification("Seller name is required.", type = "error"))
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
      if (!nrow(details) && !length(names(details))) {
        details <- data.frame(
          transaction_item_id = integer(),
          transaction_id = integer(),
          item_id = integer(),
          quantity = integer(),
          unit_price = numeric(),
          stringsAsFactors = FALSE
        )
      } else if (!"quantity" %in% names(details)) {
        details$quantity <- integer(nrow(details))
        if (nrow(details)) details$quantity[] <- 1L
      }

      cart_data <- cart()
      discount_rate <- as.numeric(input$discount %||% 0) / 100
      cart_data$unit_price <- round(cart_data$original_price * (1 - discount_rate), 2)
      new_details <- data.frame(
        transaction_item_id = next_id(details, "transaction_item_id") + seq_len(nrow(cart_data)) - 1L,
        transaction_id = tx_id,
        item_id = cart_data$item_id,
        quantity = cart_data$quantity,
        unit_price = cart_data$unit_price,
        stringsAsFactors = FALSE
      )
      details <- details[, c("transaction_item_id", "transaction_id", "item_id", "quantity", "unit_price"), drop = FALSE]
      write_csv(rbind(details, new_details), file_paths$transaction_items)

      showNotification(paste("Sale completed:", tx_number))
      cart(empty_cart())
      updateTextInput(session, "seller_name", value = "")
      updateTextInput(session, "seller_State_ID", value = "")
      updateSelectInput(session, "discount", selected = 0)
      updateNumericInput(session, "quantity", value = 1)
      changed(changed() + 1)
    })
  })
}
