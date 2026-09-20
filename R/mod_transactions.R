mod_transactions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Transaction History"),
    p("Select a transaction to view its details."),
    DTOutput(ns("transactions_table")),
    uiOutput(ns("transaction_details"))
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
        tags$hr(),
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
