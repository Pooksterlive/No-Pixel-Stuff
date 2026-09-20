# R/mod_transactions.R
mod_transactions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Transaction History"),
    DTOutput(ns("transactions_table"))
  )
}

mod_transactions_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    transaction_rows <- reactive({
      transactions <- read_transactions()
      if (!nrow(transactions)) return(data.frame())

      inventory <- read_inventory()
      details <- read_csv(file_paths$transaction_items)

      rows <- lapply(seq_len(nrow(transactions)), function(i) {
        tx <- transactions[i, , drop = FALSE]
        tx_items <- details[details$transaction_id == tx$transaction_id, , drop = FALSE]

        if (!nrow(tx_items)) {
          return(data.frame(
            transaction_id = tx$transaction_id,
            transaction_number = tx$transaction_number,
            created_at = tx$created_at,
            employee_State_ID = tx$employee_State_ID,
            seller_name = tx$seller_name,
            seller_State_ID = tx$seller_State_ID,
            payment_method = tx$payment_method,
            item_name = NA_character_,
            unit_price = NA_real_,
            total = tx$total,
            stringsAsFactors = FALSE
          ))
        }

        item_names <- inventory$name[match(tx_items$item_id, inventory$item_id)]

        tx_rows <- data.frame(
          transaction_id = tx$transaction_id,
          transaction_number = tx$transaction_number,
          created_at = tx$created_at,
          employee_State_ID = tx$employee_State_ID,
          seller_name = tx$seller_name,
          seller_State_ID = tx$seller_State_ID,
          payment_method = tx$payment_method,
          item_name = item_names,
          unit_price = tx_items$unit_price,
          total = tx$total,
          stringsAsFactors = FALSE
        )

        tx_rows
      })

      do.call(rbind, rows)
    })

    output$transactions_table <- renderDT({
      data <- transaction_rows()
      if (!nrow(data)) {
        return(datatable(data.frame(message = "No transactions found."), rownames = FALSE, options = list(dom = "t")))
      }

      datatable(
        data[, c(
          "transaction_number",
          "created_at",
          "employee_State_ID",
          "seller_name",
          "seller_State_ID",
          "payment_method",
          "item_name",
          "unit_price",
          "total"
        )],
        rownames = FALSE,
        options = list(pageLength = 20)
      )
    })
  })
}
