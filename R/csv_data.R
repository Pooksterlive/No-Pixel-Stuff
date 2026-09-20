data_dir <- "data"
file_paths <- list(
  inventory = file.path(data_dir, "inventory.csv"),
  employees = file.path(data_dir, "employees.csv"),
  transactions = file.path(data_dir, "transactions.csv"),
  transaction_items = file.path(data_dir, "transaction_items.csv")
)

empty_inventory <- function() data.frame(item_id = integer(), name = character(), price = numeric(), stringsAsFactors = FALSE)
empty_employees <- function() data.frame(State_ID = integer(), Name = character(), Role = character(), Active = logical(), Hire_Date = character(), stringsAsFactors = FALSE)
empty_transactions <- function() data.frame(transaction_id = integer(), transaction_number = character(), employee_State_ID = integer(), seller_name = character(), seller_State_ID = character(), total = numeric(), payment_method = character(), created_at = character(), stringsAsFactors = FALSE)

initialize_csv_files <- function() {
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
  if (!file.exists(file_paths$inventory)) write_csv(empty_inventory(), file_paths$inventory)
  if (!file.exists(file_paths$employees)) write_csv(empty_employees(), file_paths$employees)
  if (!file.exists(file_paths$transactions)) write_csv(empty_transactions(), file_paths$transactions)
  if (!file.exists(file_paths$transaction_items)) write_csv(data.frame(transaction_item_id = integer(), transaction_id = integer(), item_id = integer(), unit_price = numeric(), stringsAsFactors = FALSE), file_paths$transaction_items)
}

read_csv <- function(path) {
  if (!file.exists(path)) return(data.frame())
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}
write_csv <- function(data, path) write.csv(data, path, row.names = FALSE, na = "")
append_csv <- function(path, row) write_csv(rbind(read_csv(path), row), path)
read_inventory <- function() read_csv(file_paths$inventory)
read_employees <- function() read_csv(file_paths$employees)
read_transactions <- function() read_csv(file_paths$transactions)
next_id <- function(data, column) if (!nrow(data)) 1L else max(as.integer(data[[column]]), na.rm = TRUE) + 1L
