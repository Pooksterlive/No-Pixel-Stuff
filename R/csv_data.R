data_dir <- "data"
file_paths <- list(
  inventory = file.path(data_dir, "inventory.csv"),
  employees = file.path(data_dir, "employees.csv"),
  transactions = file.path(data_dir, "transactions.csv"),
  transaction_items = file.path(data_dir, "transaction_items.csv")
)

initialize_csv_files <- function() {
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
  if (!file.exists(file_paths$inventory)) write.csv(data.frame(item_id = integer(), sku = character(), item_name = character(), category = character(), condition = character(), brand = character(), cost_basis = numeric(), asking_price = numeric(), status = character(), location = character(), created_at = character(), updated_at = character()), file_paths$inventory, row.names = FALSE)
  if (!file.exists(file_paths$employees)) write.csv(data.frame(employee_id = integer(), employee_number = character(), first_name = character(), last_name = character(), role = character(), email = character(), active = logical(), created_at = character()), file_paths$employees, row.names = FALSE)
  if (!file.exists(file_paths$transactions)) write.csv(data.frame(transaction_id = integer(), transaction_number = character(), employee_id = integer(), total = numeric(), payment_method = character(), created_at = character()), file_paths$transactions, row.names = FALSE)
  if (!file.exists(file_paths$transaction_items)) write.csv(data.frame(transaction_item_id = integer(), transaction_id = integer(), item_id = integer(), unit_price = numeric()), file_paths$transaction_items, row.names = FALSE)
  if (nrow(read_csv(file_paths$employees)) == 0) append_csv(file_paths$employees, data.frame(employee_id = 1L, employee_number = "EMP-001", first_name = "Demo", last_name = "Manager", role = "Manager", email = "demo@hongkongpawn.example", active = TRUE, created_at = as.character(Sys.time())))
  if (nrow(read_csv(file_paths$inventory)) == 0) append_csv(file_paths$inventory, data.frame(item_id = 1:3, sku = c("HKP-001", "HKP-002", "HKP-003"), item_name = c("Vintage Watch", "Game Console", "Gold Bracelet"), category = c("Jewelry", "Electronics", "Jewelry"), condition = c("Good", "Very Good", "Good"), brand = c("Seiko", "Nintendo", "Unbranded"), cost_basis = c(80, 120, 250), asking_price = c(180, 275, 500), status = "Available", location = c("Case A", "Shelf 2", "Case B"), created_at = as.character(Sys.time()), updated_at = as.character(Sys.time())))
}

read_csv <- function(path) {
  if (!file.exists(path)) return(data.frame())
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}
write_csv <- function(data, path) write.csv(data, path, row.names = FALSE)
append_csv <- function(path, row) write_csv(rbind(read_csv(path), row), path)
read_inventory <- function() read_csv(file_paths$inventory)
read_employees <- function() read_csv(file_paths$employees)
read_transactions <- function() read_csv(file_paths$transactions)
next_id <- function(data, column) if (!nrow(data)) 1L else max(as.integer(data[[column]]), na.rm = TRUE) + 1L
