ITEM_STATUSES <- c("Available", "On Hold", "Sold", "Pawned", "Archived")
EMPLOYEE_ROLES <- c("Owner", "Manager", "Cashier", "Appraiser", "Inventory Clerk")
PAYMENT_METHODS <- c("Cash", "Bank", "Store credit", "Other")

currency <- function(value) paste0("$", formatC(as.numeric(value), format = "f", digits = 2, big.mark = ","))
new_number <- function(prefix) paste0(prefix, format(Sys.time(), "%Y%m%d%H%M%S"), sample(100:999, 1))
`%||%` <- function(x, y) if (is.null(x)) y else x
