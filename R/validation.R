validate_item <- function(input) {
  if (!nzchar(trimws(input$name))) return("Item name is required.")
  if (!is.numeric(input$price) || input$price < 0) return("Price must be zero or greater.")
  NULL
}

validate_employee <- function(input) {
  if (!nzchar(trimws(input$name))) return("Employee name is required.")
  if (!nzchar(trimws(input$role))) return("A role is required.")
  if (!nzchar(trimws(input$hire_date))) return("Hire date is required.")
  NULL
}
