validate_item <- function(input) {
  if (!nzchar(trimws(input$item_name))) return("Item name is required.")
  if (!nzchar(trimws(input$category))) return("Category is required.")
  if (!is.numeric(input$cost_basis) || input$cost_basis < 0) return("Cost basis must be zero or greater.")
  if (!is.numeric(input$asking_price) || input$asking_price < 0) return("Asking price must be zero or greater.")
  NULL
}
validate_employee <- function(input) {
  if (!nzchar(trimws(input$first_name)) || !nzchar(trimws(input$last_name))) return("First and last name are required.")
  if (!nzchar(trimws(input$role))) return("A role is required.")
  NULL
}
