# No-Pixel-Stuff

No Pixel GTA Server Workplace.

## Hong Kong Pawn

A multi-file R Shiny demo application for **Hong Kong Pawn**, a fictional pawn-shop point-of-sale system.

Features:

- Point of sale with simulated checkout
- CSV-backed inventory, employees, sellers, and transactions
- Inventory management
- Employee management
- Seller name and State ID captured for every transaction
- Static GitHub Pages documentation in `docs/`

### CSV file formats

`data/inventory.csv` contains only:

```text
item_id,name,price
```

`data/employees.csv` contains only:

```text
State_ID,Name,Role,Active,Hire_Date
```

`data/transactions.csv` records the employee processing the transaction and the person selling to the store:

```text
transaction_id,transaction_number,employee_State_ID,seller_name,seller_State_ID,total,payment_method,created_at
```

### Run locally

Install the required packages:

```r
install.packages(c("shiny", "DT", "bslib"))
shiny::runApp()
```

The application uses CSV files in `data/` rather than a database. Local development can write changes to those files. A deployed Shiny host must provide persistent writable storage if changes should survive restarts; writing from the app will not commit changes back to GitHub automatically.

This is a fictional demo. Do not enter real payment information, passwords, or sensitive personal data.

GitHub Pages hosts the documentation in `docs/`; the Shiny runtime must be deployed to a Shiny-compatible host such as shinyapps.io or Posit Connect.
