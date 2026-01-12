note:
- all reactive value must be in params, if you need a reactive value inside a function, pass params

plan:
- For sidebar, add a button to clear all, and individual button to clear each search box
- Implement search logic to allow user to search in the sidebar, if a search box is not visible(changing the tab pane can make some invisible), it does not take effect. However, when user goes back to the same pane, the search box should have it previous value
- For the date/datetime column, allow search by: <x, >x, <=x, >=x, x~y, where the date is greater than or equal x, less than or equal y. For datetime, the logic is the same. you can use lubridate::ymd_hms to parse common parameter
- cache all needed table info as a list in params$table_info (not reactive), for example, table name in sql, column names, and column type, so there is no need to do sql and query them latter in the code. be conservative, do it only for the value you need, if you do not need a value, do not cache it
- for the DT table, safeguard table name and column name availability, so if a table or a column is not available, the DT will not show them


After implement, the app can be run by. However, you should consider how to test the app.
```r
path <- "C:\\Users\\jiewang\\OneDrive - University of Texas Medical Branch\\grant\\DMAC\\software\\pipeline\\omop\\2026_01_07.duckdb"
con <- duckdbCon(path)
runExplorer(con, port = 1234)
```

One possible way is to use `testServer` to test your implementation, e.g.
```r
ui <- fluidPage(
  numericInput("x", "x", 0),
  numericInput("y", "y", 1),
  numericInput("z", "z", 2),
  textOutput("out")
)
server <- function(input, output, session) {
  xy <- reactive(input$x - input$y)
  yz <- reactive(input$z + input$y)
  xyz <- reactive(xy() * yz())
  output$out <- renderText(paste0("Result: ", xyz()))
}
testServer(server, {
  session$setInputs(x = 1, y = 1, z = 1)
  print(xy())
  print(output$out)
})
#> [1] 0
#> [1] "Result: 0"
```