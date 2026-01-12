note:
- all reactive value must be in params, if you need a reactive value inside a function, pass params
- If you need to update package NAMESPACE, edit zzz.r and run `devtools::document()`

plan:
- create unit test for server functions, one for each file in R/server_*.r
- use mock database connection for unit tests
```
con <- mockCon()
```
- in unit tests, use shiny::testServer to test server functions
- run test using `devtools::test(filter = "file_name")` while file_name is the name after stripping "test-" and ".r"
- start with tests/testthat/test-server-DT.r
