note:
- all reactive value must be in params, if you need a reactive value inside a function, pass params
- If you need to update package NAMESPACE, edit zzz.r and run `devtools::document()`
- for unit test, use `devtools::test(filter="")` to run a particular test file, filter is the file name after stripping test- and .R

plan:
- Use roxygen2 for documenting functions. 
- Exported functions: runExplorer, duckdbCon, mockCon
- For internal functions, still document with roxygen2, detail the input type and output type
- For external functions, give examples in the documentation. you can use mockCon() to create a mock database connection for examples.
- use mock database connection for unit tests
```
con <- mockCon()
```
- Once finish, use `devtools::document()` to update documentation
![1768255924088](image/plan/1768255924088.png)![1768255929787](image/plan/1768255929787.png)