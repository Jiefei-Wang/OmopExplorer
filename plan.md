note:
- all reactive value must be in params, if you need a reactive value inside a function, pass params
- If you need to update package NAMESPACE, edit zzz.r and run `devtools::document()`
- for unit test, use `devtools::test(filter="")` to run a particular test file, filter is the file name after stripping test- and .R
- for unit test, use `con <- mockCon()` to create a mock database connection
plan:
-  `filter_data` in shiny-DT-search.r and `concept_id_to_concept_name` in server-DT-concept_map.r are new implementation, and might not be compatible with old code
- Help me to add a function `search_deparser(con, table_name, search_columns, search_value)` to turn sidebar search input into `params_search` parameter for `filter_data` function.
  - `column_types`: a named list of column types, e.g. list("person_id"="integer", "birth_datetime"="date")
  - `search_columns`: a character vector of column names to search, e.g. c("person_id", "birth_datetime"). Can also be a sidebar custom name (e.g. "search_anything")
  - `search_value`: a character vector of search values, e.g. "123", "2020", "<2020", "2020~2021"
  - If a search value does not match column type(e.g. "abc" for integer column), ignore that search value
  - For character, always use ILIKE
  - For search on the concept id column, also do the same search on the concept name column 
- For the first step, write unittest in "tests/testthat/test-server-DT-search.r" to cover different use cases of `search_deparser` function
- Then, implement the function in "R/server-DT-search.r" and make sure the result of `search_deparser` is as expected. 

