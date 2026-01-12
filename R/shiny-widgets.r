make_search_box <- function(inputId, label, value = "", placeholder="") {
    searchInput(
        inputId = inputId,
        label = label,
        value = value,
        placeholder = placeholder,
        btnReset = icon("remove"),
        btnClass = "btn-sm",
        width = "100%"
      )
}