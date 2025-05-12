#' Summarize Turnaround Delays
#'
#' @param result_obj An object of class 'turnaround_result'
#' @param method Either 'table' or 'plot'
#'
#' @return A summary table or barplot
#' @export
summarize_turnaround <- function(result_obj, method = "table") {
  stopifnot(inherits(result_obj, "turnaround_result"))
  stopifnot(method %in% c("table", "plot"))
  
  if (method == "table") {
    return(table(result_obj$delay_flag))
  } else {
    barplot(
      table(result_obj$delay_flag),
      col = c("green", "orange", "red"),
      main = "Turnaround Delay Distribution",
      ylab = "Number of Flights"
    )
    invisible(TRUE)
  }
}
