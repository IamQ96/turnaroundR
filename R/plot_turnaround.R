#' Plot Turnaround Times
#'
#' @param result_obj An object of class 'turnaround_result'
#'
#' @return A scatter plot
#' @export
plot_turnaround <- function(result_obj) {
  stopifnot(inherits(result_obj, "turnaround_result"))
  plot(
    result_obj$turnaround,
    main = "Turnaround Duration per Flight",
    xlab = "Flight Index",
    ylab = "Turnaround Time (min)",
    col = "blue",
    pch = 19
  )
}
