#' Calculate Aircraft Turnaround Efficiency
#'
#' @param in_time POSIXct vector. Actual in-block timestamps.
#' @param out_time POSIXct vector. Actual off-block timestamps.
#' @param scheduled_turnaround Numeric. Default = 45. Planned turnaround time in minutes.
#'
#' @return An object of class 'turnaround_result'
#' @export
calculate_turnaround <- function(in_time, out_time, scheduled_turnaround = 45) {
  stopifnot(inherits(in_time, "POSIXct"), inherits(out_time, "POSIXct"))
  stopifnot(length(in_time) == length(out_time))
  
  turnaround <- as.numeric(difftime(out_time, in_time, units = "mins"))
  delay_flag <- ifelse(turnaround <= scheduled_turnaround, "On-Time", 
                       ifelse(turnaround <= scheduled_turnaround + 15, "Minor Delay", "Major Delay"))
  
  result <- list(
    in_time = in_time,
    out_time = out_time,
    turnaround = turnaround,
    delay_flag = delay_flag
  )
  attr(result, "scheduled") <- scheduled_turnaround
  class(result) <- "turnaround_result"
  return(result)
}
