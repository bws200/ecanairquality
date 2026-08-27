ecan_stop_for_status <- function(response, context) {
  if (httr::http_error(response)) {
    stop(
      sprintf(
        "%s failed with HTTP status %s.",
        context,
        httr::status_code(response)
      ),
      call. = FALSE
    )
  }
}
