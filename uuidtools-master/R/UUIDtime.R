#' @useDynLib uuidtools R_uuid_time

#' @export
#' @title Get the time of a UUID
#' @description
#' This function gets the time at which a \code{UUID} was generated.
#' @param x
#' A \code{\linkS4class{UUID}} object
#' @param tz
#' A time zone specification to be used for converting time;
#' defaults to \code{""}, i.e., the current time zone
#' @return
#' A \code{\link{POSIXct}} object specifying the time at which the UUID was
#' created, in seconds since January 1, 1970 GMT (the epoch).
#' @references
#' https://www.ietf.org/rfc/rfc4122.txt
#' @author Adam C. Gower \email{agower@@bu.edu},
#' based on libuuid by Theodore Ts'o.

UUIDtime <- function (x, tz="")
{
  if (!is(x, "UUID") || UUIDversion(x) != 1) {
    stop("Argument 'x' must contain a version 1 (time-based) 'UUID' object")
  }
  as.POSIXct(.Call("R_uuid_time", x), tz=tz, origin="1970-01-01")
}
