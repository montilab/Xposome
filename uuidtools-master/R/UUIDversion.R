#' @useDynLib uuidtools R_uuid_type

#' @export
#' @title Get the version of a UUID
#' @description
#' This function gets the bits of a \code{\linkS4class{UUID}} object
#' specifying its version (i.e., how it was generated).
#' @param x
#' A \code{\linkS4class{UUID}} object
#' @return
#' An integer value indicating the version of the UUID:
#' \enumerate{
#'   \item{Time-based UUIDs}
#'   \item{DCE security version}
#'   \item{Namespace-based version using MD5 hashing}
#'   \item{Randomly generated}
#'   \item{Namespace-based version using SHA1 hashing}
#' }
#' @references
#' https://www.ietf.org/rfc/rfc4122.txt
#' @author Adam C. Gower \email{agower@@bu.edu},
#' based on libuuid by Theodore Ts'o.

UUIDversion <- function (x)
{
  if (!is(x, "UUID")) {
    stop("Argument 'x' must contain an object of class 'UUID'")
  }
  .Call("R_uuid_type", x)
}
