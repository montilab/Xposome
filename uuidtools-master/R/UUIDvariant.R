#' @useDynLib uuidtools R_uuid_variant

#' @export
#' @title Get the variant of a UUID
#' @description
#' This function gets the bits of a \code{\linkS4class{UUID}} object
#' specifying its variant (i.e., format).
#' @param x
#' A \code{\linkS4class{UUID}} object
#' @return
#' An integer value indicating the variant of the UUID:
#' \describe{
#'   \item{\code{0}}{Reserved, NCS backward compatibility}
#'   \item{\code{1}}{DCE specification}
#'   \item{\code{2}}{Reserved, Microsoft Corporation backward compatibility}
#'   \item{\code{3}}{Reserved for future definition}
#' }
#' @references
#' https://www.ietf.org/rfc/rfc4122.txt
#' @author Adam C. Gower \email{agower@@bu.edu},
#' based on libuuid by Theodore Ts'o.

UUIDvariant <- function (x)
{
  if (!is(x, "UUID")) {
    stop("Argument 'x' must contain an object of class 'UUID'")
  }
  .Call("R_uuid_variant", x)
}
