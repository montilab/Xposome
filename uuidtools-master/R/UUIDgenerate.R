#' @useDynLib uuidtools R_uuid_generate

#' @export
#' @title Generate a set of UUIDs
#' @description
#' This function generates one or more new Universally Unique Identifiers
#' (UUIDs), which can be either time-based (version 1) or random (version 4).
#' @param use.time
#' A logical value:
#' \describe{
#'   \item{\code{TRUE}}{
#'     time-based (version 1) UUIDs are generated
#'   }
#'   \item{\code{FALSE}}{
#'     random (version 4) UUIDs are generated
#'   }
#'   \item{\code{NA} (default)}{
#'     random (version 4) UUIDs are generated, unless a reliable source
#'     of random numbers cannot be found, in which case time-based (version 1)
#'     UUIDs are generated
#'   }
#' }
#' @param n
#' A numeric value specifying the number of UUIDs to generate (default is 1)
#' @param as.character
#' A logical value specifying whether the result should be returned as a
#' character vector (default) or as a \code{\linkS4class{UUIDList}}
#' @param upper.case
#' A logical value specifying whether (if \code{upper.case} is \code{TRUE})
#' the result should be returned in upper case (\code{TRUE}), lower case
#' (\code{FALSE}), or the libuuid default (\code{NA})
#' @return
#' A set of UUIDs, returned as a character vector (with case specified by
#' \code{upper.case}) or as a \code{UUIDList} (depending on the value of
#' parameter \code{as.character}).
#' @author Adam C. Gower \email{agower@@bu.edu},
#' based on function by Simon Urbanek and libuuid by Theodore Ts'o.
#' @examples
#' # Generate a single, random (version 4) UUID as a character string
#' UUIDgenerate()
#' UUIDgenerate(use.time=FALSE)
#' # Generate a single, time-based (version 1) UUID as a character string
#' UUIDgenerate(use.time=TRUE)
#' # Generate a character vector containing 1000 time-based UUIDs
#' UUIDgenerate(use.time=TRUE, n=1000)
#' # Generate a character vector containing 1000 random UUIDs in upper case
#' UUIDgenerate(use.time=TRUE, n=1000, upper.case=TRUE)
#'
#' # Generate a UUIDList containing a single random UUID
#' UUIDgenerate(as.character=FALSE)
#' # Generate a UUIDList containing 1000 random UUIDs
#' UUIDgenerate(n=1000, as.character=FALSE)

UUIDgenerate <- function (
  use.time = NA, n = 1, as.character = TRUE,
  upper.case = getOption("UUIDuppercase")
)
{
  # Check arguments
  stopifnot(
    is.logical(use.time), length(use.time) == 1,
    is.numeric(n), length(n) == 1,
    is.logical(as.character), length(as.character) == 1,
    is.logical(upper.case), length(upper.case) == 1
  )
  # Generate UUID(s) as specified
  .Call(
    "R_uuid_generate",
    use.time, as.integer(n), as.character, upper.case
  )
}
