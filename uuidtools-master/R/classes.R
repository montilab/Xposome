#' @useDynLib uuidtools
#' R_uuid_is_null R_uuid_time R_uuid_type
#' @import S4Vectors

#' @export UUID
#' @rdname UUID-class
#' @title Class to contain a Universally Unique Identifier (UUID)
#' @description
#' This S4 class is a container for a raw vector of length 16,
#' specifying a 128-bit UUID.
#' @slot data
#' A raw vector of length 16.
#' Defaults to a vector of zeroes, i.e., a "nil UUID".
#' @references
#' https://www.ietf.org/rfc/rfc4122.txt
#' @author Adam C. Gower \email{agower@@bu.edu}

setClass(
  "UUID",
  # This prototype is referred to as the 'nil' UUID
  prototype = prototype(data = raw(16)),
  slots = c(data="raw"),
  validity = function (object)
  {
    if (length(object@data) != 16) {
      "A UUID must be 128 bits in length"
    } else {
      TRUE
    }
  },
  sealed = TRUE
);

#' @rdname UUID-class
#' @param data
#' A raw vector of length 16.
UUID <- function (data) {
  if (missing(data)) new("UUID") else new("UUID", data=data)
}

#' @export UUIDList
#' @rdname UUIDList-class
#' @title Class to contain a list of Universally Unique Identifiers (UUIDs)
#' @description
#' This S4 class is a \code{\linkS4class{SimpleList}} of
#' \code{\linkS4class{UUID}} objects.
#' @slot listData
#' A list of \code{\linkS4class{UUID}} objects.  Defaults to an empty list.
#' @slot elementType
#' Set to \code{"UUID"}.
#' @section Extends:
#' Directly extends class \code{\linkS4class{SimpleList}}.
#' @author Adam C. Gower \email{agower@@bu.edu}

setClass(
  "UUIDList",
  prototype = new("SimpleList", elementType="UUID"),
  contains = "SimpleList",
  sealed = TRUE
);

#' @rdname UUIDList-class
#' @param \dots
#' Either a set of arguments of class \code{\linkS4class{UUID}}, or a single
#' list of \code{\linkS4class{UUID}} objects.
UUIDList <- function (...) {
  new("UUIDList", listData=SimpleList(...)@listData)
}
