#' @useDynLib uuidtools
#' R_uuid_compare R_uuid_is_null R_uuid_parse R_uuid_unparse
#' @import methods S4Vectors

# pcompare #####################################################################

#' @rdname pcompare-methods
#' @name pcompare
#' @title Compare UUIDs
#' @description
#' These methods provide the ability to compare \code{\linkS4class{UUID}} and/or
#' \code{\linkS4class{UUIDList}} objects.
#' @param x
#' A \code{\linkS4class{UUID}} or \code{\linkS4class{UUIDList}} object
#' @param y
#' A \code{\linkS4class{UUID}} or \code{\linkS4class{UUIDList}} object
#' @return
#' An integer vector containing the result of the element-by-element comparison:
#' \describe{
#'   \item{\code{-1}}{
#'     The \code{UUID} element of \code{x} is less than the corresponding
#'     \code{UUID} element of \code{y}
#'   }
#'   \item{\code{0}}{
#'     The \code{UUID} element of \code{x} is equal to the corresponding
#'     \code{UUID} element of \code{y}
#'   }
#'   \item{\code{1}}{
#'     The \code{UUID} element of \code{x} is greater than the corresponding
#'     \code{UUID} element of \code{y}
#'   }
#'   \item{\code{NA}}{
#'     The \code{UUID} element of \code{x} or \code{y} is nil (this is for
#'     consistency with atomic comparisons with \code{NA} values, which also
#'     return \code{NA})
#'   }
#' }
#' The elements of shorter vectors are recycled as necessary.
#' @author Adam C. Gower \email{agower@@bu.edu},
#' based on libuuid by Theodore Ts'o.

#' @export
#' @aliases pcompare,UUID,UUID-method
setMethod(
  "pcompare", signature(x="UUID", y="UUID"),
  function(x, y)
  {
    if (isNil(x) || isNil(y)) {
      NA_integer_
    } else {
      .Call("R_uuid_compare", x, y)
    }
  }
)

#' @export
#' @rdname pcompare-methods
#' @aliases pcompare,UUIDList,UUID-method
setMethod(
  "pcompare", signature(x="UUIDList", y="UUID"),
  function(x, y) as.integer(unlist(lapply(x, pcompare, y)))
)

#' @export
#' @rdname pcompare-methods
#' @aliases pcompare,UUID,UUIDList-method
setMethod(
  "pcompare", signature(x="UUID", y="UUIDList"),
  function(x, y) as.integer(unlist(lapply(y, pcompare, x)))
)

#' @export
#' @rdname pcompare-methods
#' @aliases pcompare,UUIDList,UUIDList-method
setMethod(
  "pcompare", signature(x="UUIDList", y="UUIDList"),
  function(x, y) as.integer(mapply(pcompare, x, y))
)

# comparison ###################################################################

#' @rdname comparison-methods
#' @name Comparison
#' @title Relational operators for UUIDs
#' @description
#' These methods for binary operators use \code{\link{pcompare}} to perform
#' tests between pairs of \code{\linkS4class{UUID}} objects. They are identical
#' to the corresponding methods for \code{\linkS4class{Vector}} objects in
#' package \pkg{S4Vectors}.
#' @param e1
#' A \code{\linkS4class{UUID}} object
#' @param e2
#' A \code{\linkS4class{UUID}} object
#' @return
#' A logical vector of length 1.
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
#' @aliases ==,UUID,UUID-method
setMethod(
  "==", signature(e1="UUID", e2="UUID"),
  function (e1, e2) pcompare(e1, e2) == 0L
)

#' @export
#' @rdname comparison-methods
#' @aliases !=,UUID,UUID-method
setMethod(
  "!=", signature(e1="UUID", e2="UUID"),
  function (e1, e2) !(e1 == e2)
)

#' @export
#' @rdname comparison-methods
#' @aliases <=,UUID,UUID-method
setMethod(
  "<=", signature(e1="UUID", e2="UUID"),
  function (e1, e2) pcompare(e1, e2) <= 0L
)

#' @export
#' @rdname comparison-methods
#' @aliases >=,UUID,UUID-method
setMethod(
  ">=", signature(e1="UUID", e2="UUID"),
  function (e1, e2) e2 <= e1
)

#' @export
#' @rdname comparison-methods
#' @aliases <,UUID,UUID-method
setMethod(
  "<", signature(e1="UUID", e2="UUID"),
  function (e1, e2) !(e2 <= e1)
)

#' @export
#' @rdname comparison-methods
#' @aliases >,UUID,UUID-method
setMethod(
  ">", signature(e1="UUID", e2="UUID"),
  function (e1, e2) !(e1 <= e2)
)

# as.character #################################################################

#' @rdname as.character-methods
#' @name as.character
#' @title Coerce UUID(s) to character string(s)
#' @description
#' These methods coerce \code{\linkS4class{UUID}} or
#' \code{\linkS4class{UUIDList}} objects to character vectors.
#' @param x
#' A \code{\linkS4class{UUID}} or \code{\linkS4class{UUIDList}} object
#' @param upper.case
#' A logical value specifying whether the result should be returned in upper
#' case (\code{TRUE}), lower case (\code{FALSE}), or the libuuid default
#' (\code{NA})
#' @return
#' A character vector containing one element for each \code{UUID};
#' nil \code{UUID} values are coerced to \code{NA} values.
#' @details
#' \describe{
#'   \item{\code{UUID}}{
#'     As S4 method is used, rather than an S3 method \code{as.character.UUID},
#'     for consistency with the S4 method for \code{as.character} and class
#'     \code{UUIDList}.
#'   }
#'   \item{\code{UUIDList}}{
#'     Defining an S3 method function \code{as.character.UUIDList} does not work
#'     because the S4 method \code{\link[S4Vectors]{as.character,Vector-method}}
#'     is called instead, as \code{UUIDList} inherits
#'     \code{\linkS4class{Vector}}, and S4 methods are called before S3 methods.
#'     That S4 method passes its input to \code{\link{as.vector}}, which in turn
#'     terminates with an error when passed a \code{UUIDList} object, as an
#'     \code{as.vector} method does not exist for class \code{UUIDList}.
#'     As there is no way to coerce a \code{UUIDList} object to a vector, an S4
#'     method for \code{as.character} is defined instead.
#'   }
#' }
#' These methods are also called by the S4 method
#' \code{\link{coerce,ANY,character-method}}.
#' @author Adam C. Gower \email{agower@@bu.edu},
#' based on libuuid by Theodore Ts'o.

#' @export
#' @aliases as.character,UUID-method
setMethod(
  "as.character", signature(x="UUID"),
  function (x, upper.case=getOption("UUIDuppercase"))
  {
    if (isNil(x)) {
      NA_character_
    } else {
      .Call("R_uuid_unparse", x, upper.case)
    }
  }
)

#' @export
#' @rdname as.character-methods
#' @aliases as.character,UUIDList-method
setMethod(
  "as.character", signature(x="UUIDList"),
  function (x, upper.case=getOption("UUIDuppercase"))
  {
    unlist(lapply(x, as.character, upper.case=upper.case))
  }
)

# as.raw #######################################################################

#' @export
#' @rdname as.raw.UUID
#' @name as.raw
#' @title Coerce UUID to raw vector
#' @description
#' This S3 method coerces a \code{\linkS4class{UUID}} object to a raw vector.
#' @param x
#' A \code{\linkS4class{UUID}} object
#' @return
#' A raw vector of length 16.
#' @author Adam C. Gower \email{agower@@bu.edu}

as.raw.UUID <- function (x) x@data

# coerce #######################################################################

#' @rdname coerce-methods
#' @name as
#' @title Coercion methods for UUIDs
#' @description
#' These methods coerce \code{character} objects to \code{\linkS4class{UUID}} or
#' \code{\linkS4class{UUIDList}} objects, or \code{UUID} objects to
#' \code{UUIDList} objects.
#' @param from
#' A \code{character} or \code{UUID} object
#' @return
#' An R object of the target class.
#' @author Adam C. Gower \email{agower@@bu.edu},
#' based on libuuid by Theodore Ts'o.

#' @export
#' @aliases coerce,UUID,raw-method
setAs(
  from="UUID", to="raw",
  def = function (from) as.raw.UUID(from)
)

#' @export
#' @aliases coerce,UUID,UUIDList-method
setAs(
  from="UUID", to="UUIDList",
  def = function (from) UUIDList(from)
)

# isNil ########################################################################

#' @rdname isNil
#' @name isNil
#' @title Test for nil UUIDs
#' @description
#' These methods test whether a \code{\linkS4class{UUID}} object (or each
#' element of a \code{\linkS4class{UUIDList}} object) is a "nil UUID",
#' i.e., consists solely of zero bits.
#' It is analogous to the \code{\link{is.na}} function for atomic vectors.
#' @param x
#' A \code{\linkS4class{UUID}} or \code{\linkS4class{UUIDList}} object
#' @return
#' A logical vector of the same length as the input; if each \code{UUID} is nil,
#' \code{TRUE}; otherwise, \code{FALSE}.
#' @references
#' https://www.ietf.org/rfc/rfc4122.txt
#' @author Adam C. Gower \email{agower@@bu.edu},
#' based on libuuid by Theodore Ts'o.

#' @export
setGeneric("isNil", function (x) standardGeneric("isNil"))

#' @export
#' @rdname isNil
#' @aliases isNil,UUID-method
setMethod(
  "isNil",
  signature(x="UUID"),
  function (x) .Call("R_uuid_is_null", x)
)

#' @export
#' @rdname isNil
#' @aliases isNil,UUIDList-method
setMethod(
  "isNil",
  signature(x="UUIDList"),
  function (x) unlist(lapply(x, isNil))
)

# match ########################################################################

#' @rdname match-methods
#' @name match
#' @title UUID value matching
#' @description
#' These methods return a vector of the positions of (first) matches of a
#' \code{\linkS4class{UUID}} or \code{\linkS4class{UUIDList}} within a
#' \code{\linkS4class{UUIDList}}.
#' @param x
#' A \code{\linkS4class{UUID}} or \code{\linkS4class{UUIDList}} object
#' @param table
#' A \code{\linkS4class{UUIDList}} object
#' @param nomatch
#' The value to be returned in the case when no match is found.
#' Note that it is coerced to \code{integer}.
#' @param incomparables
#' A \code{\linkS4class{UUID}} or a \code{UUIDList} containing values that
#' should be excluded from comparison, or the logical value \code{FALSE},
#' meaning that all values can be compared.
#' @param \dots
#' Additional arguments, for use in specific methods.
#' @return
#' If there is a match, an integer vector of the same length as \code{x} giving
#' the position in \code{table} of the first match; otherwise, \code{nomatch}.
#' @seealso
#' These methods enable use of the \code{\linkS4class{Vector}} methods for
#' \code{\link{selfmatch}}, \code{\link{unique}},
#' \code{\link{\%in\%}}, \code{\link{findMatches}}, and
#' \code{\link{countMatches}} (see Note in \code{\link{pcompare}}).
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
#' @aliases match,UUID,UUIDList-method
setMethod(
  "match",
  signature(x="UUID", table="UUIDList"),
  function (x, table, nomatch=NA_integer_, incomparables=NULL, ...)
  {
    if (!(is.null(incomparables) || identical(incomparables, FALSE))) {
      if (!inherits(incomparables, c("UUID", "UUIDList"))) {
        stop(
          paste(
            "Argument 'incomparables' must inherit class 'UUID' or 'UUIDList'",
            "or be set to NULL or FALSE"
          )
        )
      } else {
        incomparables <- as.character(incomparables)
      }
    }
    base::match(
      as.character(x), as.character(table),
      nomatch=nomatch, incomparables=incomparables, ...
    )
  }
)

#' @export
#' @rdname match-methods
#' @aliases match,UUIDList,UUIDList-method
setMethod(
  "match",
  signature(x="UUIDList", table="UUIDList"),
  function (x, table, nomatch=NA_integer_, incomparables=NULL, ...)
  {
    if (!(is.null(incomparables) || identical(incomparables, FALSE))) {
      if (!inherits(incomparables, c("UUID", "UUIDList"))) {
        stop(
          paste(
            "Argument 'incomparables' must inherit class 'UUID' or 'UUIDList'",
            "or be set to NULL or FALSE"
          )
        )
      } else {
        incomparables <- as.character(incomparables)
      }
    }
    base::match(
      as.character(x), as.character(table),
      nomatch=nomatch, incomparables=incomparables, ...
    )
  }
)

# duplicated ###################################################################

#' @rdname duplicated-methods
#' @name duplicated
#' @title Determine duplicated UUIDs
#' @description
#' This method provides the ability to identify duplicate elements in a
#' \code{\linkS4class{UUIDList}} object.
#' @param x
#' A \code{\linkS4class{UUIDList}} object
#' @param incomparables
#' A \code{\linkS4class{UUID}} or a \code{UUIDList} containing values that
#' should be excluded from comparison, or the logical value \code{FALSE},
#' meaning that all values can be compared.
#' @param \dots
#' Additional arguments passed to
#' \code{\link[S4Vectors]{duplicated,Vector-method}} and to \code{\link{match}}
#' @return
#' A logical vector of the same length as \code{x} indicating whether each
#' element in \code{x} is duplicated.
#' @details
#' This method is defined only to ensure that the \code{Vector} method for
#' \code{duplicate} is used, rather than the \code{List} method, which
#' supersedes it for \code{UUIDList} objects.
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
#' @aliases duplicated,UUIDList-method
setMethod(
  "duplicated", signature(x="UUIDList"),
  function (x, incomparables=FALSE, ...) {
    selectMethod(duplicated, "Vector")(x, ...)
  }
)

# order ########################################################################

#' @rdname order-methods
#' @name order
#' @title UUIDList ordering permutation
#' @description
#' This method for \code{\linkS4class{UUIDList}} objects returns a permutation
#' that rearranges its first argument into ascending or descending order,
#' breaking ties by further arguments.
#' @param \dots
#' A sequence of \code{\linkS4class{UUIDList}} objects, all of the same length.
#' @param na.last
#' A logical value for controlling the treatment of \code{NA}s.
#' If \code{TRUE}, missing values in the data are put last;
#' if \code{FALSE}, they are put first; if \code{NA}, they are removed.
#' @param decreasing
#' A logical value specifying whether the sort order should be increasing or
#' decreasing.
#' @param method
#' A character string specifying the method to be used: partial matches are
#' allowed.  For details of specific methods, see the help for
#' \code{\link{sort}}.
#' @return
#' An integer vector of the same length as the inputs.
#' @seealso
#' This method enables use of the \code{\linkS4class{Vector}} methods for
#' \code{\link{sort}} and (together with \code{\link{as.character}})
#' \code{\link{table}} (see Note in \code{\link{pcompare}}).
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
#' @aliases order,UUIDList-method
setMethod(
  "order",
  "UUIDList",
  function (
    ..., na.last = TRUE, decreasing = FALSE,
    method = c("shell", "quick", "radix")
  ) 
  {
    args <- lapply(unname(list(...)), as.character)
    do.call(order, c(args, list(na.last = na.last, decreasing = decreasing)))
  }
)

# show #########################################################################

#' @rdname show-methods
#' @name show
#' @title Describe a UUID
#' @description
#' This method shows the contents of a \code{\linkS4class{UUID}}, including its
#' variant and version numbers.
#' @param object
#' A \code{\linkS4class{UUID}} object
#' @author Adam C. Gower \email{agower@@bu.edu}

#' @export
#' @aliases show,UUID-method
setMethod(
  "show",
  signature("UUID"),
  function (object)
  {
    if (isNil(object)) {
      cat("Nil UUID\n")
    } else{
      cat(
        sprintf(
          "UUID variant %d, version %d\n",
          UUIDvariant(object), UUIDversion(object)
        )
      )
      cat(as.character(object), "\n")
    }
    invisible()
  }
)
