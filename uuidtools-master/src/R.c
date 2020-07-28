#include <stdlib.h>          // malloc()
#include <R_ext/Rdynload.h>  // for registering routines
#include <Rdefines.h>        // R macros and functions
#include "uuid.h"            // libuuid

void uuid_unparse_wrapper (const uuid_t uu, char *out, int uppercase)
{
  if (uppercase == TRUE) {
    uuid_unparse_upper(uu, out);
  } else if (uppercase == FALSE) {
    uuid_unparse_lower(uu, out);
  } else if (uppercase == NA_INTEGER) {
    uuid_unparse(uu, out);
  }
}

SEXP R_uuid_compare (SEXP R_x, SEXP R_y)
{
  /*
  Perform bitwise comparison of two UUIDs using libuuid
  INPUT
    R_x    A UUID object
    R_y    A UUID object
  OUTPUT
    An integer vector of length 1, containing one of the following values:
      0: R_x = R_y
      1: R_x > R_y
     -1: R_x < R_y
  */

  // Check arguments
  if (
    strcmp(CHAR(asChar(GET_CLASS(R_x))), "UUID") ||
    strcmp(CHAR(asChar(GET_CLASS(R_y))), "UUID")
  )
  {
    error("Both arguments must be objects of class 'UUID'");
  }

  uuid_t *ux = (uuid_t*) RAW(GET_SLOT(R_x, Rf_install("data")));
  uuid_t *uy = (uuid_t*) RAW(GET_SLOT(R_y, Rf_install("data")));
  return ScalarInteger(uuid_compare(*ux, *uy));
}

SEXP R_uuid_generate (
  SEXP R_useTime, SEXP R_n, SEXP R_unparse, SEXP R_uppercase
)
{
  /*
  Generate a set of UUIDs using libuuid
  INPUT
    R_useTime    A logical vector of length 1
    R_n          An integer vector of length 1
    R_unparse    A logical vector of length 1
    R_uppercase  A logical vector of length 1
  OUTPUT
    A set of UUIDs of length R_n, generated as follows:
    - If R_useTime is TRUE:
      uuid_generate_time() is used to generate a time-based (version 1) UUID
    - If R_useTime is FALSE:
      uuid_generate_random() is used to generate a random (version 4) UUID
    - If R_useTime is NA:
      uuid_generate() is used to generate a UUID.
      - If a sufficiently random number source is available,
        uuid_generate_random() is used to generate a random (version 4) UUID
      - Otherwise, uuid_generate_time() is used to generate
        a time-based (version 1) UUID
    - If R_unparse is TRUE or NA, the result is returned as a character vector,
      in uppercase (if R_uppercase is TRUE), lowercase (if R_uppercase is FALSE)
      or the default (if R_uppercase is NA).
    - If R_unparse is FALSE, the result is returned as a UUIDList object.
  */

  // Check arguments
  if (!IS_LOGICAL(R_useTime) || LENGTH(R_useTime) != 1) {
    error("Argument must be a logical vector of length 1");
  }
  if (!IS_INTEGER(R_n) || LENGTH(R_n) != 1) {
    error("Argument must be an integer vector of length 1");
  }
  if (!IS_LOGICAL(R_unparse) || LENGTH(R_unparse) != 1) {
    error("Argument must be a logical vector of length 1");
  }
  if (!IS_LOGICAL(R_uppercase) || LENGTH(R_uppercase) != 1) {
    error("Argument must be a logical vector of length 1");
  }

  char *buf;
  int i;
  uuid_t u;

  // UUIDList object
  SEXP R_result;
  // Components of listData slot of result
  SEXP R_listData, R_uuid;
  // Symbol object corresponding to "data", for convenience/speed
  SEXP R_DataSymbol = Rf_install("data");

  int use_time = asInteger(R_useTime);
  int n = asInteger(R_n);
  int unparse = asInteger(R_unparse);
  int uppercase = asInteger(R_uppercase);

  if (unparse) {
    PROTECT(R_result = NEW_CHARACTER(n));
  } else {
    PROTECT(R_result = NEW_OBJECT(MAKE_CLASS("UUIDList")));
    PROTECT(R_listData = NEW_LIST(n));
  }
  for (i = 0; i < n; i++) {
    // Generate the UUID
    if (use_time == TRUE) {
      uuid_generate_time(u);
    } else if (use_time == FALSE) {
      uuid_generate_random(u);
    } else if (use_time == NA_INTEGER) {
      uuid_generate(u);
    }
    if (unparse) {
      // "Unparse" the UUID to a new character string and place in result
      buf = (char*) malloc(37);
      uuid_unparse_wrapper(u, buf, uppercase);
      SET_STRING_ELT(R_result, i, mkChar(buf));
    } else {
      // Initialize a UUID S4 object, copy contents of UUID, and place in result
      PROTECT(R_uuid = NEW_OBJECT(MAKE_CLASS("UUID")));
      uuid_copy(*(uuid_t*) RAW(GET_SLOT(R_uuid, R_DataSymbol)), u);
      SET_VECTOR_ELT(R_listData, i, R_uuid);
      UNPROTECT(1);
    }
  }
  if (unparse == FALSE) {
    // Set the listData slot of the UUIDList object and unprotect the list
    SET_SLOT(R_result, Rf_install("listData"), R_listData);
    UNPROTECT(1);
  }
  // Unprotect and return the result
  UNPROTECT(1);
  return R_result;
}

SEXP R_uuid_is_null (SEXP R_x)
{
  /*
  Determine whether a UUID is a nil UUID
  INPUT
    R_x    A UUID object
  OUTPUT
    A logical vector of length 1 indicating whether the UUID is nil.
  */

  // Check arguments
  if (strcmp(CHAR(asChar(GET_CLASS(R_x))), "UUID")) {
    error("Argument must be an object of class 'UUID'");
  }

  uuid_t *u = (uuid_t*) RAW(GET_SLOT(R_x, Rf_install("data")));
  return ScalarLogical(uuid_is_null(*u));
}

SEXP R_uuid_parse (SEXP R_x, SEXP R_validateOnly)
{
  /*
  Parse a UUID from a character vector using libuuid
  INPUT
    R_x             A character vector
    R_validateOnly  A logical vector of length 1
  OUTPUT
    A UUIDList of the same length as R_x.
    The listData slot contains, for each element of R_x:
    - If the element is NA: a nil UUID
    - If the element is not a valid UUID: a nil UUID, with a warning
    - If the element is a valid UUID: the parsed UUID
    The elementMetadata slot contains a single column named 'valid'.
    The 'valid' metadata column contains, for each element of R_x:
    - If the element encodes a valid non-nil UUID: TRUE
    - If the element does not encode a valid UUID: FALSE
    - If the element is NA or encodes a nil UUID: NA
  */

  // Parse/check arguments
  if (!IS_CHARACTER(R_x)) {
    error("Argument 1 must be of class 'character'");
  }
  if (!IS_LOGICAL(R_validateOnly) || LENGTH(R_validateOnly) != 1) {
    error("Argument 2 must be a logical vector of length 1");
  }
  int validate_only = asInteger(R_validateOnly);

  int i, parse_exitcode;
  uuid_t u;
  // UUIDList object
  SEXP R_result;
  // Components of listData slot of result
  SEXP R_listData, R_uuid;
  // Components of elementMetadata slot of result
  SEXP R_elementMetadata, R_elementMetadata_listData, R_valid;
  // Symbol object corresponding to "data", for convenience/speed
  SEXP R_DataSymbol = Rf_install("data");

  if (!validate_only) PROTECT(R_listData = NEW_LIST(LENGTH(R_x)));
  PROTECT(R_valid = NEW_LOGICAL(LENGTH(R_x)));
  for (i = 0; i < LENGTH(R_x); i++) {
    if (STRING_ELT(R_x, i) == NA_STRING) {
      // If the element of the character vector is NA_character_,
      // a nil UUID will be returned (UUID equivalent of a missing value)
      // and the validity will be set to NA.
      LOGICAL(R_valid)[i] = NA_LOGICAL;
    } else {
      // Otherwise, an attempt is made to parse the UUID,
      // and the validity is set accordingly
      parse_exitcode = uuid_parse(CHAR(STRING_ELT(R_x, i)), u);
      // uuid_parse returns 0 on success, -1 on failure;
      // here, 0 is success, _anything_ else is failure (to future-proof
      // against possible libuuid changes to error return values).
      LOGICAL(R_valid)[i] = (parse_exitcode == 0);
    }
    if (!validate_only) {
      // Initialize a UUID S4 object (creates a nil UUID)
      PROTECT(R_uuid = NEW_OBJECT(MAKE_CLASS("UUID")));
      // If the string is NA or does not encode a valid UUID, throw a warning
      // and leave a nil UUID in R_uuid;
      // otherwise, copy the contents of u into R_uuid
      if (LOGICAL(R_valid)[i] == NA_LOGICAL) {
        warning("Input is NA; returning a nil UUID");
      } else if (LOGICAL(R_valid)[i] == FALSE) {
        warning("Input is not a valid UUID; returning a nil UUID");
      } else {
        uuid_copy(*(uuid_t*) RAW(GET_SLOT(R_uuid, R_DataSymbol)), u);
      }
      // The parsed UUID is placed in the list
      SET_VECTOR_ELT(R_listData, i, R_uuid);
      UNPROTECT(1);
    }
  }
  if (!validate_only) {
    // Create a DataFrame object and set its nrows and listData slots
    PROTECT(R_elementMetadata = NEW_OBJECT(MAKE_CLASS("DataFrame")));
    SET_SLOT(
      R_elementMetadata, Rf_install("nrows"), ScalarInteger(LENGTH(R_x))
    );
    PROTECT(R_elementMetadata_listData = NEW_LIST(1));
    SET_VECTOR_ELT(R_elementMetadata_listData, 0, R_valid);
    SET_NAMES(R_elementMetadata_listData, mkString("valid"));
    SET_SLOT(
      R_elementMetadata, Rf_install("listData"), R_elementMetadata_listData
    );
    // Create a UUIDList object and set the listData and elementMetadata slots
    PROTECT(R_result = NEW_OBJECT(MAKE_CLASS("UUIDList")));
    SET_SLOT(R_result, Rf_install("listData"), R_listData);
    SET_SLOT(R_result, Rf_install("elementMetadata"), R_elementMetadata);
    UNPROTECT(5);
    return R_result;
  } else {
    UNPROTECT(1);
    return R_valid;
  }
}

SEXP R_uuid_time (SEXP R_x)
{
  /*
  Determine the timestamp of a UUID using libuuid
  INPUT
    R_x    A UUID object
  OUTPUT
    An integer vector of length 1 containing the time of the UUID.
  */

  // Check arguments
  if (strcmp(CHAR(asChar(GET_CLASS(R_x))), "UUID")) {
    error("Argument must be an object of class 'UUID'");
  }

  struct timeval tv;

  uuid_t *u = (uuid_t*) RAW(GET_SLOT(R_x, Rf_install("data")));
  uuid_time(*u, &tv);
  return ScalarInteger((int) tv.tv_sec);
}

SEXP R_uuid_type (SEXP R_x)
{
  /*
  Determine the version of a UUID using libuuid
  INPUT
  R_x    A UUID object
  OUTPUT
  An integer vector of length 1 containing the version of the UUID.
  */

  // Check arguments
  if (strcmp(CHAR(asChar(GET_CLASS(R_x))), "UUID")) {
    error("Argument must be an object of class 'UUID'");
  }

  uuid_t *u = (uuid_t*) RAW(GET_SLOT(R_x, Rf_install("data")));
  return ScalarInteger(uuid_type(*u));
}

SEXP R_uuid_unparse (SEXP R_x, SEXP R_uppercase)
{
  /*
  "Unparse" a UUID to its character representation
  INPUT
    R_x            A UUID object
    R_uppercase    A logical vector of length 1
  OUTPUT
    A character vector of length 1 containing the character representation of
    the UUID.
  */

  // Check arguments
  if (strcmp(CHAR(asChar(GET_CLASS(R_x))), "UUID")) {
    error("Argument 1 must be an object of class 'UUID'");
  }
  if (!IS_LOGICAL(R_uppercase) || LENGTH(R_uppercase) != 1) {
    error("Argument 2 must be a logical vector of length 1");
  }

  char buf[37];
  uuid_t *u;
  
  u = (uuid_t*) RAW(GET_SLOT(R_x, Rf_install("data")));
  uuid_unparse_wrapper(*u, buf, asInteger(R_uppercase));
  return mkString(buf);
}

SEXP R_uuid_variant (SEXP R_x)
{
  /*
  Determine the variant of a UUID using libuuid
  INPUT
    R_x    A UUID object
  OUTPUT
    An integer vector of length 1 containing the variant of the UUID.
  */

  // Check arguments
  if (strcmp(CHAR(asChar(GET_CLASS(R_x))), "UUID")) {
    error("Argument must be an object of class 'UUID'");
  }

  uuid_t *u = (uuid_t*) RAW(GET_SLOT(R_x, Rf_install("data")));
  return ScalarInteger(uuid_variant(*u));
}

SEXP UUID_validate(SEXP uuids) {
  SEXP out = PROTECT(allocVector(LGLSXP, length(uuids))); // allocate return value (out).
  uuid_t uuid_bin;

  for(int i = 0; i < length(uuids); i++) {
    if(STRING_ELT(uuids, i) == NA_STRING) {
      LOGICAL(out)[i] = NA_LOGICAL;
      continue;
    }

    // uuid_parse returns 0 on success, -1 on failure.
    // adding 1 will coerce to C bool, but the below is more defensive.
    // 0 is success, _anything_ else is failure (to future-proof against possible libuuid changes to error return values).
    int x = uuid_parse(CHAR(STRING_ELT(uuids, i)), uuid_bin);
    LOGICAL(out)[i] = x == 0 ? 1 : 0;
  } // closes for-loop
  
  UNPROTECT(1);
  return out;
}

static const R_CallMethodDef callMethods[] = {
  {"R_uuid_compare",  (DL_FUNC) &R_uuid_compare,  2},
  {"R_uuid_generate", (DL_FUNC) &R_uuid_generate, 4},
  {"R_uuid_is_null",  (DL_FUNC) &R_uuid_is_null,  1},
  {"R_uuid_parse",    (DL_FUNC) &R_uuid_parse,    2},
  {"R_uuid_time",     (DL_FUNC) &R_uuid_time,     1},
  {"R_uuid_type",     (DL_FUNC) &R_uuid_type,     1},
  {"R_uuid_unparse",  (DL_FUNC) &R_uuid_unparse,  2},
  {"R_uuid_variant",  (DL_FUNC) &R_uuid_variant,  1},
  {"UUID_validate",   (DL_FUNC) &UUID_validate,   1},
  {NULL, NULL, 0}
};

void R_init_uuidtools (DllInfo *dll)
{
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
