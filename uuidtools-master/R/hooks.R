.onLoad <- function (libname, pkgname)
{
    if (!is.element("UUIDuppercase", names(options()))) {
        options(UUIDuppercase = NA);
    }
    invisible()
}
