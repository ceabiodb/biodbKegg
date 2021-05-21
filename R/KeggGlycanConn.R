#' The connector class to KEGG Glycan database.
#'
#' This is a concrete connector class. It must never be instantiated directly,
#' but instead be instantiated through the factory \code{BiodbFactory}.
#' Only specific methods are described here. See super classes for the
#' description of inherited methods.
#'
#' @seealso \code{\link{KeggConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Create a connector to KEGG Glycan
#' conn <- mybiodb$getFactory()$createConn('kegg.glycan')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include KeggConn.R
#' @import chk
#' @export KeggGlycanConn
#' @exportClass KeggGlycanConn
KeggGlycanConn <- methods::setRefClass("KeggGlycanConn",
    contains=c("KeggConn"),

methods=list(

initialize=function(...) {
    callSuper(db.name='glycan', db.abbrev='gl', ...)
},

.doSearchForEntries=function(fields=NULL, max.results=0) {

    ids <- NULL

    # Call super class' method to search by name
    if ('name' %in% names(fields))
        ids <- callSuper(fields=fields, max.results=max.results)

    return(ids)
},

getEntryImageUrl=function(id) {
    # Overrides super class' method.

    fct <- function(x) {
        bu <- .self$getPropValSlot('urls', 'base.url')
        u <- c(bu, 'Fig', 'glycan', paste(x, 'gif', sep='.'))
        BiodbUrl$new(url=u)$toString()
    }

    return(vapply(id, fct, FUN.VALUE=''))
}

))

