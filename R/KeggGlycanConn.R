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
#' @importFrom R6 R6Class
#' @export
KeggGlycanConn <- R6::R6Class("KeggGlycanConn",
inherit=KeggConn,


public=list(

#' @description
#' New instance initializer. Connector classes must not be instantiated
#' directly. Instead, you must use the createConn() method of the factory class.
#' @param ... All parameters are passed to the super class initializer.
#' @return Nothing.
initialize=function(...) {
    super$initialize(db.name='glycan', db.abbrev='gl', accession.prefix='G',
        ...)
}
),

private=list(

doGetEntryImageUrl=function(id) {

    fct <- function(x) {
        bu <- self$getPropValSlot('urls', 'base.url')
        u <- c(bu, 'Fig', 'glycan', paste(x, 'gif', sep='.'))
        biodb::BiodbUrl$new(url=u)$toString()
    }

    return(vapply(id, fct, FUN.VALUE=''))
}
))
