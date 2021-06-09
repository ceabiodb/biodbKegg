
#' The connector class to KEGG Pathway database.
#'
#' This is a concrete connector class. It must never be instantiated directly,
#' but instead be instantiated through the factory \code{BiodbFactory}.
#' Only specific methods are described here. See super classes for the
#' description of inherited methods.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('kegg.module')
#'
#' # Get an entry
#' e <- conn$getEntry('M00009')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @seealso \code{\link{KeggConn}}.
#'
#' @include KeggConn.R
#' @export KeggModuleConn
#' @exportClass KeggModuleConn
KeggModuleConn <- methods::setRefClass("KeggModuleConn",
    contains=c("KeggConn"),

methods=list(

initialize=function(...) {
    callSuper(db.name='module', db.abbrev='md', accession.prefix='M', ...)
}

))
