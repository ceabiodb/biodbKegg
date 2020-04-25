
#' The connector class to KEGG Reaction database.
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
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('kegg.reaction')
#'
#' # Get an entry
#' e <- conn$getEntry('R00105')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include KeggConn.R
#' @export KeggReactionConn
#' @exportClass KeggReactionConn
KeggReactionConn <- methods::setRefClass("KeggReactionConn",
    contains="KeggConn",

methods=list(

initialize=function(...) {
    callSuper(db.name='reaction', db.abbrev='rn', ...)
}

))
