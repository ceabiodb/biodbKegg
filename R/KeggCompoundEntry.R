#' KEGG Compound entry class.
#'
#' This is the entry class for the KEGG Compound database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('kegg.compound')
#'
#' # Get an entry
#' e <- conn$getEntry('C00133')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include KeggEntry.R
#' @export KeggCompoundEntry
#' @exportClass KeggCompoundEntry
KeggCompoundEntry <- methods::setRefClass("KeggCompoundEntry",
    contains='KeggEntry',

methods=list(

initialize=function(...) {

    callSuper(...)
},

.parseFieldsStep2=function(parsed.content) {

    # Name
    .self$.parseNames(parsed.content)

    # Parse DB links
    .self$.parseDbLinks(parsed.content)

    # Other KEGG IDs
    .self$.parseMultilinesField(field='kegg.reaction.id', tag='REACTION',
                                parsed.content=parsed.content)
    .self$.parseMultilinesField(field='kegg.enzyme.id',   tag='ENZYME',
                                parsed.content=parsed.content)
    .self$.parsePathwayIds(parsed.content=parsed.content)
}

))
