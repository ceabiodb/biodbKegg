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
#' @export
KeggCompoundEntry <- R6::R6Class("KeggCompoundEntry",
inherit=KeggEntry,


public=list(

initialize=function(...) {

    super$initialize(...)
}
),

private=list(
parseFieldsStep2=function(parsed.content) {

    # Name
    private$parseNames(parsed.content)

    # Parse DB links
    private$parseDbLinks(parsed.content)

    # Other KEGG IDs
    private$parseMultilinesField(field='kegg.reaction.id', tag='REACTION',
                                parsed.content=parsed.content)
    private$parseMultilinesField(field='kegg.enzyme.id',   tag='ENZYME',
                                parsed.content=parsed.content)
    private$parsePathwayIds(parsed.content=parsed.content)
}
))
