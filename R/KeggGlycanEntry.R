#' KEGG Glycan entry class.
#'
#' This is the entry class for the KEGG Glycan database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('kegg.glycan')
#'
#' # Get an entry
#' e <- conn$getEntry('G00018')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include KeggEntry.R
#' @export
KeggGlycanEntry <- R6::R6Class("KeggGlycanEntry",
inherit=KeggEntry,


public=list(
),

private=list(
doParseFieldsStep2=function(parsed.content) {

    # Name
    private$parseNames(parsed.content)

    # Other KEGG IDs
    private$parseMultilinesField(field='kegg.reaction.id', tag='REACTION',
                                parsed.content=parsed.content)
    private$parseMultilinesField(field='kegg.enzyme.id',   tag='ENZYME',
                                parsed.content=parsed.content)
    private$parsePathwayIds(parsed.content=parsed.content)
    private$parseModuleIds(parsed.content)
    private$parseOrthologyIds(parsed.content=parsed.content)
}
))
