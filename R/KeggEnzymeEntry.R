
#' KEGG Enzyme entry class.
#'
#' This is the entry class for the KEGG Enzyme class.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('kegg.enzyme')
#'
#' # Get an entry
#' e <- conn$getEntry('1.1.1.54')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import R6
#' @include KeggEntry.R
#' @export
KeggEnzymeEntry <- R6::R6Class("KeggEnzymeEntry",
inherit=KeggEntry,

public=list(
),

private=list(
doParseFieldsStep2=function(parsed.content) {

    # Name
    private$parseNames(parsed.content)

    # Parse DB links
    private$parseDbLinks(parsed.content)

    # Other KEGG IDs
    private$parsePathwayIds(parsed.content=parsed.content)

    # Genes
    private$parseGenesIds(parsed.content)

    # Reactions (they can be set in ALL_REAC instead of DBLINKS
    private$parseReactionIds(parsed.content)
}
))
