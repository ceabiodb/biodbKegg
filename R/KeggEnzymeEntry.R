
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
#' @include KeggEntry.R
#' @export KeggEnzymeEntry
#' @exportClass KeggEnzymeEntry
KeggEnzymeEntry <- methods::setRefClass("KeggEnzymeEntry",
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
    .self$.parsePathwayIds(parsed.content=parsed.content)

    # Genes
    .self$.parseGenesIds(parsed.content)

    # Reactions (they can be set in ALL_REAC instead of DBLINKS
    .self$.parseReactionIds(parsed.content)
}

))
