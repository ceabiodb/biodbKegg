
#' KEGG Orthology entry class.
#'
#' This is the class entry for KEGG Orthology database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('kegg.orthology')
#'
#' # Get an entry
#' e <- conn$getEntry('K12668')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include KeggEntry.R
#' @export
KeggOrthologyEntry <- R6::R6Class("KeggOrthologyEntry",
inherit=KeggEntry,


public=list(

initialize=function(...) {
    super$initialize(...)
}
),

private=list(
parseFieldsStep2=function(parsed.content) {

    # Name
    private$parseNames(parsed.content, strip.chars=' ', split.char=',')

    # Parse DB links
    private$parseDbLinks(parsed.content)

    # Pathway
    private$parsePathwayIds(parsed.content)

    # Modules
    private$parseModuleIds(parsed.content)

    # Genes
    private$parseGenesIds(parsed.content)
}
))
