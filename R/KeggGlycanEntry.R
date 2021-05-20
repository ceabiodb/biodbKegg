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
#' @export KeggGlycanEntry
#' @exportClass KeggGlycanEntry
KeggGlycanEntry <- methods::setRefClass("KeggGlycanEntry",
    contains='KeggEntry',

methods=list(

initialize=function(...) {

    callSuper(...)
},

.parseFieldsStep2=function(parsed.content) {

    # Name
    .self$.parseNames(parsed.content)

    # Other KEGG IDs
    .self$.parseMultilinesField(field='kegg.reaction.id', tag='REACTION',
                                parsed.content=parsed.content)
    .self$.parseMultilinesField(field='kegg.enzyme.id',   tag='ENZYME',
                                parsed.content=parsed.content)
    .self$.parsePathwayIds(parsed.content=parsed.content)
    .self$.parseModuleIds(parsed.content)
    .self$.parseOrthologyIds(parsed.content=parsed.content)
}

))
