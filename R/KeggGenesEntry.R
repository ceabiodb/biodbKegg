
#' KEGG Genes entry class.
#'
#' This is the entry class for the KEGG Genes database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('kegg.genes')
#'
#' # Get an entry
#' e <- conn$getEntry('mmu:14635')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @importFrom R6 R6Class
#' @include KeggEntry.R
#' @export
KeggGenesEntry <- R6::R6Class("KeggGenesEntry",
inherit=KeggEntry,

public=list(
),

private=list(
doParseFieldsStep2=function(parsed.content) {

    # Name
    private$parseNames(parsed.content, strip.chars=' ;', split.char=',')

    # Parse DB links
    private$parseDbLinks(parsed.content)

    # Adjust accession with organism code
    if (self$hasField('kegg.organism.code')) {
        org <- self$getFieldValue('kegg.organism.code')
        acc <- self$getFieldValue('accession')
        self$setFieldValue('accession', paste(org, acc, sep=':'))
    }

    # Other KEGG IDs
    private$parseModuleIds(parsed.content)
    private$parsePathwayIds(parsed.content=parsed.content)
    private$parseOrthologyIds(parsed.content=parsed.content)

    # AA SEQ
    lines <- private$getTagLines(tag='AASEQ', parsed.content=parsed.content)
    seq.length <- as.integer(lines[[1]])
    sequence <- paste(lines[2:length(lines)], collapse='')
    if (seq.length != nchar(sequence))
        self$caution('Length of AA sequence (', nchar(sequence),
            ') is different from the stated length (', seq.length,
            '). In entry ', self$getFieldValue('accession'), '.')
    self$setFieldValue('aa.seq', sequence)
    self$setFieldValue('aa.seq.length', seq.length)

    # NT SEQ
    lines <- private$getTagLines(tag='NTSEQ', parsed.content=parsed.content)
    seq.length <- as.integer(lines[[1]])
    sequence <- paste(lines[2:length(lines)], collapse='')
    if (seq.length != nchar(sequence))
        self$caution('Length of NT sequence (', nchar(sequence),
            ') is different from the stated length (', seq.length,
            '). In entry ', self$getFieldValue('accession'), '.')
    self$setFieldValue('nt.seq', sequence)
    self$setFieldValue('nt.seq.length', seq.length)
}
))
