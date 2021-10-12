
#' The connector class to KEGG Enzyme database.
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
#' mybiodb <- biodb::newInst()
#'
#' # Get connector
#' conn <- mybiodb$getFactory()$createConn('kegg.enzyme')
#'
#' # Get pathway IDs related to enzymes
#' pathway.ids=conn$getPathwayIds(c('1.2.1.3', '3.7.1.3'), org='mmu')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @importFrom R6 R6Class
#' @include KeggConn.R
#' @export
KeggEnzymeConn <- R6::R6Class("KeggEnzymeConn",
inherit=KeggConn,


public=list(

#' @description
#' New instance initializer. Connector classes must not be instantiated
#' directly. Instead, you must use the createConn() method of the factory class.
#' @param ... All parameters are passed to the super class initializer.
#' @return Nothing.
initialize=function(...) {
    super$initialize(db.name='enzyme', db.abbrev='ec', ...)
},

#' @description
#' Gets organism pathways.  This method retrieves KEGG pathways of the
#' specified organism in which the enzymes are involved.
#' @param id A character vector of KEGG Compound IDs.
#' @param org The organism in which to search for pathways, as a KEGG organism
#' code (3-4 letters code, like 'hsa', 'mmu', ...). See
#' https //www.genome.jp/kegg/catalog/org_list.html for a complete list of KEGG
#' organism codes.
#' @return A vector of KEGG pathway IDs.
getPathwayIds=function(id, org) {

    pathways <- character()
    fact <- self$getBiodb()$getFactory()
    kegg.gen.conn <- fact$getConn('kegg.genes')

    # Loop on all enzymes
    for (enz.id in id) {

        enz <- self$getEntry(enz.id)
        if (is.null(enz))
            next

        pws <- private$getEnzymePathayIDs(enz=enz, org=org)

        # Record found pathways
        if ( ! is.null(pws))
            pathways <- c(pathways, pws)
    }

    return(pathways)
}
),

private=list(
getEnzymePathayIDs=function(enz, org) {

    pws <- NULL
    fact <- self$getBiodb()$getFactory()

    # Does this enzyme have a list of pathways?
    if (enz$hasField('kegg.pathway.id')) {

        # Get pathways
        pws <- enz$getFieldValue('kegg.pathway.id')

        # Convert them to specified organism
        kegg.path.conn <- fact$getConn('kegg.pathway')
        pws <- kegg.path.conn$convertToOrgPathways(pws, org=org)
    }

    # Look for genes
    else if ( ! is.null(enz) && enz$hasField('kegg.genes.id')) {

        # We skip non organism genes
        genes_ids <- enz$getFieldValue('kegg.genes.id')
        mmu_genes_ids <- genes_ids[grep(paste0('^', org, ':'), genes_ids)]
        kegg.gen.conn <- fact$getConn('kegg.genes')

        for (gene in kegg.gen.conn$getEntry(mmu_genes_ids, drop=FALSE)) {

            # We check that this gene is related to the organism:
            if ( ! is.null(gene) && gene$hasField('kegg.pathway.id')) {

                # Get pathways
                pws <- gene$getFieldValue('kegg.pathway.id')

                # Filter out wrong pathways
                kpc <- fact$getConn('kegg.pathway')
                x <- kpc$makesRefToEntry(pws, db='kegg.enzyme',
                    oid=enz.id, recurse=TRUE)
                pws <- pws[x]
            }
        }
    }

    return(pws)
}
))
