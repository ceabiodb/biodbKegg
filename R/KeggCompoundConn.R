#' The connector class to KEGG Compound database.
#'
#' This is a concrete connector class. It must never be instantiated directly,
#' but instead be instantiated through the factory \code{BiodbFactory}.
#' Only specific methods are described here. See super classes for the
#' description of inherited methods.
#'
#' @seealso \code{\link{KeggConn}}, \code{\link{KeggPathwayConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Create a connector to KEGG Compound
#' conn <- mybiodb$getFactory()$createConn('kegg.compound')
#'
#' # Search for compounds by exact mass
#' conn$wsFindExactMass(mass=174.05, retfmt='parsed')
#'
#' # Search for compounds by molecular weight 
#' conn$wsFindMolecularWeight(mass=300, retfmt='parsed')
#'
#' # Get pathway IDs related to compounds
#' pathway.ids=conn$getPathwayIds(c('C02648', 'C06144'), org='mmu')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include KeggConn.R
#' @import chk
#' @import lifecycle
#' @export
KeggCompoundConn <- R6::R6Class("KeggCompoundConn",
inherit=KeggConn,

public=list(

#' @description
#' New instance initializer. Connector classes must not be instantiated
#' directly. Instead, you must use the createConn() method of the factory class.
#' @param ... All parameters are passed to the super class initializer.
#' @return Nothing.
initialize=function(...) {
    super$initialize(db.name='compound', db.abbrev='cpd', accession.prefix='C',
        ...)
},

#' @description
#' Searches for entries by mass.
#' You must either provide a single mass through `mass` parameter or provide a
#' range through `mass.min` and `mass.max`.
#' @param See http //www.kegg.jp/kegg/docs/keggapi.html for details.
#' @param mass Single mass.
#' @param mass.min Minimal mass.
#' @param mass.max Maximal mass.
#' @param ... parameters passed to KeggConn::wsFind().
#' @return wsFind().
wsFindExactMass=function(mass=NULL, mass.min=NULL, mass.max=NULL, ...) {

    lifecycle::deprecate_soft('1.0.0', "wsFindExactMass()", "wsFind()")
    query <- if ( ! is.null(mass.min) && ! is.null(mass.max))
        paste(mass.min, mass.max, sep='-') else as.character(mass)
    if (is.null(query))
        biodb::error0('You need to specify either mass parameter or both',
                    ' mass.min and mass.max.')
    return(self$wsFind(query=query, option='exact_mass', ...))
},

#' @description
#' Searches for entries by molecular mass.
#' You must either provide a single mass through `mass` parameter or provide a
#' range through `mass.min` and `mass.max`.
#' See http //www.kegg.jp/kegg/docs/keggapi.html for details.
#' @param mass Single mass.
#' @param mass.min Minimal mass.
#' @param mass.max Maximal mass.
#' @param ... Parameters passed to KeggConn::wsFind().
#' @return wsFind().
wsFindMolecularWeight=function(mass=NULL, mass.min=NULL, mass.max=NULL, ...) {

    lifecycle::deprecate_soft('1.0.0', "wsFindMolecularWeight()", "wsFind()")
    query <- if ( ! is.null(mass.min) && ! is.null(mass.max))
        paste(mass.min, mass.max, sep='-') else as.character(mass)
    if (is.null(query))
        biodb::error0('You need to specify either mass parameter or both',
                    ' mass.min and mass.max.')
    return(self$wsFind(query=query, option='mol_weight', ...))
},

#' @description
#' Gets organism pathways for each compound. This method retrieves for
#' each compound the KEGG pathways of the organism in which the compound is
#' involved.
#' @param id A character vector of KEGG Compound IDs.
#' @param org The organism in which to search for pathways, as a KEGG organism
#' code (3-4 letters code, like 'hsa', 'mmu', ...). See
#' https //www.genome.jp/kegg/catalog/org_list.html for a complete list of KEGG
#' organism codes.
#' @param limit The maximum number of modules IDs to retrieve for each compound.
#' Set to 0 to disable.
#' @return A named list of KEGG pathway ID vectors, where the names
#' of the list are the compound IDs."
getPathwayIdsPerCompound=function(id, org, limit=3) {
    pathways <- list()

    fac <- self$getBiodb()$getFactory()
    kegg.enz.conn <- fac$getConn('kegg.enzyme')

    # Loop on all compound ids
    prg <- biodb::Progress$new(biodb=self$getBiodb(),
        msg='Retrieving pathways of compounds.', total=length(id))
    for (comp.id in id) {

        # Send progress message
        prg$increment()

        # Get compound entry
        comp <- self$getEntry(comp.id)
        if (is.null(comp))
            next

        # Get pathway IDs
        pws <- private$getCompoundPathwayIds(comp=comp, org=org)

        # Record found pathways
        if ( ! is.null(pws)) {
            if (limit > 0 && length(pws) > limit)
                pws <- pws[seq_len(limit)]
            pathways[[comp.id]] <- pws
        }
    }

    return(pathways)
},

#' @description
#' Gets organism modules for each compound. This method retrieves for
#' each compound the KEGG modules of the organism in which the compound is
#' involved.
#' @param id A character vector of KEGG Compound IDs.
#' @param org The organism in which to search for modules, as a KEGG organism
#' code (3-4 letters code, like 'hsa', 'mmu', ...). See
#' https //www.genome.jp/kegg/catalog/org_list.html for a complete list of KEGG
#' organism codes.
#' @param limit The maximum number of modules IDs to retrieve for each compound.
#' Set to 0 to disable.
#' @return A named list of KEGG module ID vectors, where the names
#' of the list are the compound IDs."
getModuleIdsPerCompound=function(id, org, limit=3) {
    modules <- list()
    pw <- self$getBiodb()$getFactory()$getConn('kegg.pathway')

    # Get pathway IDs
    pwids <- self$getPathwayIdsPerCompound(id=id, org=org)

    # Loop on all compounds
    for (i in pwids) {
        # Retrieve pathway entries for this compound
        pw.entries <- pw$getEntry(i, nulls=FALSE, drop=FALSE)
        modids <- lapply(pw.entries,
            function(e) e$getFieldValue('kegg.module.id'))
        modids <- unlist(modids)
        modids <- modids[ ! is.na(modids)]
        modids <- unique(modids)
        if (limit > 0 && length(modids) > limit)
            modids <- modids[seq_len(limit)]
        modules <- c(modules, list(modids))
    }

    # Set names
    if (length(modules) > 0)
        names(modules) <- names(pwids)

    return(modules)
},

#' @description
#' Gets organism pathways. This method retrieves KEGG pathways of the
#' specified organism in which the compounds are involved.
#' @param id A character vector of KEGG Compound IDs.
#' @param org The organism in which to search for pathways, as a KEGG organism
#' code (3-4 letters code, like 'hsa', 'mmu', ...). See
#' https //www.genome.jp/kegg/catalog/org_list.html for a complete list of KEGG
#' organism codes.
#' @return A vector of KEGG pathway IDs.
getPathwayIds=function(id, org) {

    pathways <- self$getPathwayIdsPerCompound(id=id, org=org)
    pathways <- unique(unlist(pathways, use.names=FALSE))

    return(pathways)
},

#' @description
#' Add informations (as new column appended to the end) to an existing
#' data frame containing a column of KEGG Compound IDs.
#' @param x A data frame containing at least one column with Biodb entry IDs
#' identified by the parameter `id.col`.
#' @param id.col The name of the column containing IDs inside the input data
#' frame.
#' @param org The organism in which to search for pathways, as a KEGG organism
#' code (3-4 letters code, like 'hsa', 'mmu', ...). See
#' https //www.genome.jp/kegg/catalog/org_list.html for a complete list of KEGG
#' organism codes.
#' @param limit This is the maximum number of values obtained for each ID, for
#' every column added, in case multiple values are obtained. Set to 0 to get
#' all values.
#' @param prefix Insert a prefix at the start of name of all new columns.
#' @return A data frame containing `x` and new columns appended with
#' KEGG identifiers and data.
addInfo=function(x, id.col, org, limit=3, prefix='') {

    chk::chk_is(x, 'data.frame')

    if (ncol(x) > 0) {
        chk::chk_character(id.col, 'character')
        if ( ! id.col %in% colnames(x))
            biodb::error0('Column "', id.col,
                        '" was not found inside data frame.')

        # Get ids
        ids <- as.character(x[[id.col]])

        # Get entries
        entries <- self$getEntry(ids)

        # Add enzyme IDs and reaction IDs
        ei <- self$getBiodb()$entriesFieldToVctOrLst(entries,
            field='kegg.enzyme.id', limit=limit)
        fields <- c('kegg.enzyme.id', 'kegg.reaction.id')
        y <- self$getBiodb()$entryIdsToDataframe(ei, db='kegg.enzyme',
            limit=limit, fields=fields, own.id=TRUE)

        # Add pathway info
        pwids <- self$getPathwayIdsPerCompound(ids, org=org, limit=limit)
        fields <- c('kegg.pathway.id', 'name', 'pathway.class')
        df2 <- self$getBiodb()$entryIdsToDataframe(pwids, db='kegg.pathway',
            limit=limit, fields=fields, own.id=TRUE, prefix='kegg.pathway.')
        y <- cbind(y, df2)

        # Add module info
        modids <- self$getModuleIdsPerCompound(ids, org=org, limit=limit)
        fields <- c('kegg.module.id', 'name')
        df3 <- self$getBiodb()$entryIdsToDataframe(modids, db='kegg.module',
            limit=limit, fields=fields, own.id=TRUE, prefix='kegg.module.')
        y <- cbind(y, df3)

        # Rename columns
        if ( ! is.na(prefix) && nchar(prefix) > 0 && length(colnames(y)) > 0)
            colnames(y) <- paste0(prefix, colnames(y))

        # Add info columns
        x <- cbind(x, y)
    }

    return(x)
}
),

private=list(

doGetEntryImageUrl=function(id) {
    # Overrides super class' method.

    fct <- function(x) {
        bu <- self$getPropValSlot('urls', 'base.url')
        u <- c(bu, 'Fig', 'compound', paste(x, 'gif', sep='.'))
        BiodbUrl$new(url=u)$toString()
    }

    return(vapply(id, fct, FUN.VALUE=''))
}

,getCompoundPathwayIds=function(comp, org) {

    pws <- NULL
    fac <- self$getBiodb()$getFactory()

    # Does this compound have a list of pathways?
    if (comp$hasField('kegg.pathway.id')) {

        # Get pathways
        pws <- comp$getFieldValue('kegg.pathway.id')

        # Convert them to specified organism
        kegg.path.conn <- fac$getConn('kegg.pathway')
        pws <- kegg.path.conn$convertToOrgPathways(pws, org=org)
    }

    # Look for enzymes
    else if (comp$hasField('kegg.enzyme.id')) {

        # Get pathways
        enzid <- comp$getFieldValue('kegg.enzyme.id')
        kegg.enz.conn <- fac$getConn('kegg.enzyme')
        pws <- kegg.enz.conn$getPathwayIds(enzid, org=org)

        # Filter out wrong pathways
        kpc <- fac$getConn('kegg.pathway')
        pws <- pws[kpc$makesRefToEntry(pws, db='kegg.compound',
            oid=comp$getFieldValue('accession'), recurse=TRUE)]
    }

    return(pws)
}
))
