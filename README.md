# biodbKegg

[![Codecov test coverage](https://codecov.io/gh/pkrog/biodbKegg/branch/master/graph/badge.svg)](https://codecov.io/gh/pkrog/biodbKegg?branch=master)

An R Bioconductor package for accessing [KEGG](https://www.kegg.jp/) online
database, based on Bioconductor package/framework
[biodb](https://github.com/pkrog/biodb/).

## Introduction

*biodbKegg* is an  an extension package of the *biodb* package.
It allows to connect to KEGG for retrieving entries, searching for entries
by name or mass, and searching for pathways related to compounds.
It implements *biodb* connectors for the following KEGG databases:
 * KEGG Compound.
 * KEGG Enzyme.
 * KEGG Genes.
 * KEGG Module.
 * KEGG Orthology.
 * KEGG Pathway.
 * KEGG Reaction.

## Examples

Getting a single entry:
```r
bdb <- biodb::Biodb()
kegg <- bdb$getFactory()$createConn('kegg.compound')
entries <- kegg$getEntry(c('C00133', 'C00751'))
bdb$entriesToDataframe(entries)
```

Search by mass:
```r
ids <- kegg$searchForEntries(list(monoisotopic.mass=list(value=64, delta=2.0)),
    max.results=10)
```

## Installation


Install the latest stable version using Bioconductor:
```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install('biodbKegg')
```

## Documentation

See the introduction vignette:
```r
vignette('biodbKegg', package='biodbKegg')
```

And the vignette about pathways:
```r
vignette('kegg_pathways', package='biodbKegg')
```

## Citations

 * Kanehisa, M. and Goto, S.; KEGG: Kyoto Encyclopedia of Genes and Genomes. Nucleic Acids Res. 28, 27-30 (2000), <https://doi.org/10.1093/nar/28.1.27>.
