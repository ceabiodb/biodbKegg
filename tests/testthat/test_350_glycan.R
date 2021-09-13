# Set context
biodb::testContext("Test Kegg Glycan connector.")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(ack=TRUE)

# Load package definitions
file <- system.file("definitions.yml", package='biodbKegg')
biodb$loadDefinitions(file)

# Create connector
conn <- biodb$getFactory()$createConn('kegg.glycan')

# Run tests
testRefFolder <- system.file("testref", package='biodbKegg')
biodb::runGenericTests(conn, pkgName='biodbKegg', testRefFolder=testRefFolder,
    opt=list(skip.searchable.fields=c('ref.accession', 'ref.authors', 'ref.doi',
        'ref.journal', 'ref.title')))

# Terminate Biodb
biodb$terminate()
