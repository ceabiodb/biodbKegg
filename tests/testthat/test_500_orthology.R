# Set context
biodb::testContext("Test Kegg Orthology connector.")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(ack=TRUE)

# Load package definitions
file <- system.file("definitions.yml", package='biodbKegg')
biodb$loadDefinitions(file)

# Create connector
conn <- biodb$getFactory()$createConn('kegg.orthology')

# Run tests
biodb::runGenericTests(conn,
    opt=list(skip.searchable.fields=c('ref.accession', 'ref.authors', 'ref.doi',
        'ref.journal', 'ref.title')))

# Terminate Biodb
biodb$terminate()

