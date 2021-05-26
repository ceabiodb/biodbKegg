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
biodb::runGenericTests(conn, opt=list(skip.searchable.fields='ref.title'))

# Terminate Biodb
biodb$terminate()
