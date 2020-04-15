# Main
################################################################

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(log='kegg_module_test.log', ack=TRUE)

# Load package definitions
file <- system.file("definitions.yml", package='biodbKegg')
biodb$loadDefinitions(file)

# Set context
biodb::setTestContext(biodb, "Test Kegg Module connector.")

# Create connector
conn <- biodb$getFactory()$createConn('kegg.module')

# Run tests
biodb::runGenericTests(conn)

# Terminate Biodb
biodb$terminate()

