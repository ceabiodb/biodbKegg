# Main
################################################################

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(log='kegg_reaction_test.log', ack=TRUE)

# Load package definitions
file <- system.file("definitions.yml", package='biodbKegg')
biodb$loadDefinitions(file)

# Set context
biodb::setTestContext(biodb, "Test Kegg Reaction connector.")

# Create connector
conn <- biodb$getFactory()$createConn('kegg.reaction')

# Run tests
biodb::runGenericTests(conn)

# Terminate Biodb
biodb$terminate()

