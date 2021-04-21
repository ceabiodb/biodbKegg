# Set context
biodb::testContext("Test Kegg Reaction connector.")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(ack=TRUE)

# Load package definitions
file <- system.file("definitions.yml", package='biodbKegg')
biodb$loadDefinitions(file)

# Create connector
conn <- biodb$getFactory()$createConn('kegg.reaction')

# Run tests
biodb::runGenericTests(conn)

# Terminate Biodb
biodb$terminate()

