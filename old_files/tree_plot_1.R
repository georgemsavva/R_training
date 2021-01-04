
library(read_excel)

# Load the data
TreeData <- read_excel(path="introstat.xlsx", sheet="P1-TreeSpeciesData",.name_repair="universal")

# Create a new variable corresponding to log of height.
TreeData$logHeight <- log(TreeData$height) 

# Make a plot
plot( logHeight ~ dgl, data = TreeData)

# What does this do?
cor.test(TreeData$logHeight , TreeData$dgl)
