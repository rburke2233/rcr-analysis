# rcr-analysis

Data and analysis for the Reading Chicago Reading project. **Reading Chicago Reading** is a digital humanities project focused on the Chicago Public Library's "One Book, One Chicago" program. Our goal is to build a multi-disciplinary understanding of how and why patrons choose to participate by reading and discussing the "One Book" chosen each year. The project brings together data from many sources including circulation data from CPL itself. For more about the project, visit our project blog at http://dh.depaul.press/reading-chicago. 

### Contents

This repository contains a set of R scripts and R markdown files used to generate our analysis of demographic and circulation data that has been our focus so far. We will continue to enhance this repository with additional analyses as they are completed.

Note that this respository does not contain any circulation data from the Chicago Library data. We are not at liberty to share this data, so please do not ask. Files that use this data collect it through a database connection, which requires a password to create. These files cannot be run in their present form except by members of our team.

**Subdirectories**

- /data
	- /aux: Shapefiles for the map-based visualizations
	- /branch: Demographic data for the neighborhoods surrounding the library branches
	
- /src
	- /demographic: Finds principal components of the demographic data, computes clusters
	- /model: Computes the multi-level model
	- /viz: Computes various visualizations
	
### Rmd files

Each Rmd file is accompanied by an R script that contains all of the relevant R code. The Rmd file pulls in chunks from the associated script so that hte code can be executed both as a source file and as a markdown file without duplication of code. The "Knit with Parameters" functionality is used to supply the database password.

### Acknowledgement

Special thanks to Zack Budde for contributions to various aspects of this analysis.
