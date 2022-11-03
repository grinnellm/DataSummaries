# Summarise Pacific Herring catch, biological, and abundance data

You will need the [Spawn Index](https://github.com/grinnellm/SpawnIndex) package to calculate the spawn index.
The summary figures and tables generated here are used to make the [Reports](https://github.com/grinnellm/Reports).
The summary data file is used in the Pacfic Herring stock assessment model.

## How to

Read the wiki before you start.

Base folder: C:/Grinnell/Git/ -- this is the root where everything lives.

1. Get 32-bit MS Office.

1. Get databases from H:.
  1. Where to put them (relative path).
  1. Link the backend databases manually.

1. Get R-4.3.1 (last version with 32-bit).
  1. Install required packages -- these will become known as you proceed.
  1. Use github_install() for the pbs-assess packages and the SpawnIndex package.
  1. Use the R Gui if RStudio does not work (RStudio version).

1. Run the data summaries to load data from the databases and make tables and
figures.
  1. Get the DataSummaries repo.
  1. Get the HerringFunctions repo.
  1. Test run with HG to make sure it works.
  1. Make the animations -- test with HG to make sure it works.
  1. Production mode with RunSummaries.R.
  1. TODO: Create the required folders automatically: Animations, Summaries.

1. Compile the PDF
  1. Get the Reports repo.
  1. Can be 32- or 64-bit R.
  1. Link to tinytex help page.
  1. Test run with HG to make sure it works.
  1. Production mode with MakeSummaries.R (I think).
  1. Don't add the new PDFs to the repo until they have been read by Marisa.
  
SP: Don't make commits to my main branch.
Either start your own personal branch or fork a copy from mine.
  