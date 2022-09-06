This is a repository designed to house reproducible code for paper, "Patterns of Maladaptive Exercise Behavior from ages 14-24 in a Longitudinal Cohort" (Schaumberg, Under Review). Code is available here, with a formatted, html version of the paper available at [https://kschaumb.github.io/alspac-dex-1-bookdown/](https://kschaumb.github.io/alspac-dex-1-bookdown/)

With all appropriate raw variables available in from ALSPAC , analyses can be replicated and the paper produced with three steps. 

1. Clean the data using the scorekeeper method. Scoresheets and scripts for data cleaning are available in the [alspac-scorekeep](https://github.com/embark-lab/alspac-scorekeep) repository, and a bookdown clarifying the data cleaning steps is available at [alspac-bookdown](https://embark-lab.github.io/alspac-bookdown/). Maintain the cleaned data in a 'data/' directory, which you should add as a subdirectory to the current project. 

2. Run the 'Run_R.RMD' script, located in the [R](R/) folder within the current project. This script runs all preparatory analysis in the R folder, and, from beginning to end, will take several hours of runtime on a local machine. 

3. Build a book by opening the `ALSPAC_exercise_1.Rproj` project file in this directory and bulding book using R commands. 

Each piece of analysis can, of course, also be isolated and specific steps or pieces of the analysis replicated in a piecemeal fashion.