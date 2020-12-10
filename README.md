[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

# Systematic Quantitative Parameter Reviews]{Systematic Parameter Reviews in Cognitive Modeling: Towards a Robust and Cumulative Characterization of Psychological Processes in the Diffusion Decision Model

The following repository contains data and code used for analyses in:

Tran, N.-H., van Maanen, L., Heathcote, A., Matzke, D. (submitted) Systematic Quantitative Parameter Reviews]{Systematic Parameter Reviews in Cognitive Modeling: Towards a Robust and Cumulative Characterization of Psychological Processes in the Diffusion Decision Model.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine. 

### Prerequisites

In order to be able to run the code, you will need to have an up-to-date version of [R](https://www.r-project.org/) installed on your computer and a few CRAN packages (see below).

```
# Install required packages
install.packages(c("tidyr", "plyr", "dplyr", "msm", "openxlsx", "MASS", "knitr", "ggplot2"))
```

### Data
`data/DDM_cleaned.RData`: This file contains all the empirical data collected from the literature review. The data is cleaned and divided into the different dataframes.  
`data/DDM_cleaned.xlsx`: This file contains the empirical data collected from the literature review. The data is cleaned and divided into different Excel Sheets. Each Excel Sheet page corresponds to a dataframe in the /DDM_cleaned.RData.  
parameter_distributions.RData: This file contains the table of representative distributions of the corresponding parameters of the DDM. The values correspond to the table 1 "Generated informative Prior Distributions" in the article.  
`data/DDM_cleaned_non_clinical.RData`: This file contains the empirical data collected from the literature review without any clinical studies. The data is cleaned and divided into the different dataframes.  
`data/DDM_cleaned_non_clinical.xlsx`: This file contains the empirical data collected from the literature review  without any clinical studies. The data is cleaned and divided into different Excel Sheets. Each Excel Sheet page corresponds to a dataframe in the /DDM_cleaned_non_clinical.RData.  
`data/priors_all.RData`: This file contains the main table of informative prior distributions across all empirical parameter estimates. The values correspond to the main table 1 "Informative Prior Distributions" in the article.  
`data/priors_lexical_decision_task.RData`: This file contains informative prior distributions specific to the lexical decision task. The values correspond to the table 1 "Generated informative Prior Distributions" in the article.  
`data/priors_non_clinical.RData`: This file contains informative prior distributions specific to all empirical estimates from non-clinical studies. The values correspond to the table S1 "Informative Priors Distributions for the Lexical Decision Task" in the article.  
`data/priors_random_dot_motion.RData`: This file contains informative prior distributions specific to the random dot motion task. The values correspond to the table S2 "Informative Priors Distributions for the Random Dot Motion Task" in the article.  

### Structure of the Repository
`data/`: Contains the data.  
`plots/`: All the figures produced by the R scripts will be saved here.  
`code/`: Contains the R script to plot the figures in the manuscript and the Rmarkdown with code and annotations to generate and compare different distribution functions to the empirical distribution.  

## Run the main analyses
Download all files from https://github.com/nhtran93/DDM_priors or https://osf.io/9ycu5/.

If you use R, please set the working directory to the appropriate directory where you have saved these files. If you use RStudio, you can just open the respective RStudio project and the directory will be automatically set.

You will only need to execute one R script: `code/DDM_priors_all.Rmd`. The [Rscript](code/DDM_priors_all.Rmd) contains all the code to fit the univariate and mixture distribution functions to the empirical distributions. `code/DDM_priors_all.Rmd` calls necessary functions to execute the code from `code/helpers.R`.

## Run the SI analyses
If you are are interested in fitting prior distributions to non-clinical studies, the random dot motion task or the lexical decision task, you can do this as well by executing the corresponding Rmarkdown file (i.e. `code/DDM_priors_non_clinical.Rmd`, `code/DDM_priors_random_dot_motion.Rmd`, `code/DDM_priors_lexical_decision_task.Rmd`). 

## Authors

* **[N.-Han Tran](https://www.eva.mpg.de/ecology/staff/han-tran/index.html)**
* [Leendert van Maanen](http://leendertvanmaanen.com/)
* [Andrew Heathcote](http://www.tascl.org/andrew-heathcote.html)
* [Dora Matzke](http://dora.erbe-matzke.com/)


## License

This project is licensed under the CC-BY-4.0 License - see the [LICENSE.md](LICENSE.md) file for details.
