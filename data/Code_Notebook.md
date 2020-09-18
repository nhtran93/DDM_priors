[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

# Variable Notebook

## `data/DDM_cleaned.RData` or `data/DDM_cleaned.xlsx`
Both data files correspond to each other. The data is the same and is just stored in different file formats. The different Excel Sheets in `data/DDM_cleaned.xlsx` correspond to the different dataframes in `data/DDM_cleaned.RData` which are the following (ordered in alphabetical order):
- a: Boundary Separation (unique to the subject or group in the same experiment in the same paper)
- data: Complete data containing all reported estimates and individual-level and group-level estimates
- ster: Non-decision time variability between trials (unique to the subject or group in the same experiment in the same paper)
- sv: Drift rate variability between trials (unique to the subject or group in the same experiment in the same paper)  
- sz: Starting point variability between trials (unique to the subject or group in the same experiment in the same paper)
- sz_relative: Relative starting point variability between trials (unique to the subject or group in the same experiment in the same paper)
- ter: Non-decision time (unique to the subject or group in the same experiment in the same paper)
- v:  Drift rate (unique to the subject or group in the same experiment in the same paper)
- z: Starting point (unique to the subject or group in the same experiment in the same paper)
- zr: Bias/ Relative starting point (unique to the subject or group in the same experiment in the same paper)
- zr_mirrored: Mirrored Bias/ Mirrored Relative starting point (unique to the subject or group in the same experiment in the same paper)

The distribution of each parameter estimate consisted of reported estimates that were unique to the subject or group in the same experiment in the same paper. We did not repeatedly consider estimates that were fixed across conditions, subjects or groups within the same experiment in the same paper to avoid biased distributions. Only the unique parameter estimates retrieved from the same experimental design and model parameterization were considered and can be found in the corresponding parameter dataframe or Excel sheet (e.g, a or ster).

The following variables can be found in the dataframes and Excel sheets:
  
- drift_rate: Empirical drift rate estimates found in the literature review.  
- a: Empirical boundary separation estimates found in the literature review.  
- zr: Empirical bias estimates found in the literature review.  
- z: Empirical starting point estimates found in the literature review.  
- ter: Empirical non-decision estimates found in the literature review.  
- sv: Empirical variability between trials in drift rates estimates found in the literature review.  
- sz: Empirical variability between trials in starting point estimates found in the literature   review.  
- ster: Empirical variability between trials in non-decision time estimates found in the literature review.  
- complete_reference: The bibliography of the paper.  
- extracted_grap: Indication whether the estimate was retrieved from a figure in the paper (1 = from a figure, 0 = not from a figure).  
- fit_other_data: Indication whether the same data has already been fit and reported in another previous paper. If the data originates from another paper and has been fit in that paper, a bibliography is given.  


## `data/parameter_distributions.RData`
The `data/parameter_distributions.RData` contains the table of prior distributions of the corresponding parameters of the DDM. The values correspond to table 1 "Generated representative distributions for DDM parameters".
The data file contains a list 'parameter_distributions' for each DDM parameter:   
- v: drift rate  
- a: boundary separation  
- z: starting point  
- zr: Bias/ relative starting point  
- zr_mirrored: Mirrored Bias  
- ter: Non-decision time  
- sv: Between trial variability in drift rate  
- sz: Between trial variability in starting point  
- relative_sz: Between trial variability in relative starting point  
- ster: Between trial variability in non-decision time  
  

Each of these parameter entries contains another sublist:  
- distribution: A dataframe containing or vector containing the best-fitting distribution. If the best-fitting distribution is a mixture distribution, then a dataframe is stored with information on the mixture distribution and what Akaike weight it has. Otherwise a vector with the name of the univariate distribution is stored.  
- mixing: A value referring to the mixing weight of the best-fitting distribution component.   
- location: The location parameter.  
- shape: The shape parameter.  
- scale: The scale or variance parameter.  
- df: The degrees of freedom.  

## Authors

* **[N.-Han Tran](https://www.eva.mpg.de/ecology/staff/han-tran/index.html)**  
* [Leendert van Maanen](http://leendertvanmaanen.com/)  
* [Andrew Heathcote](http://www.tascl.org/andrew-heathcote.html)  
* [Dora Matzke](http://dora.erbe-matzke.com/)  


## License

This project is licensed under the CC-BY-4.0 License - see the [LICENSE.md](LICENSE.md) file for details.
