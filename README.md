# Human-canine interaction data

This repository contains the code and data to reproduce the results in "Repurposing a peer-reviewed publication to engage students in statistics: An illustration of study design, data collection, and analysis" (Evans, Cipolli, Draper, and Binfet). 

## File list

* `dog_data_raw.csv`: the raw data from [Binfet *et al.* (2022)](https://www.tandfonline.com/doi/abs/10.1080/08927936.2021.1944558). To anonymize the data, several demographic variables have been removed.
* `dog_data_cleaned.csv`: data after initial cleaning and processing with `dog_data_cleaning.R`
* `data_dictionary.pdf`: data dictionary describing `dog_data_raw.csv` and `dog_data_cleaned.csv`
* `dog_data_cleaning.R`: process and transform raw data (`dog_data_raw.csv`) into format ready for analysis (`dog_data_cleaned.csv`)
* `data_analysis.R`: create figures and tables with the processed data
* `dog_paper_activity.*`: class activity introducing students to the [original research paper](https://www.tandfonline.com/doi/abs/10.1080/08927936.2021.1944558) by Binfet *et al.* Both PDF and HTML formats are provided, and the R Markdown source for the HTML
* `homework.css`: CSS file for formatting `dog_paper_activity.html`
* `pre_test_scales.pdf`: Figure 1 in Evans *et al.*
* `pairwise_relationships.pdf`: Figure 2 in Evans *et al.*
* `pre_post_diff.pdf`: Figure 3 in Evans *et al.*

## Workflow

To reproduce the results in Evans *et al.*: 

* Run `dog_data_cleaning.R` to create `dog_data_cleaned.csv`
* Run `data_analysis.R` to create the figures and tables