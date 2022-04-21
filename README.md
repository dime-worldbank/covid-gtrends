# Replication package for: The Evolution of the COVID-19 Pandemic Through the Lens of Google Searches

## Code

### Main Script 
* `_main.R`: Main script that runs all code, including data cleaning and analysis.

### Organization
Code is organized into two main folders. [DataWork](https://github.com/worldbank/covid-gtrends/tree/main/DataWork) includes all code to replicate analysis of the paper. [Dashboard](https://github.com/worldbank/covid-gtrends/tree/main/Dashboard) contains code to develop the [dashboard](https://datanalytics.worldbank.org/covid_gtrends/) associated with this project.

The `DataWork` folder is organized into the below folders. The number indicates code that must be run before others. For example, code in `01_` should be run before `02_`; however, folders with the same number can be run in any order.
* `01_process_ancillary_data`: Cleans individual datasets used throughout the analysis, including downloading data from specific sources (for example, downloading and cleaning data from the Oxford COVID-19 Government Response Tracker). 
* `02_translate_search_terms`: Translates each search term into difference languages
* `03_determine_most_common_language`: For each country, determines the most common language used to make Google searchers
* `04_scrape_gtrends_data`: Scrapes Google search data across countries, relying on the [gtrendsR package](https://cran.r-project.org/web/packages/gtrendsR/gtrendsR.pdf)
* `04_scrape_gtrends_us_data_across_terms`: Scrapes Google search data across U.S. States, relying on the [gtrendsR package](https://cran.r-project.org/web/packages/gtrendsR/gtrendsR.pdf)
* `05_clean_regions`: Cleans Google search data downloaded at the regional level.
* `05_clean_timeseries`: Cleans Google search data downloaded at the country level.
* `06_analysis`: All code for analysis, including generating figures and tables. 

## Data

Data can be found in the `/Data` folder [here](https://www.dropbox.com/sh/uhg7n8j8neq7cww/AADG_teKFPFe1qYxZvZ0lLUoa?dl=0). [NOTE: At a later date, all data will be moved into github; for now, please download from the provided link].

Within `/Data`, there is a folder for each dataset. Each dataset folder generally contains a `/RawData` and `/FinalData` folder, where `/RawData` contains data downloaded from its source and `FinalData` contains data processed from code. Each folder within `/Data` includes a readme that documents the source of the data.

## To replicate analysis

1. Clone/download this github repository.
2. Download the project folder that includes data.
3. In [_main.R](https://github.com/worldbank/covid-gtrends/blob/main/_main.R):
* `dropbox_file_path` should point to the data folder
* `github_file_path` should point to the code folder
* Set `RUN_CODE` to `TRUE`
4. Run `_main.R`; this runs all scripts needed to replicate the analysis, including data cleaning and generating all tables and figures


