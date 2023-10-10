# Replication package for: The Evolution of the COVID-19 Pandemic Through the Lens of Google Searches

## To replicate analysis

1. Clone this repository
2. In [`_main.R`](https://github.com/dime-worldbank/covid-gtrends/blob/main/_main.R), change `github_file_path` to point to the github repo.
3. Run [`_main.R`](https://github.com/dime-worldbank/covid-gtrends/blob/main/_main.R); this runs all scripts needed to replicate the analysis, including data cleaning and generating all tables and figures. Tables and figures are exported to: [`Paper Figures and Tables/`](https://github.com/dime-worldbank/covid-gtrends/tree/main/Paper%20Figures%20and%20Tables)

## Code

### Main Script 
* [`_main.R`](https://github.com/dime-worldbank/covid-gtrends/blob/main/_main.R): Main script that runs all code, including data cleaning and analysis.

### Organization
Code is organized into two main folders.

* [DataWork](https://github.com/worldbank/covid-gtrends/tree/main/DataWork) includes all code to replicate analysis of the paper. 

* [Dashboard](https://github.com/worldbank/covid-gtrends/tree/main/Dashboard) contains code to develop the [dashboard](https://datanalytics.worldbank.org/covid_gtrends/) associated with this project.

#### DataWork
The `DataWork` folder is organized into the below folders. The number indicates code that must be run before others. For example, code in `01_` should be run before `02_`; however, folders with the same number can be run in any order.
* `01_process_ancillary_data`: Cleans individual datasets used throughout the analysis, including downloading data from specific sources (for example, downloading and cleaning data from the [World Development Indicators](https://databank.worldbank.org/source/world-development-indicators)). 
* `02_translate_search_terms`: Translates each search term into different languages
* `03_determine_most_common_language`: For each country, determines the most common language used to make Google searches.
* `04_scrape_gtrends_data`: Scrapes Google search data across countries and keywords, relying on the [gtrendsR package](https://cran.r-project.org/web/packages/gtrendsR/gtrendsR.pdf)
* `05_clean_gtrends_data`: Cleans Google search data into analysis-ready datasets, including merging in data from other sources.
* `06_analysis`: All code for analysis, including generating figures and tables. 

#### Dashboard
To prepare data for the dashboard, the [`/Dashboard/_dash_main.R`](https://github.com/dime-worldbank/covid-gtrends/blob/main/Dashboard/_dash_main.R) should be run.

## Data

Data can be found in the `/Data` folder [here](https://github.com/dime-worldbank/covid-gtrends/tree/main/Dashboard). 
Within `/Data`, there is a folder for each dataset. Each dataset folder generally contains a `/RawData` and `/FinalData` folder, where `/RawData` contains data downloaded from its source and `FinalData` contains data processed from code. Each folder within `/Data` includes a readme that documents the source of the data.





