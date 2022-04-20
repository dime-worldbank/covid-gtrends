# Replication package for: The Evolution of the COVID-19 Pandemic Through the Lens of Google Searches

## Code

### Main Script 
* `_main.R`: Main script that runs all code, including data cleaning and analysis.

### Organization
Code is organized into the below folders:

## Data

Data can be found is the `/Data` folder [here](https://www.dropbox.com/sh/uhg7n8j8neq7cww/AADG_teKFPFe1qYxZvZ0lLUoa?dl=0).

Within `/Data`, there is a folder for each dataset. Each dataset folder generally contains a `/RawData` and `/FinalData` folder, where `/RawData` contains data downloaded from its source and `FinalData` contains data processed from code. Each folder within `/Data` includes a readme that documents the source of the data.

## To replicate analysis

1. Clone/download this github repository.
2. Download the project folder that includes data [here](https://www.dropbox.com/sh/uhg7n8j8neq7cww/AADG_teKFPFe1qYxZvZ0lLUoa?dl=0)
3. In [_main.R](https://github.com/worldbank/covid-gtrends/blob/main/_main.R):
* `dropbox_file_path` should point to the data folder
* `github_file_path` should point to the code folder
* Set `RUN_CODE` to `TRUE`
4. Run `_main.R`; this runs all scripts needed to replicate the analysis, including data cleaning and generating all tables and figures


