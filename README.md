# Project Title: Impact of Carbon Dioxide Taxes

---

## Description

This project aims to evaluate the impact of carbon dioxide taxes on reducing 
emissions. By analyzing historical data, policy implementations, and economic
indicators, we will assess the effectiveness of carbon taxes in promoting 
sustainable environmental practices and reducing carbon footprints.

## Directory Structure

The directory structure for this project is as follows:

```
├── README.md
├── data
│   ├── raw
│   ├── processed
├── src
│   ├── 00_preprocess.R
├── results
│   ├── figures
│   ├── tables
├── setup.R
├── LICENSE
```

## Contents

- **data**: Contains all datasets used in the analysis.
  - `raw`: Raw data as obtained from the sources.
  - `processed`: Cleaned and processed data ready for analysis.

- **src**: R scripts for the project.
  - `00_preprocess.R`: Script with preprocessing of data. Create data/processed/panel_data.csv
  - `01_eda.R`: Script with exploratory analysis
  - `02_assumptions.R`: script with `fect` implementation
  - `02_DID_PTA.R`: script with DiD implementation

- **results**: Contains results of the analysis.
  - `figures`: Graphs and plots generated from the analysis.
  - `tables`: Tables summarizing the findings.
  - `final_presentation.qmd`: Quarto presentation code

## Getting Started

### Prerequisites

Ensure you have the following installed:

- R
- RStudio
- Git

### Installation

1. Clone the repository:

```sh
git clone https://github.com/Songeo/climate_change_project.git
```

2.	Navigate to the project directory:

```sh
cd climate_change_project
```

### Setup

1.	Source the setup script to configure the project environment:

```sh
source("setup.R")
```

### Download data

1. Download raw data from 
[Drive Link](https://drive.google.com/drive/folders/1EpnqGvAHDo0NDhW5E86HlN9H6RxGvfex?usp=drive_link)

Data source: [Climate Change Indicators Dashboard](https://climatedata.imf.org/pages/access-data) 


2. Review variables and frequencies at [Aux] Variables description File [Doc Link](https://docs.google.com/spreadsheets/d/1Wi-eEsxhlAh1JGq6Y8fvvN3mqcajraz1aXOsq9fTLU8/edit?usp=sharing)
