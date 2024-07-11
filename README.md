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
├── reports
│   ├── figures
│   ├── draft.md
│   ├── final_report.md
├── setup.R
├── LICENSE
```

## Contents

- **data**: Contains all datasets used in the analysis.
  - `raw`: Raw data as obtained from the sources.
  - `processed`: Cleaned and processed data ready for analysis.

- **src**: R scripts for the project.
  - `00_preprocess.R`: Script with preprocessing of data. Create data/processed/panel_data.csv

- **results**: Contains results of the analysis.
  - `figures`: Graphs and plots generated from the analysis.
  - `tables`: Tables summarizing the findings.

- **reports**: Drafts and final reports.
  - `figures`: Figures used in the reports.
  - `draft.md`: Initial draft of the report.
  - `final_report.md`: Final version of the report.

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

1. Download data from 
[Drive Link](https://drive.google.com/drive/folders/1EpnqGvAHDo0NDhW5E86HlN9H6RxGvfex?usp=drive_link)

Data source: [Climate Change Indicators Dashboard](https://climatedata.imf.org/pages/access-data) 

