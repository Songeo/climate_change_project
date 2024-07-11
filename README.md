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
│   ├── 00_eda.R
├── results
│   ├── figures
│   ├── tables
├── reports
│   ├── figures
│   ├── draft.md
│   ├── final_report.md
├── requirements.txt
├── setup.R
├── LICENSE
```

## Contents

- **data**: Contains all datasets used in the analysis.
  - `raw`: Raw data as obtained from the sources.
  - `processed`: Cleaned and processed data ready for analysis.
  - `external`: Any external datasets or supplementary data.

- **src**: R scripts for the project.
  - `data_preprocessing.R`: Script for cleaning and preparing data.
  - `analysis.R`: Script for performing data analysis.
  - `models.R`: Script for developing and evaluating models.

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

