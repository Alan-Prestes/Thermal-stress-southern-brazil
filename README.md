# Daily Thermal Stress Climatology for Southern Brazil

This repository contains the R code used to obtain, process, and summarize climatic data for the assessment of thermal stress in dairy cattle across municipalities in Southern Brazil.

The workflow was developed to support the scientific study of historical patterns of the Temperature-Humidity Index (THI), thermal load, and daily duration of heat stress.

## Study area

The analysis includes municipalities from the three states of Southern Brazil:

* Paraná (PR)
* Santa Catarina (SC)
* Rio Grande do Sul (RS)

Municipality boundaries and geographic coordinates are obtained programmatically using the `geobr` package.

## Climatic data

Hourly climatic data are retrieved from the NASA POWER database using the `nasapower` R package.

The following variables are used:

* **T2M** — Air temperature at 2 meters (°C)
* **RH2M** — Relative humidity at 2 meters (%)

The historical period considered in the analysis is **2006–2025**.

## Analytical workflow

The R script performs the following steps:

1. Retrieves all municipalities in Southern Brazil using `geobr`.
2. Determines a representative geographic coordinate for each municipality.
3. Downloads hourly temperature and relative humidity data from NASA POWER.
4. Checks the temporal completeness of the climatic records.
5. Calculates daily maximum and minimum temperature.
6. Calculates daily maximum and minimum relative humidity.
7. Estimates daily minimum and maximum THI assuming an **anticyclic daily pattern between temperature and relative humidity**.
8. Estimates daily thermal load (`THIload`) and the duration of thermal stress above the defined THI threshold.
9. Repeats these calculations independently for each day of each year.
10. Calculates historical daily means for the 2006–2025 period.
11. Removes February 29 to generate a standardized **365-day climatology** for each municipality.
12. Performs quality-control procedures to verify data completeness for all municipalities.

## Anticyclic approach

The daily thermal cycle was modeled assuming an inverse relationship between air temperature and relative humidity.

Therefore:

* minimum THI is estimated from **minimum temperature and maximum relative humidity**;
* maximum THI is estimated from **maximum temperature and minimum relative humidity**.

Thermal load and daily heat-stress duration are calculated from these daily THI limits before the historical daily climatological means are obtained.

## Heat-stress threshold

A THI threshold of **70** is used to characterize thermal stress.

For each day and year, the workflow estimates:

* minimum THI;
* maximum THI;
* thermal load (`THIload`);
* daily duration of exposure above the THI threshold (hours/day).

The climatological values are subsequently calculated by averaging the corresponding daily estimates across the historical period.

## Final dataset

For each municipality, the final climatological dataset contains **365 observations**, representing the historical daily climatology.

The main variables include:

| Variable         | Description                                                  |
| ---------------- | ------------------------------------------------------------ |
| `code_muni`      | Municipality IBGE code                                       |
| `name_muni`      | Municipality name                                            |
| `Estado`         | Brazilian state                                              |
| `lon`            | Longitude                                                    |
| `lat`            | Latitude                                                     |
| `MO`             | Month                                                        |
| `DY`             | Day of month                                                 |
| `DOY`            | Day of year (1–365)                                          |
| `T_max`          | Historical mean daily maximum temperature                    |
| `T_min`          | Historical mean daily minimum temperature                    |
| `UR_max`         | Historical mean daily maximum relative humidity              |
| `UR_min`         | Historical mean daily minimum relative humidity              |
| `ITU_max`        | Historical mean daily maximum THI                            |
| `ITU_min`        | Historical mean daily minimum THI                            |
| `THIload`        | Historical mean daily thermal load                           |
| `horas_estresse` | Historical mean daily duration of thermal stress (hours/day) |

## Data quality control

The workflow includes automatic checks for:

* presence of all expected calendar days;
* availability of hourly temperature and relative humidity records;
* number of valid observations per day;
* completeness of the historical period;
* 365 climatological observations per municipality;
* expected range of heat-stress duration (0–24 h/day);
* consistency between minimum and maximum THI.

Municipalities with incomplete or problematic records are identified and reported separately.

## R packages

The main R packages required are:

```r
library(geobr)
library(sf)
library(nasapower)
library(dplyr)
library(lubridate)
library(writexl)
```

## Repository structure

```text
thermal-stress-southern-brazil/
│
├── README.md
├── LICENSE
├── CITATION.cff
│
└── R/
    └── climatologia_ITU_Sul.R
```

## Running the analysis

Clone or download this repository and open the project in R/RStudio.

Install the required packages if necessary and run:

```r
source("R/climatologia_ITU_Sul.R")
```

The output directory can be defined in the R script according to the user's local environment.

Because climatic data are retrieved directly from NASA POWER, internet access is required during data acquisition.

## Data sources

Climatic data are obtained from the **NASA Prediction Of Worldwide Energy Resources (NASA POWER)** project.

Municipality boundaries and geographic information are obtained through the Brazilian geospatial database accessed using the `geobr` R package.

## Reproducibility

The repository is intended to provide the computational workflow used in the associated scientific study and facilitate reproducibility of the climatic data acquisition, processing, quality control, and thermal-stress calculations.

The repository does not necessarily contain the complete raw hourly NASA POWER dataset. Instead, the R workflow allows the climatic data to be retrieved directly from the original source and processed using the procedures adopted in the study.

## Citation

If you use this code or workflow, please cite the associated scientific article and the archived version of this repository.

Citation information will be updated following publication of the associated article.

## License

The source code is made available for scientific and reproducibility purposes. See the `LICENSE` file for reuse conditions.

## Author

**Alan Prestes**

Researcher and Professor
Brazil
