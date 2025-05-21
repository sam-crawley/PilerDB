# PilerDB

This R package contains Party/Identity-Language, Ethnicity and Religion (PILER)
database, along with the code to build the database.

The database contains a calculation of the degree of division in the country,
based on patterns of how different groups support political parties. This
calculation is based on data drawn from a number of publicly available
cross-national surveys.

The code is designed to make it straightforward to extend coverage in the
database by adding new surveys.

For more details, see:

Fraenkel J & Crawley S (forthcoming), Does ethnic fractionalization lead to polarization? Introducing a new dataset on communal party affiliations. Ethnopolitics.

## Accessing the data
There are two methods that can be used to access the PILER data: (1) via Excel
spreadsheets, (2) via the R data structure that ships with the package.

The Excel spreadsheets for the latest release can be found on the GitHub
[Releases](https://github.com/sam-crawley/PilerDB/releases/latest) page.

To access the R data structure, install and load the package (see below). The
data will be loaded as the 'piler' object. See the gen.piler.db() documentation
for details on the structure of the piler object.

## Installing the package

```
library(remotes)
install_github("sam-crawley/PilerDB")
library(PilerDB)
```

## Exploring the data

To view the PILER DB via the Shiny web interface, install this package, and run:

```
PilerDB::launchPilerDash()
```
