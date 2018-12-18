# openPoznan R package

Package aims to download data shared by Poznań city at the webpage: [https://egov.psnc.pl/node/29](https://egov.psnc.pl/node/29) 

The scope of data is listed below:

- adresses,
- bikes,
- cementaries,
- cesspools, 
- churches,
- council and electoral areas,
- graves,
- monuments,
- parking machines,
- paths,
- public transport,
- schools.

This package was created during internship in Department of Statistics at Poznań University of Economics and Business.

## Installation

```
devtools::install_github("lwawrowski/openPoznan")
```

## Data

In the package you will found three datasets:

- all graves in Poznań,
- localization of churches in Poznań,
- data from city bikes systems downloaded on 9.08.2018.

## Vignettes

Package contains 3-parts vignette of functions examples and 3 other vignettes devoted to bikes, graves and public transport.

## Shiny application

Shiny application to vizualize map with results obtained by this package was also written.