# AFSC RACE GAP Survey App

The RACE Survey App is a consistent, reproducible, and reliable local resource website for our boats while boats are out to sea. This repository houses the functions used to create the RACE Survey App (sub_tasks/, run.R), the html files that make up the website (docs/), as well as the many files that are embedded in those websites (files/). The files in total are too large to be housed within Github and are instead stored locally in the files/ folder. The G: drive version of the RACE Survey App (G:\RACE_Survey_App, must be connected to the VPN and have access to RACE_GF network) contains the most up to date version of this files/ folder. When first cloning/forking this repository, make sure to copy the files/ folder from the G drive version of the app to your copy of the repository. If you are working on this app, you should periodically copy the files/ folder from the G drive version of the app to your copy of the repository so that you are up to date with the files/ content. 

This [googlesheet](https://docs.google.com/spreadsheets/d/1AIQ0JEUA20D-g32uRQfRMZb0wW4SXl2n8Lwb_62uW-o/edit#gid=0) currently houses most of the input data used to populate the RACE Survey App. 

The run.R script is the main engine that produces the application. It sources a sequence of scripts that are stored in the sub_tasks/ folder. These scripts are numbered in the order they are called. The general flow of run.R consists of :

* sub_tasks/01_import_R_packages.R: imports/installs R packages
* sub_tasks/02_functions.R: imports helper functions
* sub_tasks/03_data.R: imports and cleans data
* In between these steps, we create the df that defines the website page structure
* sub_tasks/04_render_main_page.R: renders our main page called index.html
* In between these steps, each page is rendered
* sub_tasks/05_render_species_pages.R: renders the species ID pages

See the [wiki page](https://github.com/afsc-gap-products/RACE_Survey_App/wiki) on how to modify the data inputs. 

> Code is always in development

> This document is for informational purposes only and does not necessarily represent the views or official position of the Department of Commerce, the National Oceanic and Atmospheric Administration, or the National Marine Fisheries Service. Not to be cited without permission from the authors.

## This code is primarally maintained by: 

**Zack Oyafuso** (Zack.Oyafuso AT noaa.gov; @ZOyafuso-NOAA)

**Sarah Friedman** (Sarah.Friedman AT noaa.gov; @SarahFriedman-NOAA)

**Emily Markowitz** (Emily.Markowitz AT noaa.gov; @EmilyMarkowitz-NOAA)

**Liz Dawson** (Liz.Dawson AT noaa.gov; @liz-dawson-NOAA)

Alaska Fisheries Science Center, 

National Marine Fisheries Service, 

National Oceanic and Atmospheric Administration,

Seattle, WA 98195

## NOAA README

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

## NOAA License

Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. §105). The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for Software authored in its entirety by the Department of Commerce. To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States.

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" height="75" alt="NOAA Fisheries">

[U.S. Department of Commerce](https://www.commerce.gov/) | [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) |
[NOAA Fisheries](https://www.fisheries.noaa.gov/)
