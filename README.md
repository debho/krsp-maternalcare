
## LAST UPDATED: 5/3/2022

## Does maternal care induce adaptive changes in offspring behavioral characteristics in wild North American red squirrels (*Tamiasciurus hudsonicus*)?

This repository contains the data and code used in all analyses for my Master's thesis written in 2022 as part of the AMDP requirements.

### data

All the raw data I used for my study are in this folder.

#### personality-master

This file contains *processed* personality data from behavioral assay until 2021.

#### AllNests

This file contains all nest attendance data transcribed from the nest sheets in the field until 2021.

#### AllNestsCENS

This file is similar to AllNests, except with additional columns (*return_lat* and *move_lat*) for censoring all latencies to 420 seconds. This means that if a mother did not return to her pups or move her pups within 7 minutes (i.e. 420 seconds), the latency was recorded as 420 seconds in these columns.

### scripts

This folder contains all the scripts I used to load in data, clean data, run my analyses, and generate my figures.

#### packages.R

This script has all the packages used in the analyses. Running this whole script first will load all the required packages.

#### dataimport.R

This script was used for accessing the KRSP database, as well as pulling tables from it to obtain additional data needed to run my analyses. You will need a KRSP database login ID and password to run this script and get access to the database.

#### personality.R

I used this script to load in raw personality data from *personality-master.csv*, clean the data, and run PCA on personality data to obtain my PC scores for OFT and MIS.

In my case, I was only using weaned juveniles (60-80 days old at the time of the behavioral assays). Juveniles with no known parturition date were omitted from this analysis.

#### nestattendance.R

This script was used to prepare nest attendance data from *AllNestCENS.csv* for analysis.

I converted the variables that indicated whether a mom returned (m_return) or moved her pups (m_move) to a binary variable. I also calculated the ages of the pups at the time of the nest intrusions.

#### survival.R

Using data from the *flastall2* table from the KRSP database, this script returned binary variables for whether a pup survived at least 60 days and at least 200 days.

#### density.R

This is a modified version of Dr. Andrew McAdam's script from the KRSP GitHub. Here, I calculated the spring grid density using population size collected during the May census of each year.

Since I did not have the grid areas for some of the grids (RR, BT, SUX), I could not get grid densities for these grids.

#### dataconsol.R

In this script, I consolidated all the data I prepared in previous scripts into a master table for analysis (*master*). Here, I also prepared the data for analysis, such as standardizing continuous variables such that mean = 0 and sd = 1, and factoring categorical variables. See comments in the script for more details.

I also extracted data from 2018-2021 (*recent4*) for analyses that involved using rattle/chickadee playbacks in the models as this method of playbacks only started in 2018.

#### analysis.R

All statistical analyses were performed using the code in this script. These analyses were mostly linear (mixed-effects) models. Detailed explanations are in the comments of the scripts.

#### plots.R

All plots were generated here.

### output

All plots generated in `plots.R` are saved in this folder.

### analysis-notes.Rmd

Rmd doc I used to track sample sizes so that I could see how many trials I lost with each additional filter added.

I also used this to plan my analyses, including what my models should look like and the data sets I would use for them.
