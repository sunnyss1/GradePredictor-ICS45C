# Grade Predictor for ICS 45C

Grade Predictor for ICS 45C, is a Shiny web app that takes the values of project scores and midterms to predict what grade a student would get.

## Table of Contents

[Packages](#packages)

[Usage](#usage)

[Other Notes](#other-notes)

## Packages

This project uses the following R packages

- `shiny`: for web layout and interacting with R
- `ggplot2`: for plotting
- `ordinal`: for model building

## Usage

Run the file `app.R` and the web app will launch in your default browser (or inside of RStudio depending on your settings).

From there you can change the sliders to indicate scores earned along with how late it was. After inputing the values, click on calculate and it will give a plot of the probabilities of getting each grade.

## Other Notes

The raw data for the grades can be found [here](https://github.com/sunnyss1/ICS45C-data).
