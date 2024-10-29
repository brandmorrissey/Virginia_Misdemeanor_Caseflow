# Misdemeanor Caseflow Viz Tool

This Shiny application provides an interactive tool for visualizing data concerning misdemeanor charges heard in Virginia District Courts. The data used comes from court records made publicly available at VirginiaCourtData.org.

## Uses
The application offers two primary functionalities:

1) Sankey Plot: Users can create a Sankey plot to visualize the flow of misdemeanor case processing, illustrating key stages from arrest to disposition.

2) Yearly Charges Plot: Users can generate a plot displaying the total number of misdemeanor charges filed per year from 2010 to 2023.

## Data Filtering
To enhance data exploration, users can filter the data based on:

1) Offense Category: Corresponding to headings of the Code of Virginia.
2) Crime Family: Reflecting subheadings of various offense categories.
3) Jurisdiction: Indicating the specific judicial district court where the case was charged.


## Instructions
1) Open R
2) Load Shiny library
3) Run this code: runGitHub("Virginia_Misdemeanor_Caseflow", "brandmorrissey")
