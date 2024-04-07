# Tribology Data Processing
Authors: Adam Brobson, Ethan Chapman, Diksha Gulati, Rachel McConaghy, Jairus Metzger
Date: December 8, 2023
Course: STA 475

# Introduction
This project aims to enhance data organization, reporting and visualization for tribological experiments collected in individual .mat files. Tribology, the study of interacting surfaces in relative motion, often involves experiments with a tribometer to identify varying wear rates over time and under different condictions for different materials and alloys. By examining variables such as material, size, and density, the focus lies on understanding how friction coefficient and wear rate are influenced. The project addresses issues with data storage and visualization, aiming to streamline processes and avoid redundant experiments by allowing all .mat formats to be collected to a new database or added to an existing databased and have results visualized in an Rshiny application.

# Methods
Overview of Application
Data initially stored in individual .mat files are processed using MATLAB and R scripts.
MATLAB scripts convert .mat files into cell formatting and then into R's native data format (.rds files).
R Shiny application is used for data visualization and exploration of the experiments in the database.

## Steps for Database Handling

### Starting a new database:

- Convert .mat files using Table2Cell MATLAB script.
- Run DataProcessing.RMD to create RDS database files.
- Run experimentSummaries.RMD to generate summary RDS file.
- Launch the application from the database folder.

### Adding to an existing database:

- Convert additional .mat files.
- Run Add2Database.RMD.
- Run experimentSummaries.RMD.
- Launch the application.

# Application Features
- Experiment Summary Explorer Table
- Provides a sortable table with key experiment variables and results.
- Allows searching within text fields.
- Experiment APS Scatter Plot Visualizations
- Explore continuous data with options to select experiments, variables, plot types, scales, colors, and download data.
- Experiment Sliding Distance Scatter Plot Visualizations
- Plot volume lost or friction coefficient against total sliding distance.
- Customize plot appearance and download data.
- Experiment Bar Chart Visualizations
- Plot coefficient of friction, Ktotal, and Kmonte variables over experiment numbers.
- Customize plot appearance and download data.

# Results
The application offers efficient data exploration and visualization tools. Users can easily identify trends and relationships within the experimental data. Additional experiments can easily be added to an existing database for efficiency.

# Discussion
Future updates could automate file handling processes for .mat to .rds conversion for increased efficiency.
A broader dataset could enhance the diversity of observations and improve visualization insights.
Consideration should be given to uncertainty calculations for more accurate data representation.


For detailed instructions on setting up and using the application, please refer to the project documentation and scripts provided.
