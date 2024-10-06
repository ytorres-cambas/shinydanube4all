# [danubefishiny](https://y-torres-cambas.shinyapps.io/danubefishiny/)



![danubefishiny](/data/bitmap.png) <!-- Optional: Add a screenshot of your app -->

## Description

[danubefishiny](https://y-torres-cambas.shinyapps.io/danubefishiny/)
 is a Shiny application designed for visualizing fish species occurrences in the Danube River Basin. This app allows users to filter species, view occurrences on a map, and generate reports.

## Features

- Interactive map and table showing fish species occurrences.
- Filter options for species, year, locality, country and dataset.
- Dynamic visualizations such as pie charts and bar charts to represent data.

## Installation

To run this app locally, you need to have R and the required packages installed. Follow the steps below:

1. Clone this repository:
   ```bash
   git clone https://github.com/ytorres-cambas/shinydanube4all.git

2. Navigate to the project directory:
    ```bash
    cd your-repo-name

3. Open R and install the required packages:
   ```bash
   R
   install.packages(c("shiny", "leaflet", "plotly", "dplyr", "sf", "bsicons"))

4. Run the app:
   ```bash
   R
   library(shiny)
   runApp("path/to/the/danubefishiny.R")  # Replace with the path to the app file

## Usage

Once the app is running, you can:

    1. Select species and filter the data using the sidebar.
    
    2. View the filtered occurrences on the map.
    
    3. Generate various plots based on the selected data.
    
## Contributing

If you would like to contribute to this project, please fork the repository and submit a pull request. For any issues or suggestions, feel free to create an issue in the repository.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgements

Funding was provided by the European Union’s Horizon Europe research and innovation programme through the project DANUBE4all (grant agreement no. 101093985).
