# Formula One Statistics Application

This is a Scala-based console application designed to provide various statistics and insights about Formula One seasons and drivers. The application processes data from a text file and allows users to display different statistics based on their input. Evaluation and Testing are located in the `documents` folder.

## Features

- View the winners and stats for each season
- Display detailed stats for a specific season
- Show the total races for each season
- Calculate and display average points per season
- List total points per season in ascending order
- Display the total points for a specific driver
- Exit the application

## Prerequisites

- Scala 2.13 or newer
- Open JDK 23.0.1 or newer
- IntelliJ IDEA 2024.3.1 (or any other compatible IDE)


## Project Structure

- `ConsoleApp.scala`: Main application file containing logic to read data, process user input, and display statistics.
- `data.txt`: Text file that contains the Formula One statistics.

## Getting Started

1. Clone the repository:
    ```sh
    git clone https://github.com/vrusso300/ProgPara_CW1
    ```

2. Open the project in IntelliJ IDEA or your preferred IDE.

3. Ensure that the `data.txt` file is located in the `src/resources/` directory.

4. Run the `ConsoleApp.scala` file.

5. Follow the on-screen prompts to interact with the application.

## Data File Format

The `data.txt` file should contain data in the following format:

year, driver1: points wins, driver2: points wins, etc...

Example:

2023, Max Verstappen: 575 19, Sergio Perez: 285 2, etc...

## Menu Options

1. **Display winners and stats for each season**: Shows the winner, points, and wins for each season.
2. **Display stats for a specific season**: Prompts the user to enter a season and displays the stats for that season.
3. **Display total races per season**: Shows the total number of races in each season.
4. **Display average points per season**: Shows the average number of points for each season.
5. **Display total points per season in ascending order**: Lists the total points for each season, sorted in ascending order.
6. **Display a specific driver's total points**: Prompts for a driver's name and displays their total points.
7. **Quit**: Exits the application.
