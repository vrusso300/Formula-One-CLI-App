import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn.{readInt, readLine}
import scala.util.Using
import scala.util.Try

object ConsoleApp extends App {

  // *******************************************************************************************************************
  // MAIN APPLICATION LOGIC
  // *******************************************************************************************************************

  // Read data from file
  private val mapData: Map[Int, List[(String, Float, Int)]] = readFile("./src/resources/data.txt")

  // Define new menu options as a Map of actions
  private val actionMap: Map[Int, () => Boolean] = Map(
    1 -> handleDisplayData, // Option 1: Display winner and stats from each season
    6 -> handleQuit // Option 6: Quit the application

  )

  // Start the application by entering the menu loop
  menuLoop()

  // *******************************************************************************************************************
  // MENU HANDLING FUNCTIONS
  // *******************************************************************************************************************

  // Function to handle the menu options


  // Function to process the menu, declarative style
  private def menuLoop(): Unit = {
    // Set of expected options in the application
    val expectedOptions = Set(1, 2, 3, 4, 5, 6)

    // Pure function to validate and process the input
    def handleInput(input: String): Either[String, Option[Int]] =
      Try(input.toInt).toOption match {
        case Some(option) if expectedOptions.contains(option) =>
          if (option == 6) Right(None) // Signal to exit
          else Right(Some(option)) // Valid option
        case _ => Left("Invalid input. Please enter a valid number.")
      }

    // Tail-recursive menu loop
    @tailrec
    def loop(): Unit = {
      // Collect user input and process it
      val input = displayMenuAndReadOption()
      handleInput(input) match {
        case Right(None) => println("Exiting the program.") // Exit case
        case Right(Some(option)) =>
          if (processMenuOption(option)) loop() // Process option and continue
          else println("Exiting due to action result.")
        case Left(error) =>
          println(error) // Display error and retry
          loop()
      }
    }

    loop() // Start the loop
  }

  // Displays the menu and reads user input
  // returns the menu option selected by the user
  private def displayMenuAndReadOption(): String = {
    println(
      """|Please select one of the following:
         |  1 - Display winner and stats from each season
         |  2 - Display total number of wins per season
         |  3 - Display average points per season
         |  4 - Display total points per season
         |  5 - Display a drivers stats
         |  6 - Quit""".stripMargin)
    val input = readLine()
    input

  }


  // Processes the menu option selected by the user
  private def processMenuOption(option: Int): Boolean = {

    actionMap.get(option) match {
      case Some(action) => action() // Invoke the corresponding action
      case None =>
        println("Invalid option. Please try again.")
        true
    }
  }

  // *******************************************************************************************************************
  // MENU ACTION HANDLERS
  // *******************************************************************************************************************

  private def handleDisplayData(): Boolean = {
    println("Option 1 selected...")
    displayData(mapData)
    true
  }

  // Handles the action for quitting the application
  private def handleQuit(): Boolean = {
    println("Quitting the application...")
    false // Return false to exit the application loop
  }

  // *******************************************************************************************************************
  // USER INTERACTION FUNCTIONS
  // *******************************************************************************************************************


  // Displays the data for each season
  @tailrec
  private def displayData(data: Map[Int, List[(String, Float, Int)]]): Unit = {
    // Base case (no more data to display) activates when nonempty is false

    if (data.nonEmpty) {
      println()
      // Extract the first entry from the map (season is identifier int, driver is list of tuples)
      val (season, drivers) = data.head
      println(s"Season $season:")
      // Display each driver's data
      drivers.foreach {
        case (driver, points, wins) =>
          println(s"Driver: $driver, Points: $points, Wins: $wins")
      }
      // Tail recursively call the function with the remaining data
      displayData(data.tail)
    }
  }


  // *******************************************************************************************************************
  // UTILITY FUNCTIONS
  // *******************************************************************************************************************

  private def readFile(fileName: String): Map[Int, List[(String, Float, Int)]] = {

    var mapData = Map[Int, List[(String, Float, Int)]]()

    try {
      // Safely manage file reading
      Using(Source.fromFile(fileName)) { bufferedSource =>
        var seasonData = List[(String, Float, Int)]()
        var currentSeason = -1

        for (line <- bufferedSource.getLines()) {
          println(s"Reading line: $line")
          val splitLine = line.split(",", 2).map(_.trim) // Split line into season and drivers
          val season = splitLine(0).toInt
          val allDrivers = splitLine(1).split(",").map(_.trim) // Split all drivers into individual entries

          if (season != currentSeason) {
            // Save the previous season data and reset
            if (currentSeason != -1) {
              mapData += (currentSeason -> seasonData)
            }
            currentSeason = season
            println(s"New season: $currentSeason") // Debug print
            seasonData = List()
          }

          // Process each driver entry
          for (driverEntry <- allDrivers) {
            val driverDetails = driverEntry.split(":").map(_.trim)
            val name = driverDetails(0)
            val stats = driverDetails(1).split(" ").map(_.trim)
            val points = stats(0).toFloat
            val wins = stats(1).toInt
            seasonData = seasonData :+ (name, points, wins)
          }
        }

        // Save the last season's data
        if (currentSeason != -1) {
          mapData += (currentSeason -> seasonData)
        }
      }
    } catch {
      case e: Exception =>
        println(s"Error reading file: ${e.getMessage}")
    }

    println(s"Loaded data: $mapData") // Debug print
    mapData
  }

}
