import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Using
import scala.util.Try

object ConsoleApp extends App {

  // *******************************************************************************************************************
  // MAIN APPLICATION LOGIC
  // *******************************************************************************************************************

  // Read data from file
  private val mapData: Map[Int, List[(String, Float, Int)]] = readFile("./src/resources/data.txt")

  // Define new menu options as a map of actions
  private val actionMap: Map[Int, () => Boolean] = Map(
    1 -> handleDisplayWinners,
    2 -> handleDisplayWins,
    6 -> handleQuit // Option 6: Quit the application

  )
  // Start the application by entering the menu loop
  menuLoop()

  // *******************************************************************************************************************
  // MENU HANDLING FUNCTIONS
  // *******************************************************************************************************************


  // Function to process the menu, declarative style
  private def menuLoop(): Unit = {
    // Set of expected options in the application
    val expectedOptions = Set(1, 2, 3, 4, 5, 6)

    // Pure function to validate and process the input string
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
    // Start loop
    loop()
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

  private def handleDisplayWinners(): Boolean = {
    println("Option 1 selected...")
    displayWinners(getDriverStats, mapData)
    true
  }

  private def handleDisplayWins(): Boolean = {
    println("Option 2 selected...")
    displayWins(getTotalWins, mapData)
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

  // Frontend higher-order function to display all winners
  private def displayWinners(getStats: Map[Int, List[(String, Float, Int)]] => Map[Int, (String, Float, Int)], data: Map[Int, List[(String, Float, Int)]]) : Unit = {
    // Invoke the backend function to get the processed data
    val stats = getStats(data)
    // Display the results (driver name, points, wins, for each season)
    stats.foreach { case (season, (driver, points, wins)) =>
      println(s"Season $season: Driver: $driver, Points: $points, Wins: $wins")
    }
  }

  // Frontend higher-order function to display total wins
  private def displayWins(getWins: Map[Int, List[(String, Float, Int)]] => Map[Int, Int], data: Map[Int, List[(String, Float, Int)]]) : Unit = {
    val wins = getWins(data)
    // Display the results
    wins.foreach { case (season, totalWins) =>
      println(s"Season $season: Total Wins: $totalWins")
    }
  }

  // *******************************************************************************************************************
  // DATA OPERATION FUNCTIONS
  // *******************************************************************************************************************

  // Backend functionthat extracts and processes the data
  private def getDriverStats(data: Map[Int, List[(String, Float, Int)]]): Map[Int, (String, Float, Int)] = {
    data.map { case (season, drivers) =>
      val bestDriver = drivers.maxBy(_._2)
      season -> (bestDriver._1, bestDriver._2, bestDriver._3)
    }

  }

  // Backend function to get total wins
  private def getTotalWins(data: Map[Int, List[(String, Float, Int)]]): Map[Int, Int] = {
    data.map { case (season, drivers) =>
      val totalWins = drivers.map(_._3).sum
      season -> totalWins
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
