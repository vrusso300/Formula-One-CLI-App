import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn.readInt
import scala.io.Source
import scala.util.Using

object ConsoleApp extends App {

  // *******************************************************************************************************************
  // MAIN APPLICATION LOGIC
  // *******************************************************************************************************************

  // Read data from file
  private val mapData: Map[Int, List[(String, Float, Int)]] = readFile("./src/resources/data.txt")

  // Display loaded data for verification
  displayData(mapData)


  // Define new menu options as a Map of actions
  private val actionMap: Map[Int, () => Boolean] = Map(
    1 ->
      (() => {
        println("Option 1 selected")
        true
      }),
    6 -> handleQuit // Option 6: Quit the application

  )

  // Application menu loop
  private var opt: Int = 0
  do {
    opt = displayMenuAndReadOption() // Show menu and get user input
  }
  while (processMenuOption(opt)) // Process user choice until quit option


  // *******************************************************************************************************************
  // MENU HANDLING FUNCTIONS
  // *******************************************************************************************************************


  // Displays the menu and reads user input
  // returns the menu option selected by the user
  private def displayMenuAndReadOption(): Int = {
    println(
      """|Please select one of the following:
         |  1 - Display winner and stats from each season
         |  2 - Display total number of wins per season
         |  3 - Display average points per season
         |  4 - Display total points per season
         |  5 - Display a drivers stats
         |  6 - Quit""".stripMargin)
    readInt()
  }


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


  // Handles the action for quitting the application
  private def handleQuit(): Boolean =
  {
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
    if (data.nonEmpty)
    {
      // Extract the first entry from the map (season is identifier int, driver is list of tuples)
      val (season, drivers) = data.head
      println(s"Season $season:")
      drivers.foreach
      {
        case (driver, points, wins) =>
          println(s"  Driver: $driver, Points: $points, Wins: $wins")
      }
      // Tail recursively call the function with the remaining data
      displayData(data.tail)
    }
  }


  // *******************************************************************************************************************
  // UTILITY FUNCTIONS
  // *******************************************************************************************************************

  private def readFile(fileName: String): Map[Int, List[(String, Float, Int)]] =
  {

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