
import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Using
import scala.util.Try

// Singleton object
object ConsoleApp extends App {

  // *******************************************************************************************************************
  // MAIN APPLICATION LOGIC & GLOBAL VARIABLES/FUNCTIONS
  // *******************************************************************************************************************

  // Path to the data file
  private val dataFilePath = "./src/resources/data.txt"

  // Read data from file
  private val mapData: Map[Int, List[(String, Float, Int)]] = readFile(dataFilePath)

  // Define new menu options as a map of actions
  private val actionMap: Map[Int, () => Boolean] = Map(
    1 -> handleDisplayWinners,
    2 -> handleDisplaySelection,
    3 -> handleDisplayWins,
    4 -> handleDisplayAvg,
    5 -> handleDisplayPointsAscending,
    6 -> handleQuit
  )

  // More dynamic menu option if needed/added in the future
  private val expectedOptions = List.range(1, actionMap.keys.max + 1)

  // Get seasons only in ascending order
  private val seasonList: List[Int] = mapData.keys.toList.sorted

  // Private sum list lambda function
  private val sumFunction: (BigDecimal, BigDecimal) => BigDecimal = (x: BigDecimal, y: BigDecimal) => x + y

  // Start the application by entering the menu loop
  menuLoop()

  // *******************************************************************************************************************
  // MENU HANDLING FUNCTIONS
  // *******************************************************************************************************************

  private def menuLoop(): Unit = {

    // Tail rec loop to keep the application running
    @tailrec
    def loop(): Unit = {
      val input = displayMenuAndReadOption()
      handleMenuInput(input) match {
        case Right(option) =>
          if (processMenuOption(option)) loop()
        case Left(error) =>
          println(error)
          loop()
      }
    }

    loop() // Start loop
  }

  // Displays the menu and reads user input
  // returns the menu option selected by the user
  private def displayMenuAndReadOption(): String = {
    println(
      """|Please select one of the following:
         |  1 - Display winners and stats from each season
         |  2 - Display a specific season's stats
         |  3 - Display total wins per season
         |  4 - Display average points per season1
         |  5 - Display total points per season ascending
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

  private def handleDisplaySelection(): Boolean = {
    println("Option 2 selected...")
    displaySelectedSeason(getSelectedSeason, mapData)
    true
  }

  private def handleDisplayWins(): Boolean = {
    println("Option 3 selected...")
    displayWins(getTotalWins, mapData)
    true
  }

  private def handleDisplayAvg(): Boolean = {
    println("Option 4 selected...")
    displayAverageWins(getAvgWins, mapData)
    true
  }

  private def handleDisplayPointsAscending(): Boolean = {
    println("Option 5 selected...")
    displayPointsAscending(getPointsAscending, mapData)
    true
  }



  // Handles the action for quitting the application
  private def handleQuit(): Boolean = {
    println("Option 7 selected...")
    println("Quitting the application...")
    false // Return false to exit the application loop
  }

  // *******************************************************************************************************************
  // USER INTERACTION FUNCTIONS
  // *******************************************************************************************************************

  // Frontend higher-order function to display all winners
  private def displayWinners(getStats: Map[Int, List[(String, Float, Int)]] => Map[Int, (String, Float, Int)], data: Map[Int, List[(String, Float, Int)]]): Unit = {
    // Invoke the backend function to get the processed data
    val stats = getStats(data)
    // Display the results (driver name, points, wins, for each season)
    stats.foreach { case (season, (driver, points, wins)) =>
      println(s"Season $season: Driver: $driver, Points: $points, Wins: $wins")
    }
  }

  // Frontend higher-order function to display total wins
  private def displayWins(getWins: Map[Int, List[(String, Float, Int)]] => Map[Int, Int], data: Map[Int, List[(String, Float, Int)]]): Unit = {
    val wins = getWins(data)
    // Display the results
    wins.foreach { case (season, totalWins) =>
      println(s"Season $season: Total Wins: $totalWins")
    }
  }


  // Frontend higher-order function to display selected season
  private def displaySelectedSeason(getSelectedSeason:(Map[Int, List[(String, Float, Int)]], Int) => Map[Int, List[(String, Float, Int)]], data: Map[Int, List[(String, Float, Int)]]): Unit = {
    println("Enter the season you want to display:")
    val selectedSeason = readLine()
    handleSeasonInput(selectedSeason) match {
      case Right(_) =>
        val seasonDrivers = getSelectedSeason(data, selectedSeason.toInt) // passes two arguments to getSelectedSeason (seen in signature in parameters)
        println(s"Season: $selectedSeason:")
        seasonDrivers.foreach { case (_, drivers) =>
          drivers.foreach { case (driver, points, wins) =>
            println(s"Driver: $driver, Points: $points, Wins: $wins")
          }
        }
      case Left(error) =>
        println(error)
        menuLoop()
    }
  }

  private def displayAverageWins(getAvgWins: Map[Int, List[(String, Float, Int)]]=> Map[Int, BigDecimal], data: Map[Int, List[(String, Float, Int)]]): Unit= {
    val avgPoints = getAvgWins(data)
    //Display results
    avgPoints.foreach { case (season, average) =>

      println(s"Season: $season: Average Points: $average")
    }
  }
  private def displayPointsAscending(getWinsAscending: Map[Int, List[(String, Float, Int)]]=> Seq[Any], data: Map[Int, List[(String, Float, Int)]]): Unit= {
    val sumWins = getWinsAscending(data)
    //Display results
    sumWins.foreach { case (season, sum) =>
      println(s"Season: $season: Total Points: $sum")
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

  // Filter method to get selected season
  private def getSelectedSeason(data: Map[Int, List[(String, Float, Int)]], selectedSeason: Int): Map[Int, List[(String, Float, Int)]] = {
    data.filter { case (season, _) =>
      season == selectedSeason
    }

  }

  // Backend function to get total wins
  private def getTotalWins(data: Map[Int, List[(String, Float, Int)]]): Map[Int, Int] = {
    data.map { case (season, drivers) =>
      val totalRaces = drivers.foldLeft(0) { (sum, driver) => sum + driver._3 }
      // Return mapped Map
      season -> totalRaces // seasons are keys, totalRaces are values
    }

  }

  private def getAvgWins(data: Map[Int, List[(String, Float, Int)]]): Map[Int, BigDecimal] = {

    @tailrec
    def calculateAvg(drivers: List[(String, Float, Int)], total: Float, count: Int): BigDecimal = drivers match {
      case Nil => if (count == 0) 0 // Handle empty list to avoid dividing by 0 error
      else total / count
      case (_, points, _) :: tail => calculateAvg(tail, total + points, count + 1)
    }

    def processEntries(entries: List[(Int, List[(String, Float, Int)])]): Map[Int, BigDecimal] = entries match {
      case Nil => Map.empty
      case (season, drivers) :: tail =>
        val avgPoints = calculateAvg(drivers, 0, 0)
        val roundedAverage = avgPoints.setScale(2, BigDecimal.RoundingMode.CEILING)
        processEntries(tail) + (season -> roundedAverage)
    }

    processEntries(data.toList)
  }

  private def getPointsAscending(data: Map[Int, List[(String, Float, Int)]]): Seq[(Int, BigDecimal)] = {
    val sumSeasons = data.map { case (season, drivers) =>
      // Change fold right starting number to bigdecimal
      val totalPoints: BigDecimal = drivers.foldRight(BigDecimal(0)) { (driver, sum) => sumFunction(driver._2, sum) }
      season -> totalPoints
    }.toSeq.sortBy(_._2) // Sort by total points (values)
    sumSeasons // Convert back to map to match return sig
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

  // Pure function to validate and process the input string from console
  private def handleMenuInput(input: String): Either[String, Int] =
    Try(input.toInt).toOption match {
      case Some(option) if expectedOptions.contains(option) => Right(option)
      case _ => Left(s"Invalid input '$input'. Please enter a valid number from ${expectedOptions.head}-${expectedOptions.size}.")
    }

  // Pure function to validate and process the input string from console
  private def handleSeasonInput(input: String): Either[String, Int] =
    Try(input.toInt).toOption match {
      case Some(option) if seasonList.contains(option) => Right(option)
      case _ => Left(s"Invalid input '$input'. Please enter a year between ${seasonList.head}-${seasonList.last}.")
    }

}
