import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Using
import scala.util.Try
import scala.collection.immutable.ListMap


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
    6 -> handleDisplaySelected,
    7 -> handleQuit
  )

  // Name related data and validation
  private val nameList: List[String] = mapData.flatMap { case (_, drivers) => // Flatten the map to get the values
    drivers.collect { case (name, _, _) =>
      name
    }
  }.toList.distinct // Get unique names only

  private val nameBuffer: String => String = x => {
    x.trim.indexOf(" ") match {
      case spaceIndex =>
        val firstName = x.slice(0, spaceIndex)
        val lastName = x.slice(spaceIndex + 1, x.length)
        val fullName = firstName.capitalize + " " + lastName.capitalize
        fullName
      case _ =>
        x // Return the original string if no space is found
    }
  }

  // Predicate to check if the name is in the list by comparing the names in a filter
  private val namePredicate = (x: String, y: String) => x == y
  private val errName: String = s"Please enter a valid name from the list: ${nameList.mkString(", ")}."
  private def validateName: String => Either[String, Either[Int, String]] = handleInput(nameList)(_: String)(errName) // Curried function

  // Dynamic menu options and validation
  private val expectedOptions: List[Int] = List.range(1, actionMap.keys.size + 1)
  private val errMenu: String = s"Err: Please enter a valid number between ${expectedOptions.head} and ${expectedOptions.last}."
  private def validateMenuInput: String => Either[String, Either[Int, String]] = handleInput(expectedOptions)(_: String)(errMenu) // Curried function

  // Season related data and validation
  private val seasonList: List[Int] = mapData.keys.toList.sorted
  private val errSeason: String = s"Err: Please enter a valid year between ${seasonList.head} and ${seasonList.last}."
  private def validateSeason: String => Either[String, Either[Int, String]] = handleInput(seasonList)(_: String)(errSeason) // Curried function

  // Etc lambda functions
  private val sumFloatRound = (x: Float, y: Float) => (x + y).round

  // Main application entry
  private def startApplication(): Unit = {
    println()
    println("Welcome to the formula one application!")
    menuLoop()
  }


  // Begin loop; last to be invoked to avoid forward referencing compilation errors
  startApplication()

  // *******************************************************************************************************************
  // MENU HANDLING FUNCTIONS
  // *******************************************************************************************************************

  private def menuLoop(): Unit = {
    // Tailrec loop
    @tailrec
    def loop(): Unit = {
      val input = displayMenuAndReadOption()
      validateMenuInput(input) match {
        case Right(Left(option)) =>
          if (processMenuOption(option)) loop()
        case Left(error) =>
          println(error)
          loop()
      }
    }
    // Start the recursive loop
    loop()
  }

  // Displays the menu and reads user input
  // returns the menu option selected by the user
  private def displayMenuAndReadOption(): String = {
    println(
      """|Please select one of the following:
         |  1 - Display winners and stats from each season
         |  2 - Display a specific season's stats
         |  3 - Display total wins per season
         |  4 - Display average points per season
         |  5 - Display total points per season ascending
         |  6 - Display a specific driver's total wins
         |  7 - Quit""".stripMargin)
    readLine()
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

  private def handleDisplaySelected(): Boolean = {
    println("Option 6 selected...")
    displaySelectedPoints(getSelectedPoints, mapData)
    true
  }

  // Handles the action for quitting the application
  private def handleQuit(): Boolean = {
    println("Option 7 selected...")
    println("Quitting the application...")
    false // Return false to exit the application loop
  }

  // *******************************************************************************************************************
  // USER INTERACTION FUNCTIONS (FRONTEND)
  // *******************************************************************************************************************

  // Frontend higher-order function to display all winners
  private def displayWinners(getStats: Map[Int, List[(String, Float, Int)]] => Map[Int, (String, Float, Int)], data: Map[Int, List[(String, Float, Int)]]): Unit = {
    // Invoke the backend function to get the processed data
    val stats = getStats(data)
    // Display the results (driver name, points, wins, for each season)
    stats.foreach { case (season, (driver, points, wins)) =>
      println(s"Season $season: Winner: $driver, Points: $points, Wins: $wins")
    }
  }

  // Frontend higher-order function to display total wins
  private def displayWins(getWins: Map[Int, List[(String, Float, Int)]] => Map[Int, Int], data: Map[Int, List[(String, Float, Int)]]): Unit = {
    val wins = getWins(data)
    // Display the results using a declarative approach
    wins.map { case (season, totalWins) =>
      s"Season $season: Total Wins: $totalWins"
    }.foreach(println)
  }

  private def displaySelectedSeason(getSelectedSeason: (Map[Int, List[(String, Float, Int)]], Int) => Map[Int, List[(String, Float, Int)]], data: Map[Int, List[(String, Float, Int)]]): Unit = {
    println("Please enter the season you want to display:")
    val selectedSeason = readLine()
    validateSeason(selectedSeason) match {
      case Right(Left(option)) =>
        val seasonDrivers = getSelectedSeason(data, option)
        println(s"Season: $selectedSeason:")
        seasonDrivers.foreach { case (_, drivers) =>
          drivers.foreach { case (driver, points, wins) =>
            println(s"Driver: $driver, Points: $points, Wins: $wins")
          }
        }
      case Left(error) =>
        println(error)
    }
  }

  private def displaySelectedPoints(getSelectedPoints: (Map[Int, List[(String, Float, Int)]], String) => Map[String, Float], data: Map[Int, List[(String, Float, Int)]]): Unit = {
    println("Please enter the player you want to display in the format: 'First Last':")
    val userInput = readLine()
    val expectedInput = nameBuffer(userInput)

    (namePredicate(expectedInput, userInput), validateName(userInput)) match {
      case (true, Right(Right(option))) =>
        getSelectedPoints(data, option).foreach { case (driver, points) =>
          println(s"Driver: $driver Total Points: $points ")
        }
      case (true, Left(error)) =>
        println(error)
      case (false, _) =>
        println(s"Invalid input '$userInput'. Please enter a name in the format: ${nameList.head}")
    }
  }

  private def displayAverageWins(getAvgWins: Map[Int, List[(String, Float, Int)]] => Map[Int, Float], data: Map[Int, List[(String, Float, Int)]]): Unit = {
    val avgPoints = getAvgWins(data)
    //Display results
    avgPoints.foreach { case (season, average) =>
      println(s"Season: $season: Average Points: $average")
    }
  }

  private def displayPointsAscending(getWinsAscending: Map[Int, List[(String, Float, Int)]] => Map[Int, Float], data: Map[Int, List[(String, Float, Int)]]): Unit = {
    val sumWins = getWinsAscending(data)
    //Display results
    sumWins.foreach { case (season, sum) =>
      println(s"Season: $season: Total Points: $sum")
    }
  }

  // *******************************************************************************************************************
  // DATA OPERATION FUNCTIONS (BACKEND)
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

  private def getAvgWins(data: Map[Int, List[(String, Float, Int)]]): Map[Int, Float] = {

    @tailrec
    def calculateAvg(drivers: List[(String, Float, Int)], total: Float, count: Int): Float = drivers match {
      case Nil => if (count == 0) 0 // Handle empty list to avoid dividing by 0 error
      else total / count
      case (_, points, _) :: tail => calculateAvg(tail, sumFloatRound(total, points), count + 1)
    }

    def processEntries(data: List[(Int, List[(String, Float, Int)])]): Map[Int, Float] = data match {
      case Nil =>
        Map.empty
      case (season, drivers) :: tail =>
        val avgPoints = calculateAvg(drivers, 0, 0)
        val roundedAverage = avgPoints.round
        processEntries(tail) + (season -> roundedAverage)
    }

    processEntries(data.toList)
  }

  private def getPointsAscending(data: Map[Int, List[(String, Float, Int)]]): Map[Int, Float] = {
    val sumSeasons = data.map { case (season, drivers) =>
      val totalPoints: Float = drivers.foldRight(0f) { (driver, sum) => sumFloatRound(driver._2, sum) }
      season -> totalPoints
    }

    // Sort by total points and retain the sorted order in a list map to match return signature
    ListMap(sumSeasons.toSeq.sortBy(_._2): _*)

  }

  private def getSelectedPoints(data: Map[Int, List[(String, Float, Int)]], inputName: String): Map[String, Float] = {
    val fullName = inputName
    val sumPoints = data.collect { case (_, drivers) =>
      // Filter selected driver based
      drivers.filter { case (name, _, _) => namePredicate(name, fullName) }
        // Sum selected driver
        .map { case (_, points, _) => points }.sum
    }.sum // Sum all seasons

    Map(fullName -> sumPoints)

  }

  // *******************************************************************************************************************
  // UTILITY FUNCTIONS
  // *******************************************************************************************************************

  // Curried Function to validate and process the input based on a list of expected values
  // Dynamically prints error msg based on where it was called from
  private def handleInput(validOptions: List[Any])(input: String)(contextMessage: String): Either[String, Either[Int, String]] =
    Try(input.toInt).toOption match {
      case Some(option) if validOptions.contains(option) =>
        Right(Left(option)) // Return the valid integer input
      // If parse fails (i.e., is a name and cant be an integer), check if the name is in the list
      case None if validOptions.contains(input) =>
        Right(Right(input)) // Return the valid string input
      case _ =>
        Left(s"Invalid input '$input'. $contextMessage") // Return an error message

    }

  private def readFile(fileName: String): Map[Int, List[(String, Float, Int)]] = {
    var mapData = Map[Int, List[(String, Float, Int)]]()

    try {
      // Safely manage file reading
      Using(Source.fromFile(fileName)) { bufferedSource =>
        var seasonData = List[(String, Float, Int)]()
        var currentSeason = -1

        for (line <- bufferedSource.getLines()) {
          val splitLine = line.split(",", 2).map(_.trim) // Split line into season and drivers
          val season = splitLine(0).toInt
          val allDrivers = splitLine(1).split(",").map(_.trim) // Split all drivers into individual entries

          if (season != currentSeason) {
            // Save the previous season data and reset
            if (currentSeason != -1) {
              mapData += (currentSeason -> seasonData)
            }
            // Update the current season
            currentSeason = season

            // Reset the season data
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
    mapData
  }

}
