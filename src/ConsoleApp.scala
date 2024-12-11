// Import libraries

import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn.readLine
import scala.collection.immutable.ListMap
import scala.util.{Using, Try, Success, Failure}

// Singleton object
object ConsoleApp extends App {

  // *******************************************************************************************************************
  // MAIN APPLICATION LOGIC & GLOBAL VARIABLES/FUNCTIONS
  // *******************************************************************************************************************

  // Path to the data file
  private val dataFilePath = "./src/resources/data.txt"

  // Read data from file via readFile utility function and store in map
  private val mapData: Map[Int, List[(String, Float, Int)]] = readFile(dataFilePath) match {
    // Pattern match success or failure
    case Right(data) => data
    case Left(error) =>
      println(s"Error reading data file: $error")
      // Exit the application if the file reading fails
      sys.exit(1)
  }

  // Define new menu options as a map of actions
  private val actionMap: Map[Int, () => Boolean] = Map( // Map of menu options to lambda functions
    1 -> handleDisplayWinners,
    2 -> handleDisplaySelection,
    3 -> handleDisplayRaces,
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
  private val errName: String = s"The name wasn't found. Please enter a valid name from the list: ${nameList.mkString(", ")}."

  // Dynamic menu options and validation
  private val expectedOptions: List[Int] = List.range(1, actionMap.keys.size + 1)
  private val errMenu: String = s"Please enter a valid number between ${expectedOptions.head} and ${expectedOptions.last}."

  // Season related data and validation
  private val seasonList: List[Int] = mapData.keys.toList.sorted
  private val errSeason: String = s"Please enter a valid number between ${seasonList.head} and ${seasonList.last}. "

  // partially applied curried functions to validate the user input based on the expected options
  private def validateName: String => Either[String, Either[Int, String]] = handleInput(nameList)(_)(errName)

  private def validateSeason: String => Either[String, Either[Int, String]] = handleInput(seasonList)(_)(errSeason)

  private def validateMenuInput: String => Either[String, Either[Int, String]] = handleInput(expectedOptions)(_)(errMenu)

  // Main application entry
  private def startApplication(): Unit = {
    println("\nWelcome to the formula one application!")
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
      // Display the menu and read the user input
      val input = displayMenuAndReadOption()
      // Pattern match the validation result, returns as int if valid
      validateMenuInput(input) match {
        case Right(Left(option)) =>
          // If the input is valid, process the menu option and loop
          if (processMenuOption(option)) loop()
        // If invalid, display the error message and loop for another input
        case Left(error) =>
          println(error)
          loop()
      }
    }
    // Start the tailrecursive loop
    loop()
  }

  // Displays the menu and reads user input
  // returns the menu option selected by the user
  private def displayMenuAndReadOption(): String = {
    println(
      """|Please select one of the following:
         |  1 - Display winners and stats from each season
         |  2 - Display a specific season's stats
         |  3 - Display total races per season
         |  4 - Display average points per season
         |  5 - Display total points per season ascending
         |  6 - Display a specific driver's total points
         |  7 - Quit""".stripMargin)
    readLine() // Read the user input as string
  }

  // Processes the menu option selected by the user
  private def processMenuOption(option: Int): Boolean = {
    actionMap.get(option) match {
      // Invoke the corresponding action
      case Some(action) => action()
      case None =>
        println("Invalid option. Please try again.")
        true
    }
  }

  // *******************************************************************************************************************
  // MENU ACTION HANDLERS
  // *******************************************************************************************************************

  // Handles the action for displaying winners
  private def handleDisplayWinners(): Boolean = {
    println("Option 1 selected...")
    displayWinners(getDriverStats)
    true
  }

  // Handles the action for displaying a specific season's stats
  private def handleDisplaySelection(): Boolean = {
    println("Option 2 selected...")
    displaySelectedSeason(getSelectedSeason)
    true
  }

  // Handles the action for displaying total races
  private def handleDisplayRaces(): Boolean = {
    println("Option 3 selected...")
    displayRaces(getTotalRaces)
    true
  }

  // Handles the action for displaying average points
  private def handleDisplayAvg(): Boolean = {
    println("Option 4 selected...")
    displayAvgPoints(getAvgPoints)
    true
  }

  // Handles the action for displaying total points in ascending order
  private def handleDisplayPointsAscending(): Boolean = {
    println("Option 5 selected...")
    displayPointsAscending(getPointsAscending)
    true
  }


  // Handles the action for displaying a specific driver's total points
  private def handleDisplaySelected(): Boolean = {
    println("Option 6 selected...")
    displaySelectedPoints(getSelectedPoints)
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
  private def displayWinners(getStats: Map[Int, List[(String, Float, Int)]] => Map[Int, (String, Float, Int)]): Unit = {
    // Invoke the backend function to get the processed data
    val stats = getStats(mapData)
    // Display the results (driver name, points, wins, for each season)
    stats.foreach { case (season, (driver, points, wins)) =>
      println(s"Season $season: Winner: $driver, Points: $points, Wins: $wins")
    }
  }

  // Frontend higher-order function to display a specific season's stats
  private def displaySelectedSeason(getSelectedSeason: (Map[Int, List[(String, Float, Int)]], Int) => Map[Int, List[(String, Float, Int)]]): Unit = {
    println("Please enter the season you want to display:")
    // Get the user input
    val selectedSeason = readLine()
    // Validate the user input, and display the results
    validateSeason(selectedSeason) match {
      case Right(Left(option)) =>
        // Use the backend function to get the selected season data
        val seasonDrivers = getSelectedSeason(mapData, option)
        println(s"Season: $selectedSeason:")
        seasonDrivers.foreach { case (_, drivers) =>
          drivers.foreach { case (driver, points, wins) =>
            println(s"Driver: $driver, Points: $points, Wins: $wins")
          }
        }
      // Display the error message if the input is invalid
      case Left(error) =>
        println(error)
    }
  }

  // Frontend higher-order function to display total races
  private def displayRaces(getWins: Map[Int, List[(String, Float, Int)]] => Map[Int, Int]): Unit = {
    val wins = getWins(mapData)
    // Display the results using a declarative approach
    wins.map { case (season, totalRaces) =>
      s"Season $season: Total Races: $totalRaces"
    }.foreach(println)
  }

  // Frontend higher-order function to display average points
  private def displayAvgPoints(getAvgPoints: Map[Int, List[(String, Float, Int)]] => Map[Int, Float]): Unit = {
    val avgPoints = getAvgPoints(mapData)
    //Display results
    avgPoints.foreach { case (season, average) =>
      println(f"Season: $season: Average Points: $average%.2f")
    }
  }

  // Frontend higher-order function to display total points in ascending order
  private def displayPointsAscending(getWinsAscending: Map[Int, List[(String, Float, Int)]] => Map[Int, Float]): Unit = {
    val sumWins = getWinsAscending(mapData)
    //Display results
    sumWins.foreach { case (season, sum) =>
      println(s"Season: $season: Total Points: $sum")
    }
  }

  // Frontend function to display selected driver's total points
  private def displaySelectedPoints(getSelectedPoints: (Map[Int, List[(String, Float, Int)]], String) => Map[String, Float]): Unit = {
    // Get the user input
    println("Please enter the player you want to display:")

    // Standardise the input
    val userInput = readLine().split("\\s+").mkString(" ").trim

    // Validate the name§
    validateName(userInput) match {
      case Right(Right(driver)) =>
        // Find the actual name
        val actualName = nameList.find(_.equalsIgnoreCase(driver)).get

        // Display points for the selected driver
        getSelectedPoints(mapData, driver).foreach { case (_, points) =>
          println(s"Driver: $actualName \nTotal Points: $points")
        }

      // Show validation error
      case Left(error) => println(error)
      // If validation fails unexpectedly
      case _ => println(s"Invalid input '$userInput'. Please enter a valid name.")
    }
  }
  // *******************************************************************************************************************
  // DATA OPERATION FUNCTIONS (BACKEND)
  // *******************************************************************************************************************

  // Backend function that extracts and processes the data
  private def getDriverStats(data: Map[Int, List[(String, Float, Int)]]): Map[Int, (String, Float, Int)] = {
    val driverStats = data.map { case (season, drivers) =>
      val bestDriver = drivers.maxBy(_._2)
      season -> (bestDriver._1, bestDriver._2, bestDriver._3)
    }

    // Return the driver stats
    driverStats
  }

  // Filter method to get selected season
  private def getSelectedSeason(data: Map[Int, List[(String, Float, Int)]], selectedSeason: Int): Map[Int, List[(String, Float, Int)]] = {
    val seasonStats = data.filter { case (season, _) =>
      season == selectedSeason
    }

    // Return the selected season
    seasonStats
  }

  // Backend function to get total wins
  private def getTotalRaces(data: Map[Int, List[(String, Float, Int)]]): Map[Int, Int] = {
    val totalWins = data.map { case (season, drivers) =>
      val totalRaces = drivers.foldLeft(0) { (sum, driver) => sum + driver._3 }

      // seasons are keys, totalRaces are values
      season -> totalRaces
    }

    // Return the total wins for seasons
    totalWins
  }

  // Backend function to get average points
  private def getAvgPoints(data: Map[Int, List[(String, Float, Int)]]): Map[Int, Float] = {

    // Calculate the average points for each season in a tail-recursive func
    @tailrec
    def calculateAvg(drivers: List[(String, Float, Int)], total: Float, count: Int): Float = drivers match {
      // Base case, if the list is empty, return the average
      case Nil => if (count == 0) 0 else total / count
      // Recursive case, sum the points and increment the count
      case (_, points, _) :: tail => calculateAvg(tail, total + points, count + 1)
    }

    // Map the seasons to the average points
    val result: Map[Int, Float] = data.map { case (season, drivers) =>
      // Calculate the average points for each season
      val avgPoints = calculateAvg(drivers, 0, 0)
      // Round the points up to 2 decimal places using big decimal, then convert to float
      val roundedAverage = BigDecimal(avgPoints).setScale(2, BigDecimal.RoundingMode.HALF_UP).toFloat
      season -> roundedAverage
    }

    // Return the average points for seasons
    result
  }

  private def getPointsAscending(data: Map[Int, List[(String, Float, Int)]]): Map[Int, Float] = {
    // Calculate total points for each season
    val sumSeasons: Map[Int, Float] = data.map { case (season, drivers) =>
      val totalPoints = drivers.foldRight(0f)((driver, sum) => driver._2 + sum)
      season -> totalPoints
    }

    // Sort the results by total points in ascending order, return
    val sortedSeasons = ListMap(sumSeasons.toSeq.sortBy(_._2): _*)
    sortedSeasons
  }

  // Backend function to get selected driver's total points
  private def getSelectedPoints(data: Map[Int, List[(String, Float, Int)]], inputName: String): Map[String, Float] = {
    val fullName = inputName
    val sumPoints = data.collect { case (_, drivers) =>
      // Filter based on simple function to check if the name matches
      drivers.filter { case (name, _, _) => name.equalsIgnoreCase(fullName) }
        // Sum selected driver
        .map { case (_, points, _) => points }.sum
      // Sum all seasons
    }.sum

    // Return the selected driver's total points as a Map
    Map(fullName -> sumPoints)
  }

  // *******************************************************************************************************************
  // VALIDATION & FILE HANDLING FUNCTIONS
  // *******************************************************************************************************************

  // Curried Function to validate and process the input based on a list of expected values
  private def handleInput(validOptions: List[Any])(input: String)(contextMessage: String): Either[String, Either[Int, String]] =
    Try(input.toInt).toOption match {
      case Some(option) if validOptions.contains(option) =>
        // If parse is successful and the option is in the list, return the option
        Right(Left(option))

      // If parse fails (i.e., is a name and cant be an integer), check if its in the passed list
      case None if validOptions.map(_.toString.toLowerCase).contains(input.toLowerCase) =>
        Right(Right(input))
      case _ =>
        // Return the error message if the input is invalid
        Left(s"Invalid input '$input'. $contextMessage")
    }

  // Read data from file
  private def readFile(fileName: String): Either[String, Map[Int, List[(String, Float, Int)]]] = {
    // Safely manage file reading, Using contains inherent Try logic
    Using(Source.fromFile(fileName)) { bufferedSource =>
      val lines = bufferedSource.getLines().toList
      val data = lines.headOption match {
        // Handle empty file
        case None => return Left("File is empty")
        // Handle non-empty file
        case Some(_) =>
          // Use foldLeft to accumulate the result in an immutable map
          lines.foldLeft(Map[Int, List[(String, Float, Int)]]()) {
            case (mapData, line: String) =>
              val splitLine = line.split(",", 2).map(_.trim)
              // Split the line into season and drivers
              val season = splitLine(0).toInt
              // Split the drivers by comma
              val allDrivers = splitLine(1).split(",").map(_.trim)

              // Process each driver entry and create a list of tuples
              val seasonData = allDrivers.map { driverEntry =>
                // Split the driver details by colon
                val driverDetails = driverEntry.split(":").map(_.trim)
                val name = driverDetails(0)
                // Split the stats by space
                val stats = driverDetails(1).split(" ").map(_.trim)
                val points = stats(0).toFloat
                val wins = stats(1).toInt
                (name, points, wins)
              }.toList

              // Update the map with the new season data, appending to the existing list
              mapData + (season -> (mapData.getOrElse(season, List()) ++ seasonData))
          }
      }
      // Return the data as a Right (success) resul
      Right(data)
    } match {
      // Handle the Try result
      // Print success; return the data if successful
      case Success(data) => println("Data loaded successfully"); data
      // Return the error message if the file reading fails
      case Failure(exception) => Left(exception.getMessage)
    }
  }
}
