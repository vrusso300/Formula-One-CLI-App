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

  // Read data from file
  private val mapData: Map[Int, List[(String, Float, Int)]] = readFile(dataFilePath) match {
    case Right(data) => data
    case Left(error) =>
      println(s"Error reading data file: $error")
      sys.exit(1)
  }

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
  private val nameBuffer: String => String = input => input.trim.split("\\s+").zipWithIndex.map { case (word, idx) =>
    val isMiddleName = idx > 0 && idx < input.split("\\s").length - 1
    if (isMiddleName && word.forall(_.isLower)) word
    else if (word.headOption.exists(_.isUpper) && word.tail.forall(_.isLower)) word
    else word.capitalize
  }.mkString(" ")
  private val namePredicate = (x: String, y: String) => x == y
  private val errName: String = s"The name wasn't found. Please enter a valid name from the list: ${nameList.mkString(", ")}."
  private def validateName: String => Either[String, Either[Int, String]] = handleInput(nameList)(_: String)(errName) // Curried function

  // Dynamic menu options and validation
  private val expectedOptions: List[Int] = List.range(1, actionMap.keys.size + 1)
  private val errMenu: String = s"Please enter a valid number between ${expectedOptions.head} and ${expectedOptions.last}."
  private def validateMenuInput: String => Either[String, Either[Int, String]] = handleInput(expectedOptions)(_: String)(errMenu) // Curried function

  // Season related data and validation
  private val seasonList: List[Int] = mapData.keys.toList.sorted
  private val errSeason: String = s"Please enter a valid number between ${expectedOptions.head} and ${expectedOptions.last}. For example, type 1 to view winners."
  private def validateSeason: String => Either[String, Either[Int, String]] = handleInput(seasonList)(_: String)(errSeason) // Curried function

  // Etc lambda functions
  private val sumFloat = (x: Float, y: Float) => x + y

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
         |  6 - Display a specific driver's total points
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
    displayAvgPoints(getAvgPoints, mapData)
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
    println("Please enter the player you want to display (case sensitive):")
    val userInput = readLine()
    val expectedInput = nameBuffer(userInput)

    (namePredicate(expectedInput, userInput), validateName(userInput)) match {
      case (true, Right(Right(option))) =>
        getSelectedPoints(data, option).foreach { case (driver, points) =>
          println(s"Driver: $driver \nTotal Points: $points ")
        }
      case (true, Left(error)) =>
        println(error)
      case (false, _) =>
        println(s"Invalid input '$userInput'. Please enter the name in the format: $expectedInput")
    }
  }

  private def displayAvgPoints(getAvgPoints: Map[Int, List[(String, Float, Int)]] => Map[Int, Float], data: Map[Int, List[(String, Float, Int)]]): Unit = {
    val avgPoints = getAvgPoints(data)
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

  // Backend function that extracts and processes the data
  private def getDriverStats(data: Map[Int, List[(String, Float, Int)]]): Map[Int, (String, Float, Int)] = {
    val driverStats = data.map { case (season, drivers) =>
      val bestDriver = drivers.maxBy(_._2)
      season -> (bestDriver._1, bestDriver._2, bestDriver._3)
    }

    driverStats
  }

  // Filter method to get selected season
  private def getSelectedSeason(data: Map[Int, List[(String, Float, Int)]], selectedSeason: Int): Map[Int, List[(String, Float, Int)]] = {
    val seasonStats = data.filter { case (season, _) =>
      season == selectedSeason
    }
    seasonStats
  }

  // Backend function to get total wins
  private def getTotalWins(data: Map[Int, List[(String, Float, Int)]]): Map[Int, Int] = {
    val totalWins = data.map { case (season, drivers) =>
      val totalRaces = drivers.foldLeft(0) { (sum, driver) => sum + driver._3 }
      // Return mapped Map
      season -> totalRaces // seasons are keys, totalRaces are values
    }

    totalWins

  }

  private def getAvgPoints(data: Map[Int, List[(String, Float, Int)]]): Map[Int, Float] = {

    @tailrec
    def calculateAvg(drivers: List[(String, Float, Int)], total: Float, count: Int): Float = drivers match {
      case Nil => if (count == 0) 0 else total / count
      case (_, points, _) :: tail => calculateAvg(tail, sumFloat(total, points), count + 1)
    }

    val result: Map[Int, Float] = data.map { case (season, drivers) =>
      val avgPoints = calculateAvg(drivers, 0, 0)
      val roundedAverage = BigDecimal(avgPoints).setScale(2, BigDecimal.RoundingMode.HALF_UP).toFloat
      season -> roundedAverage
    }
    result
  }

  private def getPointsAscending(data: Map[Int, List[(String, Float, Int)]]): Map[Int, Float] = {
    // Calculate total points for each season
    val sumSeasons: Map[Int, Float] = data.map { case (season, drivers) =>
      val totalPoints = drivers.foldRight(0f)((driver, sum) => sumFloat(driver._2, sum))
      season -> totalPoints
    }

    // Sort the results by total points in ascending order
    val sortedSeasons = ListMap(sumSeasons.toSeq.sortBy(_._2): _*)

    // Return the sorted map
    sortedSeasons
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
  // VALIDATION & FILE HANDLING FUNCTIONS
  // *******************************************************************************************************************

  // Curried Function to validate and process the input based on a list of expected values
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


  private def readFile(fileName: String): Either[String, Map[Int, List[(String, Float, Int)]]] = {
    // Safely manage file reading
    Using(Source.fromFile(fileName)) { bufferedSource =>
      val lines = bufferedSource.getLines().toList

      val data = lines.headOption match {
        case None => return Left("File is empty")
        case Some(_) =>
          // Use foldLeft to accumulate the result in an immutable map
          lines.foldLeft(Map[Int, List[(String, Float, Int)]]()) {
            case (mapData, line: String) =>
              val splitLine = line.split(",", 2).map(_.trim)
              val season = splitLine(0).toInt
              val allDrivers = splitLine(1).split(",").map(_.trim)

              // Process each driver entry and create a list of tuples
              val seasonData = allDrivers.map { driverEntry =>
                val driverDetails = driverEntry.split(":").map(_.trim)
                val name = driverDetails(0)
                val stats = driverDetails(1).split(" ").map(_.trim)
                val points = stats(0).toFloat
                val wins = stats(1).toInt
                (name, points, wins)
              }.toList

              // Update the map with the new season data, appending to the existing list
              mapData + (season -> (mapData.getOrElse(season, List()) ++ seasonData))
          }
      }
      Right(data)
    } match {
      case Success(data) => println("Data loaded successfully"); data
      case Failure(exception) => Left(exception.getMessage)
    }
  }
}
