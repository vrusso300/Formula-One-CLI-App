import scala.io.Source
import scala.io.StdIn.readInt

object ConsoleApp extends App {

  // *******************************************************************************************************************
  // MAIN APPLICATION LOGIC
  // *******************************************************************************************************************

  // Read data from file
  val mapData: Map[String, Int] = readFile("./src/data.txt")

  // Display loaded data for verification
  println(mapData)

  // Define new menu options as a Map of actions
  val actionMap: Map[Int, () => Boolean] = Map(
    1 ->
      (() => {
        println("Option 1 selected")
        true
      }),

  )

  // Application menu loop
  var opt: Int = 0
  do {
    opt = displayMenuAndReadOption() // Show menu and get user input
  }
  while (processMenuOption(opt)) // Process user choice until quit option


  // *******************************************************************************************************************
  // MENU HANDLING FUNCTIONS
  // *******************************************************************************************************************


  // Displays the menu and reads user input
  // returns the menu option selected by the user
  def displayMenuAndReadOption(): Int = {
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


  def processMenuOption(option: Int): Boolean = {
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


  // *******************************************************************************************************************
  // UTILITY FUNCTIONS
  // *******************************************************************************************************************



  import scala.util.Using
  def readFile(filename: String): Map[String, Int] = {
    val mapBuffer = scala.collection.mutable.Map[String, Int]()

    try {
      Using(Source.fromFile(filename)) { source =>
        for (line <- source.getLines()) {
          val splitline = line.split(",").map(_.trim)
          if (splitline.length >= 2) {
            try {
              mapBuffer(splitline.head) = splitline(1).toInt
            } catch {
              case _: NumberFormatException =>
                println(s"Skipping invalid numeric value in line: $line")
            }
          } else {
            println(s"Skipping malformed line: $line")
          }
        }
      }.get // Get the result or throw any errors
    } catch {
      case ex: Exception =>
        println(s"Failed to read file: ${ex.getMessage}")
        throw ex // Rethrow the exception or handle as necessary
    }
    mapBuffer.toMap
  }

}
