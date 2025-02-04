
////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it’s not necessary
// that you understand or even look at the code in this file.
//////////////////////////////////////////////////////////////

package o1.football2.gui

import o1.football2.*
import o1.gui.Dialog.*
import o1.gui.*


/** The singleton object `FootballApp` represents a simple application that a programmer
  * can use to experiment with some methods in the package `o1.football2`.
  *
  * **NOTE TO STUDENTS: In this course, you don’t need to understand how this object works
  * on the inside. It’s enough to know that you can use this file to start the program.** */
object FootballApp extends App, O1SwingDefaults:
  for
    home <- requestChoice("Home team:", ExampleLeague.Clubs,                        Centered)
    away <- requestChoice("Away team:", ExampleLeague.Clubs.filterNot( _ == home ), Centered)
  do
    val dialog = new OngoingMatchDialog(home, away) with TerminatesOnClose with O1WindowDefaults
    dialog.centerOnScreen()
    dialog.open()

