package actors
import akka.actor.Actor
import controllers.GameController
import de.htwg.mps.menschAergereDichNicht.actor._
import de.htwg.mps.menschAergereDichNicht.model.{Board, Dice}

final case object RequestCurrentString
final case class ReqGameController(gc: GameController)

class WuiActor extends Actor {

  var lastSender = sender
  var mayRoll = false
  var reqMove = false

  def newGame() = {
    lastSender ! NewGame
    currentString = "Started new game\nGo back to \'/game\'"

  }

  def dice(): String = {
    if (!mayRoll) {
      currentString = "It's not time to roll the dice now!\n\n" + currentString
    } else {
      val value = Dice.roll
      lastSender ! Rolled(value)
      currentString = "Rolled " + value + "!\n\n"
      currentString += boardString

      mayRoll = false
    }
    return currentString
  }

  def handleNumber(number: Int) = {
    if (noPlayers) {
      lastSender ! HumanCount(number)
      println("WUI: handle " + number)
      currentString = "Number of players: " + number + "\nGo back to \'/game\'"
      noPlayers = false
    } else if (reqMove) {
      println("WUI: reqMove")
      lastSender ! ExecuteMove(Some(number))
      reqMove = false
    } else {
      currentString += "That's not a valid input!"
    }
  }

  var gc: Option[GameController] = None
  var currentString = "Welcome"
  var noPlayers = false
  var boardString = ""

  var cantMove = false

  override def receive: Receive = {
    case ReqGameController(gc) =>
      this.gc = Some(gc)
      gc.actor = Some(this)

    case Handler(h) =>
    case RequestHumanCount(min, max) =>
      lastSender = sender
      noPlayers = true
      currentString = "Please enter number of Human Players [" + min + "-" + max + "]"

    case ShowBoard(pegs) =>
      println("WUI: ShowBoard")
      boardString = Board.toString(pegs).replace(' ', '_')
      currentString = boardString

    case RequestRollDice(player) =>
      mayRoll = true
      println("WUI: RequestRollDice")
      val tmp = currentString
      currentString = ""
      if (cantMove) {
        currentString = "Can't move any peg. Your turn ends.\n"
        cantMove = false
      }
      currentString += "Turn for " + player + "\n\n" + tmp + "\n" +
        "Enter 'd' to roll the dice"

    case Rolled(value) =>
      currentString = "Rolled " + value + "!\n\n"

    case ShowBoardWithOptions(pegs, options) =>
      currentString += Board.toStringMove(pegs, options).replace(' ', '_')

    case ShowWinScreen(winner) =>
    case RequestMovePeg(player, options) =>
      reqMove = true
      currentString += "\n" + player + "'s turn please select what to do"
      var option_string = ""
      var can_choose = false
      for ((movable, i) <- options.zipWithIndex) {
        if (movable) {
          can_choose = true
          option_string += (i + 1) + " "
        }
      }
      if (can_choose) {
        currentString += "\nPlease select one of the following numbers: " + option_string
      } else {
        cantMove = true
        sender ! ExecuteMove(None)
      }

    case EndGame =>
  }
}
