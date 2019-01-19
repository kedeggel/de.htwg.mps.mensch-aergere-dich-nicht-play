package controllers

import actors.{ReqGameController, WuiActor}
import akka.actor.{ActorRef, ActorSystem, Props}
import de.htwg.mps.menschAergereDichNicht.actor._
import javax.inject._
import play.api.mvc._

@Singleton
class GameController @Inject()(cc: ControllerComponents)
    extends AbstractController(cc) {

  var actor: Option[WuiActor] = None

  init()

  def requestHumanCount(min: Int, max: Int) = Action {
    println("requestHumanCount")
    Ok(
      views.html
        .game("Please insert number of Human Players [" + min + "-" + max + "]")
    )
  }

  def game() = Action {
    Ok(views.html.game(actor.get.currentString))
  }

  def about = Action {
    Ok(views.html.index())
  }

  def dice() = Action {
    actor.get.dice
    Thread.sleep(100)
    Ok(views.html.game(actor.get.currentString))
  }

  def newGame = Action {
    actor.get.newGame()
    Ok(views.html.game(actor.get.currentString))
  }

  def handleNumber(number: Int) = Action {
    actor.get.handleNumber(number)
    Thread.sleep(100)

    Ok(views.html.game(actor.get.currentString))
  }

  def init() = {
    val system = ActorSystem()
    val game: ActorRef = system.actorOf(Props(classOf[Game]), "GameController")
    val wuiActorRef = system.actorOf(Props[WuiActor], "View")
    system.actorOf(Props[Tui], "ViewMain")
    wuiActorRef ! ReqGameController(this)
    game ! NewGame
  }

  def welcome() = Action {
    Ok(views.html.welcome())
  }

}
