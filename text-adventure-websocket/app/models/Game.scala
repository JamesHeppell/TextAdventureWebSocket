package models

import play.api.libs.json._
import play.api.libs.json.Json

class Game {
    val logger = play.api.Logger(getClass)

  object GameStates extends Enumeration{
      type GameState = Value
      val entrance, entrance_2, openbox_1, openbox_2, north, east, maze_1 = Value
  }

  object GameActions extends Enumeration{
      type GameAction = Value
      val start, north, south, east, west, continue, openbox, undefined = Value
  }

  var hasStarted: Boolean = false
  var allowedActions = List(GameActions.start)
  var state = GameStates.entrance
  var gameText = ""
  var northActionLeadsTo = GameStates.entrance
  var southActionLeadsTo = GameStates.entrance
  var eastActionLeadsTo = GameStates.entrance
  var westActionLeadsTo = GameStates.entrance
  var contiuneActionLeadsTo = GameStates.entrance
  var openboxActionLeadsTo = GameStates.entrance
  var characterHP: Int = 10
  var items: String = ""

  def getGameActionFromString(action: String): GameActions.Value = {
      action match{
          case "n" => GameActions.north
          case "s" => GameActions.south
          case "e" => GameActions.east
          case "w" => GameActions.west
          case "c" => GameActions.continue
          case "o" => GameActions.openbox
          case "start" => GameActions.start
          //catch the default
          case _ => GameActions.undefined
                    
      }
  }

  def updateState(){
      this.state match{
          case GameStates.entrance => entrance()
          case GameStates.entrance_2 => entrance_2()
          case GameStates.north => north()
          case GameStates.east => east()
          //catch the default with variable
          case whoa => println("Unexpected GameState: " + whoa.toString)
      }
  }


  def entrance(){
      gameText = "As you climb...."
      allowedActions = List(GameActions.north, GameActions.east)
      northActionLeadsTo = GameStates.north
      eastActionLeadsTo = GameStates.east
  }


  def entrance_2(){
      gameText = "As you head back towards the entrace, do you want to go north or east"
      allowedActions = List(GameActions.north, GameActions.east)
      northActionLeadsTo = GameStates.north
      eastActionLeadsTo = GameStates.east
  }

  def north(){
      gameText = "As you travel along this dark passage..."
      allowedActions = List(GameActions.openbox, GameActions.continue)
      openboxActionLeadsTo = GameStates.openbox_1
      contiuneActionLeadsTo = GameStates.maze_1
  }

  def east(){
      gameText = "As you travel down this path you notice a box, do you open the box or continue?"
      allowedActions = List(GameActions.openbox, GameActions.continue)
  }


  def changeStateFromAction(action: String){
      logger.info(s"client action is $action")
      action match {
          case "n" => state = northActionLeadsTo
          case "s" => state = southActionLeadsTo
          case "e" => state = eastActionLeadsTo
          case "w" => state = westActionLeadsTo
          case "c" => state = contiuneActionLeadsTo
          case "o" => state = openboxActionLeadsTo
          case "start" => state = GameStates.entrance
      }
  }

  def isValidAction(action: String): Boolean = {
      val isValid = allowedActions.contains(getGameActionFromString(action))
      logger.info(s"is valid action $isValid")
      logger.info(getGameActionFromString(action).toString())
      logger.info(allowedActions.toString())
      return isValid
  }

  def toJson(clientMessage:String): JsValue = {
        Json.parse(s"""{"history": "‘$clientMessage’",
                        "adventure_text": "'$gameText'",
                        "characterHP":"'$characterHP'",
                        "items":"'$items'"}""")
    }
}
