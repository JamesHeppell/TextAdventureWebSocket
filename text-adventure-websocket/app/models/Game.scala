package models

import play.api.libs.json._
import play.api.libs.json.Json

class Game {
    val logger = play.api.Logger(getClass)

  object GameStates extends Enumeration{
      type GameState = Value
      val entrance, entrance_2, openbox_1, openbox_2, north, south_2, maze_1, west_2, fight_1, east, east_2,
        east_3, north_3, east_2_key, gate_key, openbox_3, passage_1, fight_witch, openbox_4, openbox_5,
        west_3, east_4, fight_2, south_3, south_4, north_2, west_4, finish = Value
  }

  object GameActions extends Enumeration{
      type GameAction = Value
      val start, north, south, east, west, continue, openbox, undefined = Value
  }

  var character = new Character()
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
          case GameStates.openbox_1 => openbox_1()
          case GameStates.openbox_2 => openbox_2()
          case GameStates.north => north()
          case GameStates.south_2 => south_2()
          case GameStates.maze_1 => maze_1()
          case GameStates.west_2 => west_2()
          case GameStates.fight_1 => fight_1()
          case GameStates.east => east()
          case GameStates.east_2 => east_2()
          case GameStates.east_3 => east_3()
          case GameStates.north_3 => north_3()
          case GameStates.east_2_key => east_2_key()
          case GameStates.gate_key => gate_key()
          case GameStates.openbox_3 => openbox_3()
          case GameStates.passage_1 => passage_1()
          case GameStates.fight_witch => fight_witch()
          case GameStates.openbox_4 => openbox_4()
          case GameStates.openbox_5 => openbox_5()
          case GameStates.west_3 => west_3()
          case GameStates.east_4 => east_4()
          case GameStates.fight_2 => fight_2()
          case GameStates.south_3 => south_3()
          case GameStates.south_4 => south_4()
          case GameStates.north_2 => north_2()
          case GameStates.west_4 => west_4()
          case GameStates.finish => finish()
          //catch the default with variable
          case whoa => println("Unexpected GameState: " + whoa.toString)
      }
  }


  def entrance(){
      gameText = "As you climb the stairs from the lower level you open the door and enter a maze of possibilities." +
                " You are going to have to navigate carefully to make it through this one alive." +
                " You get your compass out as that will help you not get lost, you walk for a little " +
                "bit until you reach a junction, do you start by going <b>east</b> or <b>north</b>?"
      allowedActions = List(GameActions.north, GameActions.east)
      northActionLeadsTo = GameStates.north
      eastActionLeadsTo = GameStates.east

      character.reset()
  }


  def entrance_2(){
      gameText = "As you head back towards the entrace, do you want to go <b>north</b> or <b>east</b>?"
      allowedActions = List(GameActions.north, GameActions.east)
      northActionLeadsTo = GameStates.north
      eastActionLeadsTo = GameStates.east
  }

  def openbox_1(){
      gameText = "As you slowly open the box you near a hissing sounds inside, then then ouch! A snake" +
                " has bitten your hand, loose one HP. You quickly put the lid back on the box and" +
                " <b>continue</b> walking."
      allowedActions = List(GameActions.continue)
      contiuneActionLeadsTo = GameStates.maze_1

      character.decreaseHealth(1)
  }

  def openbox_2(){
      gameText = "As you slowly open the box you near a hissing sounds inside, then then ouch! A snake" +
                " has bitten your hand, loose one HP. You quickly put the lid back on the box and <b>continue</b>" +
                " walking."
      allowedActions = List(GameActions.continue)
      contiuneActionLeadsTo = GameStates.entrance_2

      character.decreaseHealth(1)
  }

  def north(){
      gameText = "As you travel along this dark passage you notice a box, do you <b>open the box</b> or <b>continue</b>?"
      allowedActions = List(GameActions.openbox, GameActions.continue)
      openboxActionLeadsTo = GameStates.openbox_1
      contiuneActionLeadsTo = GameStates.maze_1
  }

  def south_2(){
      gameText = "As you travel along this dark passage you notice a box, do you <b>open the box</b> or <b>continue</b>?"
      allowedActions = List(GameActions.openbox, GameActions.continue)
      openboxActionLeadsTo = GameStates.openbox_2
      contiuneActionLeadsTo = GameStates.entrance_2  
  }

  def maze_1(){
      gameText = "You now approach a junction, do you want to go <b>east</b>, <b>west</b> or <b>south</b>?"
      allowedActions = List(GameActions.west, GameActions.south, GameActions.east)
      westActionLeadsTo = GameStates.west_2
      southActionLeadsTo = GameStates.south_2
      eastActionLeadsTo = GameStates.east_2  
  }

  def west_2(){
      gameText = "As you travel west you hear a growling noise and approach a deadend and what looks" +
                " like a monster! <b>Continue</b>..."
      allowedActions = List(GameActions.continue)
      contiuneActionLeadsTo = GameStates.fight_1
  }

  def fight_1(){
      gameText = "As you slay the monster, you take a little damage, and then <b>Continue</b> back to the junction you came from."
      allowedActions = List(GameActions.continue)
      contiuneActionLeadsTo = GameStates.maze_1

      character.decreaseHealth(1)
  }

  def east(){
      gameText = "As you travel down this path you notice a box, do you <b>open the box</b> or <b>Continue</b>?"
      allowedActions = List(GameActions.openbox, GameActions.continue)
      openboxActionLeadsTo = GameStates.openbox_4
      contiuneActionLeadsTo = GameStates.east_4
  }

  def east_2(){
      gameText = "As you travel down this passage its leads you to another junction... you can either go <b>west</b>," +
      " <b>east</b> or <b>north</b>"
      allowedActions = List(GameActions.west, GameActions.north, GameActions.east)  
      westActionLeadsTo = GameStates.maze_1
      northActionLeadsTo = GameStates.north_3
      eastActionLeadsTo = GameStates.east_3
  }

  def east_3(){
       gameText = "You approach a dead end, back you go (<b>continue</b>)..."
       allowedActions = List(GameActions.continue)
       contiuneActionLeadsTo = GameStates.east_2
  }

  def north_3(){
      gameText = "As you travel along this dark passage you notice a box, do you <b>open the box</b> or as" +
                " its a dead end or <b>continue</b> back?"
      allowedActions = List(GameActions.openbox, GameActions.continue)
      openboxActionLeadsTo = GameStates.openbox_3
      contiuneActionLeadsTo = GameStates.east_2
  }

  def openbox_3(){
      gameText = "You slowly open the box and find a key! This will come in handy, <b>continue</b>..."
      allowedActions = List(GameActions.continue)
      contiuneActionLeadsTo = GameStates.east_2_key

      character.addKey()
  }

  def east_2_key(){
      gameText = "As you start to head back, you feel the key vibrate and you hear the walls in the" +
                " castle moving as though the floor is rearranging. Are you going to have to resolve" +
                " the maze or has it made things easier for you? <b>Continue</b> onwards..."
      allowedActions = List(GameActions.continue)
      contiuneActionLeadsTo = GameStates.gate_key
  }

  def gate_key(){
      gameText = "As you keep walking you approach a locked gate, you try the key and it fits! As you" +
                " walk on through you feel that this must lead to somewhere good (<b>continue</b>)..."
      allowedActions = List(GameActions.continue) 
      contiuneActionLeadsTo = GameStates.passage_1
      character.useKey()
  }

  def passage_1(){
      gameText = "As you keep walking hoping the end is near, you hear the voice of an evil witch!" +
                "'Ah so you made it through my little maze, I hope you didn't get bitten by my little snakes...'" +
                " <b>continue</b>"
      allowedActions = List(GameActions.continue)
      contiuneActionLeadsTo = GameStates.fight_witch
    
      character.decreaseHealth(character.maxHealth - character.health)
  }

  def fight_witch(){
      gameText = "After a long battle you slay the witch...and if by magic the exit reveals itself." +
                " <b>Continue</b> on to the exit"
      allowedActions = List(GameActions.continue)
      contiuneActionLeadsTo = GameStates.finish

      character.decreaseHealth(2)
  }

  def openbox_4(){
      gameText = "As you slowly open the box you near a hissing sounds inside, then then ouch! A snake" +
                " has bitten your hand, loose one HP. You quickly put the lid back on the box and" +
                " <b>continue</b>."
      allowedActions = List(GameActions.continue)
      contiuneActionLeadsTo = GameStates.east_4

      character.decreaseHealth(1)
  }

  def openbox_5(){
      gameText = "As you slowly open the box you near a hissing sounds inside, then then ouch!" +
                " A snake has bitten your hand, loose one HP. You quickly put the lid back on the" +
                " box and <b>continue</b>."
      allowedActions = List(GameActions.continue)
      contiuneActionLeadsTo = GameStates.entrance_2

      character.decreaseHealth(1)
  }

  def west_3(){
      gameText = "As you travel along this dark passage you notice a box, do you <b>open the box</b> or <b>continue</b>?"
      allowedActions = List(GameActions.continue, GameActions.openbox)
      contiuneActionLeadsTo = GameStates.entrance_2
      openboxActionLeadsTo = GameStates.openbox_5
  }

  def east_4(){
      gameText = "You approach another junction, do you go <b>north</b>, <b>south</b>, or <b>west</b>?"
      allowedActions = List(GameActions.west, GameActions.north, GameActions.south)  
      westActionLeadsTo = GameStates.west_3
      northActionLeadsTo = GameStates.north_2
      southActionLeadsTo = GameStates.south_3
  }

  def fight_2(){
      gameText = "As you slay the monster you <b>continue</b> back to the junction you came from."
      allowedActions = List(GameActions.continue)  
      contiuneActionLeadsTo = GameStates.east_4

      character.decreaseHealth(1)
  }

  def south_3(){
      gameText = "As you travel south you hear a growling noise and approach a dead end and what looks" +
                " like a monster! <b>Continue</b> to find out if you survive..."
      allowedActions = List(GameActions.continue)
      contiuneActionLeadsTo = GameStates.fight_2
  }

  def south_4(){
      gameText = "After a little bit of walking to reach a dead end, better start <b>continuing</b> back..."
      allowedActions = List(GameActions.continue)
      contiuneActionLeadsTo = GameStates.north_2
  }

  def north_2(){
      gameText = "Further in you go you approach another junction... <b>west</b>, <b>south</b> or <b>east</b> are your choices"
      allowedActions = List(GameActions.west, GameActions.south, GameActions.east)
      westActionLeadsTo = GameStates.west_4
      southActionLeadsTo = GameStates.south_4
      eastActionLeadsTo = GameStates.east_4
  }

  def west_4(){
      gameText = "You approach a locked gate, looks like you will need key to open this!" +
                " You might need to go searching the boxes...<b>continue</b> back"
      allowedActions = List(GameActions.continue)
      contiuneActionLeadsTo = GameStates.north_2
  }

  def finish(){
      gameText = "Congratulations, you have completed the maze and survived! <br/> Type <b>start</b>" +
                 " to try again and impove your time..."
      allowedActions = List(GameActions.start)
      hasStarted = false
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
        Json.parse(s"""{"history": "$clientMessage",
                        "adventure_text": "$gameText",
                        "characterHP":"${character.health}",
                        "items":"${character.items}"}""")
    }
}
