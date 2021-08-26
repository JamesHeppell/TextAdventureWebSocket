package models

import akka.actor._
import play.api.libs.json._
import play.api.libs.json.Json

object SimpleWebSocketActor {
    def props(clientActorRef: ActorRef) = Props(new SimpleWebSocketActor(clientActorRef))
}

class SimpleWebSocketActor(clientActorRef: ActorRef) extends Actor {
    val logger = play.api.Logger(getClass)

    logger.info(s"SimpleWebSocketActor class started")
    var isTimerStarted: Boolean = false
    var inGameTime: Int = 0

    var myGame = new Game()

    // this is where we receive json messages sent by the client
    // and send them a json reply
    def receive = {
        case jsValue: JsValue =>
            logger.info(s"JS-VALUE: $jsValue")
            
            val clientMessage = getMessage(jsValue)       
            if (clientMessage == "start"){
                myGame.hasStarted = true
                startGameTimerIfNotStarted()
            }
            
            if (myGame.hasStarted){
                if (myGame.isValidAction(clientMessage)){
                    myGame.changeStateFromAction(clientMessage)
                    myGame.updateState()
                    val json: JsValue = myGame.toJson(clientMessage)
                    clientActorRef ! (json)    
                }else{
                    //todo error message to help player with bad messgaes.
                    val json: JsValue = Json.parse(s"""{"history": "'$clientMessage' is not a valid action"}""")
                    clientActorRef ! (json) 
                }
            }

    }

    // parse the "message" field from the json the client sends us
    def getMessage(json: JsValue): String = (json \ "message").as[String]
    
    def startGameTimerIfNotStarted() = {
        inGameTime = 0
        if (!isTimerStarted) startInGameTimeAndMessages
        isTimerStarted = true
    }

    // start in game time and send to client
    def startInGameTimeAndMessages() = {
        val t = new java.util.Timer()
        val task = new java.util.TimerTask {
        def run() = {
                        sendInGameTime(inGameTime)
                        inGameTime += 1
                }
        }
        t.schedule(task, 1000L, 1000L)
        //task.cancel()
    }

    // send the in game time to the client to update the game view
    def sendInGameTime(time:Int) = {
        val json: JsValue = Json.parse(s"""{"time": $time}""")
        clientActorRef ! (json)
    }
}