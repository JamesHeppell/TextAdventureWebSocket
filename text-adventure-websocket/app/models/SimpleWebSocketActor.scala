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

    var timer = new java.util.Timer()
    var taskSendingInGameTime = new java.util.TimerTask {
            def run() = {
                        sendInGameTime(inGameTime)
                        inGameTime += 1
                }
        }

    var myGame = new Game()
    var hints = List("Be careful which boxes you open",
                     "The Key is somewhere in the north",
                     "That's all the hints for now",
                     "okay if you want the solution one more time",
                     "n-c-e-n-o-c-c-c-c-c",
                     "That was the last hint!")
    var hintNumber:Int = 0

    // this is where we receive json messages sent by the client
    // and send them a json reply
    def receive = {
        case jsValue: JsValue =>
            logger.info(s"JS-VALUE: $jsValue")
            
            val clientMessage = getMessage(jsValue)       
            if (clientMessage == "start"){
                hintNumber = 0
                myGame.hasStarted = true
                startGameTimerIfNotStarted()
            }
            
            if (myGame.hasStarted){
                if (myGame.isValidAction(clientMessage)){
                    myGame.changeStateFromAction(clientMessage)
                    myGame.updateState()
                    val json: JsValue = myGame.toJson(clientMessage)
                    clientActorRef ! (json)    
                } else if(clientMessage == "h"){
                    val json: JsValue = Json.parse(s"""{"history": "(hint) ${hints(hintNumber)}"}""")
                    if(hintNumber < hints.length - 1){hintNumber += 1}
                    clientActorRef ! (json) 
                }else{
                    //todo error message to help player with bad messgaes.
                    val json: JsValue = Json.parse(s"""{"history": "'$clientMessage' is not a valid action"}""")
                    clientActorRef ! (json) 
                }
            }

            if(!myGame.hasStarted){
                timer.cancel()
                isTimerStarted = false
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
        timer = new java.util.Timer()
        taskSendingInGameTime = new java.util.TimerTask {
            def run() = {
                        sendInGameTime(inGameTime)
                        inGameTime += 1
                }
        }
        timer.schedule(taskSendingInGameTime, 1000L, 1000L)
    }

    // send the in game time to the client to update the game view
    def sendInGameTime(time:Int) = {
        val json: JsValue = Json.parse(s"""{"time": $time}""")
        clientActorRef ! (json)
    }
}