package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import akka.actor.ActorSystem
import play.api.libs.streams.ActorFlow
import akka.stream.Materializer
import play.api.libs.json._
import models.SimpleWebSocketActor

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject() (cc: ControllerComponents) (implicit system: ActorSystem)
extends AbstractController(cc) {

  val logger = play.api.Logger(getClass)
  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def index() = Action { implicit request: Request[AnyContent] =>
    logger.info("index page was called")
    Ok(views.html.index())
  }

  // the WebSocket
  def ws = WebSocket.accept[JsValue, JsValue] { requestHeader =>
      ActorFlow.actorRef { actorRef =>
          SimpleWebSocketActor.props(actorRef)
      }
  }
}
