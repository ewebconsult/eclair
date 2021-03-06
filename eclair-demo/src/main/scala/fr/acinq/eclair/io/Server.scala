package fr.acinq.eclair.io

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.io.{IO, Tcp}
import fr.acinq.eclair.Boot

/**
 * Created by PM on 27/10/2015.
 */
class Server extends Actor with ActorLogging {

  import Tcp._
  import context.system

  IO(Tcp) ! Bind(self, new InetSocketAddress("192.168.1.43", 45000))

  def receive = {
    case b @ Bound(localAddress) =>
      log.info(s"bound on $b")

    case CommandFailed(_: Bind) => context stop self

    case c @ Connected(remote, local) =>
      log.info(s"connected to $remote")
      val connection = sender()
      val handler = context.actorOf(Props(classOf[AuthHandler], connection, Boot.blockchain, false))
      connection ! Register(handler)
  }

}

object Server extends App {
  implicit val system = ActorSystem("system")
  val server = system.actorOf(Props[Server], "server")
}

