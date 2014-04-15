package hwo2014

import org.json4s._
import org.json4s.DefaultFormats
import java.net.Socket
import java.io.{BufferedReader, InputStreamReader, OutputStreamWriter, PrintWriter}
import org.json4s.native.Serialization
import scala.annotation.tailrec

object NoobBot extends App {
  args.toList match {
    case hostName :: port :: botName :: botKey :: _ =>
      new NoobBot(hostName, Integer.parseInt(port), botName, botKey)
    case _ => println("args missing")
  }
}

class NoobBot(host: String, port: Int, botName: String, botKey: String) {
  implicit val formats = new DefaultFormats{}
  val socket = new Socket(host, port)
  val writer = new PrintWriter(new OutputStreamWriter(socket.getOutputStream, "UTF8"))
  val reader = new BufferedReader(new InputStreamReader(socket.getInputStream, "UTF8"))
  send(MsgWrapper("join", Join(botName, botKey)))
  play

  @tailrec private def play {
    val line = reader.readLine()
    if (line != null) {
      Serialization.read[MsgWrapper](line) match {
        case MsgWrapper("carPositions", _) =>
        case MsgWrapper(msgType, _) =>
          println("Received: " + msgType)
      }

      send(MsgWrapper("throttle", 0.5))
      play
    }
  }

  def send(msg: MsgWrapper) {
    writer.println(Serialization.write(msg))
    writer.flush
  }

}

case class Join(name: String, key: String)
case class MsgWrapper(msgType: String, data: JValue) {
}
object MsgWrapper {
  implicit val formats = new DefaultFormats{}

  def apply(msgType: String, data: Any): MsgWrapper = {
    MsgWrapper(msgType, Extraction.decompose(data))
  }
}
