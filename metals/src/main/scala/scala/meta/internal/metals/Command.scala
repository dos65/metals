package scala.meta.internal.metals

import scala.reflect.ClassTag
import scala.util.matching.Regex

import scala.meta.internal.metals.JsonParser._
import scala.meta.internal.metals.MetalsEnrichments._

import org.eclipse.{lsp4j => l}

sealed trait BaseCommand {
  def id: String
  def title: String
  def description: String
  def arguments: String

  protected def isApplicableCommand(params: l.ExecuteCommandParams): Boolean =
    params.getCommand.stripPrefix("metals.") == id
}

case class Command(
    id: String,
    title: String,
    description: String,
    arguments: String = "`null`"
) extends BaseCommand {
  def unapply(params: l.ExecuteCommandParams): Boolean = {
    isApplicableCommand(params)
  }

  def toExecuteCommandParams(): l.ExecuteCommandParams = {
    new l.ExecuteCommandParams(
      id,
      List[Object]().asJava
    )
  }

}

case class OpenBrowserCommand(
    url: String,
    title: String,
    description: String
) extends BaseCommand {
  def id: String = s"browser-open-url:$url"
  def arguments: String = "`null`"

}

object OpenBrowserCommand {
  private val OpenBrowser: Regex = "browser-open-url:(.*)".r
  def unapply(params: l.ExecuteCommandParams): Option[String] = {
    val command = Option(params.getCommand).getOrElse("")
    command match {
      case OpenBrowser(url) => Some(url)
      case _ => None
    }
  }
}

trait ParameterCodec[A] {
  def toLspArgs(a: A): List[AnyRef]
  def decode(raw: List[AnyRef]): Option[A]
}

object ParamerCodec {

  def singleArg[A: ClassTag]: ParameterCodec[A] = {
    new ParameterCodec[A] {
      private val parser = new JsonParser.Of[A]
      override def toLspArgs(a: A): List[AnyRef] = {
        List(a.toJson)
      }
      override def decode(rawArgs: List[AnyRef]): Option[A] = {
        if (rawArgs.size != 1) None
        else {
          rawArgs(0) match {
            case parser.Jsonized(t1) =>
              Option(t1)
            case _ => None
          }
        }
      }

    }
  }

  def listArgs[A: ClassTag]: ParameterCodec[List[A]] =
    new ParameterCodec[List[A]] {
      private val parser = new JsonParser.Of[A]

      override def toLspArgs(a: List[A]): List[AnyRef] =
        a.map(_.toJson)

      override def decode(raw: List[AnyRef]): Option[List[A]] = {
        val decoded = raw.flatMap {
          case parser.Jsonized(a) => Some(a)
          case _ => None
        }
        Some(decoded)
      }
    }

  def tuple2[A: ClassTag, B: ClassTag]: ParameterCodec[(A, B)] = {
    new ParameterCodec[(A, B)] {
      private val parserA = new JsonParser.Of[A]
      private val parserB = new JsonParser.Of[B]

      override def toLspArgs(v: (A, B)): List[AnyRef] =
        List(v._1.toJson, v._2.toJson)

      override def decode(raw: List[AnyRef]): Option[(A, B)] = {
        (raw(0), raw(1)) match {
          case (parserA.Jsonized(t1), parserB.Jsonized(t2)) =>
            Option((t1, t2))
          case _ => None
        }
      }
    }
  }
}

case class ParametrizedCommand[A](
    id: String,
    title: String,
    description: String,
    arguments: String,
    codec: ParameterCodec[A]
) extends BaseCommand {

  def unapply(params: l.ExecuteCommandParams): Option[A] = {
    if (isApplicableCommand(params)) {
      Option(params.getArguments()).flatMap(jArgs =>
        codec.decode(jArgs.asScala.toList)
      )
    } else None
  }

  def toLSP(argument: A): l.Command =
    new l.Command(title, id, codec.toLspArgs(argument).asJava)

  def toExecuteCommandParams(argument: A): l.ExecuteCommandParams = {
    new l.ExecuteCommandParams(
      id,
      codec.toLspArgs(argument).asJava
    )
  }
}

object ParametrizedCommand {

  def SingleArg[A: ClassTag](
      id: String,
      title: String,
      description: String,
      arguments: String
  ): ParametrizedCommand[A] = {
    ParametrizedCommand(
      id,
      title,
      description,
      arguments,
      ParamerCodec.singleArg
    )
  }

  def DoubleArgs[A: ClassTag, B: ClassTag](
      id: String,
      title: String,
      description: String,
      arguments: String
  ): ParametrizedCommand[(A, B)] = {
    ParametrizedCommand(
      id,
      title,
      description,
      arguments,
      ParamerCodec.tuple2
    )
  }

  def ListArgs[A: ClassTag](
      id: String,
      title: String,
      description: String,
      arguments: String
  ): ParametrizedCommand[List[A]] = {
    ParametrizedCommand[List[A]](
      id,
      title,
      description,
      arguments,
      ParamerCodec.listArgs
    )
  }

}
