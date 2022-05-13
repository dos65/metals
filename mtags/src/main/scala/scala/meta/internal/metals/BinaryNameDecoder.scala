package scala.meta.internal.metals

import scala.annotation.tailrec

object BinaryNameDecoder {

  val encodingTable: Map[Char, (String, Char)] = {
    def entry(ch: Char, encoded: String): (Char, (String, Char)) =
      (encoded.head, (encoded, ch))

    Map(
      entry('~', "tilde"),
      entry('=', "eq"),
      entry('<', "less"),
      entry('>', "greater"),
      entry('!', "bang"),
      entry('#', "hash"),
      entry('%', "percent"),
      entry('^', "up"),
      entry('&', "amp"),
      entry('|', "bar"),
      entry('*', "times"),
      entry('/', "div"),
      entry('+', "plus"),
      entry('-', "minus"),
      entry(':', "colon"),
      entry('\\', "bslash"),
      entry('?', "qmark"),
      entry('@', "at")
    )
  }

  def decode(name: String): String = {
    @tailrec
    def process(input: String, idx: Int, output: StringBuilder): String = {
      if (idx >= input.length())
        output.mkString
      else {
        val ch = input.charAt(idx)
        if (ch == '$' && idx + 1 != input.length()) {
          val headCh = input.charAt(idx + 1)
          encodingTable.get(headCh) match {
            case Some((fullRepr, value))
                if idx + fullRepr.length < input.length =>
              val end = idx + fullRepr.length
              val slice = input.substring(idx + 1, end + 1)
              if (slice == fullRepr)
                process(input, end + 1, output.append(value))
              else
                process(input, idx + 2, output.append(ch).append(headCh))
            case _ =>
              process(input, idx + 1, output.append(ch))
          }
        } else
          process(input, idx + 1, output.append(ch))
      }
    }
    process(name, 0, new StringBuilder)
  }
}
