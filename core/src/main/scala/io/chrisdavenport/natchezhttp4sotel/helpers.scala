package io.chrisdavenport.natchezhttp4sotel

private[natchezhttp4sotel] object helpers {
  import java.io.{OutputStream, FilterOutputStream, ByteArrayOutputStream, PrintStream}

  def printStackTrace(e: Throwable): String = {
    val baos = new ByteArrayOutputStream
    val fs   = new AnsiFilterStream(baos)
    val ps   = new PrintStream(fs, true, "UTF-8")
    e.printStackTrace(ps)
    ps.close
    fs.close
    baos.close
    new String(baos.toByteArray, "UTF-8")
  }

  /** Filter ANSI codes out of an OutputStream. */
  private class AnsiFilterStream(os: OutputStream) extends FilterOutputStream(os) {
    case class State(apply: Int => State)

    val S: State = State {
      case 27 => I0
      case _  => F
    }

    val F: State = State(_ => F)

    val T: State = State(_ => T)

    val I0: State = State {
      case '[' => I1
      case _   => F
    }

    val I1: State = State {
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
              => I2
      case _   => F
    }

    val I2: State = State {
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
              => I2
      case ';' => I1
      case '@' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' |
          'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | '[' | '\\'| ']' |
          '^' | '_' | '`' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' |
          'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' | '{' |
          '|' | '}' | '~'
              => T // end of ANSI escape
      case _   => F
    }

    // Strategy is, accumulate values as long as we're in a non-terminal state, then either discard
    // them if we reach T (which means we accumulated an ANSI escape sequence) or print them out if
    // we reach F.

    private var stack: List[Int] = Nil
    private var state: State     = S // Start

    override def write(n: Int): Unit =
      state.apply(n) match {

        case F =>
          stack.foldRight(())((c, _) => super.write(c))
          super.write(n)
          stack = Nil
          state = S

        case T =>
          stack = Nil
          state = S

        case s =>
          stack = n :: stack
          state = s

      }

  }
}