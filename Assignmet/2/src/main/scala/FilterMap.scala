// Collaborator: Parm, Chakeera, Tasfia, Krittin

object FilterMap extends App {
  def map[A, B](f: A => B, xs: List[A]): List[B] = {
    def loop(F: A => B, xs: List[A], lst: List[B]): List[B] =
      xs match {
        case head :: next => loop(F, next, lst :+ F(head))
        case Nil          => lst
      }
    loop(f, xs, List())
  }

  def filter[A](p: A => Boolean, xs: List[A]): List[A] = {
    def loop(P: A => Boolean, xs: List[A], lst: List[A]): List[A] =
      xs match {
        case head :: next if P(head) => loop(P, next, lst :+ head)
        case head :: next            => loop(P, next, lst)
        case Nil                     => lst
      }
    loop(p, xs, List())
  }
}
