// Collaborator: Parm, Chakeera, Tasfia, Krittin

object OptionFriends extends App {
  def lookup(xs: List[(String, String)], key: String): Option[String] =
    xs match {
      case head :: next if (head._1 == key) => Some(head._2)
      case head :: next                     => lookup(next, key)
      case Nil                              => None
    }

  def lookups[A](xs: List[(String, A)], key: String): Option[A] =
    xs match {
      case head :: next if (head._1 == key) => Some(head._2)
      case head :: next                     => lookups[A](next, key)
      case Nil                              => None
    }

  def resolve(
      userIdFromLoginName: String => Option[String],
      majorFromUserId: String => Option[String],
      divisionFromMajor: String => Option[String],
      averageScoreFromDivision: String => Option[Double],
      loginName: String
  ): Double =
    userIdFromLoginName(loginName)
      .flatMap(majorFromUserId)
      .flatMap(majorFromUserId)
      .flatMap(divisionFromMajor)
      .flatMap(averageScoreFromDivision)
      .fold(0.0)(x => x + 0)
}
