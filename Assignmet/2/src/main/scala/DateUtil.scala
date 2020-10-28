// Collaborator: Parm, Chakeera, Tasfia, Krittin

object DateUtil extends App {
  type Date = (Int, Int, Int)

  val monthMap = Map(
    (1, "January"),
    (2, "February"),
    (3, "March"),
    (4, "April"),
    (5, "May"),
    (6, "June"),
    (7, "July"),
    (8, "August"),
    (9, "September"),
    (10, "October"),
    (11, "November"),
    (12, "December")
  )

  val monthDays = Map(
    (1, 31),
    (2, 28),
    (3, 31),
    (4, 30),
    (5, 31),
    (6, 30),
    (7, 31),
    (8, 31),
    (9, 30),
    (10, 31),
    (11, 30),
    (12, 31)
  )

  val monthDaysLeap = Map(
    (1, 31),
    (2, 29),
    (3, 31),
    (4, 30),
    (5, 31),
    (6, 30),
    (7, 31),
    (8, 31),
    (9, 30),
    (10, 31),
    (11, 30),
    (12, 31)
  )

  def isOlder(a: Date, b: Date): Boolean =
    if (isReasonableDate(a) && isReasonableDate(b)) {
      if (a._3 < b._3) true
      else if (a._3 == b._3) {
        if (a._2 < b._2) true
        else if (a._2 == b._2) {
          if (a._1 < b._1) true
          else false
        } else false
      } else false
    } else throw new RuntimeException("Wrong date format!!")

  def numberInMonth(xs: List[Date], month: Int): Int = {
    def loop(xs: List[Date], month: Int, acc: Int): Int =
      xs match {
        case head :: next if !(isReasonableDate(head)) =>
          throw new RuntimeException("Wrong date format!!")
        case head :: next if (head._2 == month) =>
          loop(next, month, acc + 1)
        case head :: next =>
          loop(next, month, acc)
        case Nil => acc
      }
    if (!(month >= 1 && month <= 12)) 0
    else loop(xs, month, 0)
  }

  def numberInMonths(xs: List[Date], months: List[Int]): Int = {
    def loop(
        xs: List[Date],
        months: List[Int],
        monthToRead: List[Int],
        monthReading: Int,
        acc: Int
    ): Int =
      xs match {
        case head :: next if !(isReasonableDate(head)) =>
          throw new RuntimeException("Wrong date format!!")
        case head :: next if (head._2 == monthReading) =>
          loop(next, months, months.tail, months.head, acc + 1)
        case head :: next =>
          monthToRead match {
            case headM :: nextM =>
              loop(xs, months, nextM, headM, acc)
            case Nil =>
              loop(next, months, months.tail, months.head, acc)
          }
        case Nil => acc
      }
    if (months.isEmpty) 0
    else loop(xs, months, months.tail, months.head, 0)
  }

  def datesInMonth(xs: List[Date], month: Int): List[Date] = {
    def loop(xs: List[Date], month: Int, Xs: List[Date]): List[Date] =
      xs match {
        case head :: next if !(isReasonableDate(head)) =>
          throw new RuntimeException("Wrong date format!!")

        case head :: next if (head._2 == month) =>
          loop(next, month, Xs :+ head)
        case head :: tail =>
          loop(tail, month, Xs)
        case Nil => Xs
      }
    if (!(month >= 1 && month <= 12)) List()
    else loop(xs, month, List())
  }

  def datesInMonths(xs: List[Date], months: List[Int]): List[Date] = {
    def loop(
        xs: List[Date],
        months: List[Int],
        monthToRead: List[Int],
        monthReading: Int,
        Xs: List[Date]
    ): List[Date] =
      xs match {
        case head :: next if !(isReasonableDate(head)) =>
          throw new RuntimeException("Wrong date format!!")

        case head :: next if (head._2 == monthReading) =>
          loop(next, months, months.tail, months.head, Xs :+ head)
        case head :: tail =>
          monthToRead match {
            case headM :: nextM =>
              loop(xs, months, nextM, headM, Xs)
            case Nil =>
              loop(tail, months, months.tail, months.head, Xs)
          }
        case Nil => Xs
      }
    if (months.isEmpty) List()
    else loop(xs, months, months.tail, months.head, List())
  }

  def dateToString(d: Date): String = {
    if (!isReasonableDate(d))
      throw new RuntimeException("Wrong date format!!")
    monthMap.get(d._2).fold("")(_ + "") + "-" + d._1 + "-" + d._3
  }

  def whatMonth(n: Int, yr: Int): Int = {
    def loop(
        day: Int,
        year: Int,
        monthDay: Map[Int, Int],
        days: Int,
        month: Int
    ): Int = {
      if (day < days || month == 12) month
      else
        loop(
          day,
          year,
          monthDay,
          days + monthDay.get(month).fold(0)(_ + 0),
          month + 1
        )
    }

    if (
      (yr % 400 == 0) ||
      (yr % 4 == 0 && yr % 100 != 0) && n < 367
    ) {
      loop(n, yr, monthDaysLeap, monthDaysLeap.getOrElse(1, 31), 1)
    } else if (n < 366 && yr > 0) {
      loop(n, yr, monthDays, monthDays.getOrElse(1, 31), 1)
    } else {
      throw new RuntimeException("Wrong date format!!")
    }
  }

  def oldest(dates: List[Date]): Option[Date] = {
    def loop(dates: List[Date], max: Date): Option[Date] = {
      dates match {
        case Nil                                => Some(max)
        case head :: next if isOlder(head, max) => loop(next, head)
        case head :: next                       => loop(next, max)
      }

    }
    if (dates.isEmpty) None
    else loop(dates, dates.head)
  }

  def isReasonableDate(d: Date): Boolean = {
    if (d._1 > 31 || d._2 < 1 || d._2 > 12 || d._3 < 1) false
    else if ((d._3 % 400 == 0) || (d._3 % 4 == 0 && d._3 % 100 != 0)) {
      if (d._1 > monthDaysLeap.get(d._2).fold(30)(_ + 0)) false
      else true
    } else {
      if (d._1 > monthDays.get(d._2).fold(30)(_ + 0)) false
      else true
    }
  }
}
