import scalaz._

import scala.util.Try

object FileData {

  def main(args: Array[String]): Unit = {
    print("fp-programmng")
  }
  println(RawData.generateRawUsers)
}

object RawData {

  def generateRawUsers : Seq[RawUser] = Iterator(
    RawUser("Mahesh", "Sameera", "1-098-098-3000") ,
    RawUser("Vijay", "Surasetti", "1-098-098-3000")
  ).toSeq
}

case class Domainuser (person: Person, phonenumber: Phonenumber)

case class Phonenumber (countryCode: Int, areaCode: Int,prefix:Int, lineNumber: Int )

case class Results(successes: Int, failuers: Int)
case class TransformError(error: String)

object Phonenumber {
  private val pattern = """(\d{1})-(\d{3})-(\d{3})-(\d{4})""".r

  private val toInt(s: String) : TransformError \/ Int = {
    Try(s.toInt).toDisjunsction.leftMap(e=> TransformError(e.getMessage))
  }

  def from(phonenumber: String) : TransformError \/ Phonenumber ={
      phonenumber.match{
        case pattern(code, area, prefix, line) =>{
          (toInt(code) |@| toInt(area) |@| toInt(prefix) |@| toInt(line) )(Phonenumber)
          Phonenumber(code.toInt, area.toInt, prefix.toInt, line.toInt)
        }
        case _ => -\/(TransformError(s"phonenumber isnt match ${phonenumber}"))
      }
  }
}

case class Person (firstname: String, lastname: String)

case class RawUser(
                  firstname: String,
                  lastname: String,
                  phone: String
                  ) {
  //lazy  val person : Person =???
}