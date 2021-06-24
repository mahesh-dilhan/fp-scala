import scalaz._
import Scalaz._

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

  def toInt(s: String) : TransformError \/ Int = {
    scala.util.Try(s.toInt).toDisjunction.leftMap(e=> TransformError(e.getMessage))
  }

  def from(phonestring: String) : TransformError \/ Phonenumber ={
      phonestring match{
        case pattern(code, area, prefix, line) => {

          //(toInt(code) |@| toInt(area) |@| toInt(prefix) |@| toInt(line)) (Phonenumber)
       //   (toInt(code),toInt(area),toInt(prefix),toInt(line))(Int)
          \/-(Phonenumber(code.toInt,area.toInt,prefix.toInt, line.toInt))
        }
        case _ => -\/(TransformError(s"phonenumber isnt match ${phonestring}"))
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