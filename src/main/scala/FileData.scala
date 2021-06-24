import scalaz._
import Scalaz._

import scala.util.Try

object FileData {

  def main(args: Array[String]): Unit = {
    println("fp-programmng")
  }
  println(RawData.generateRawUsers)

}

def transform(raw : RawUser) : TransformError \/ Domainuser = {
  val maybePerson : TransformError \/ Person = raw.person
  val maybePhone : TransformError \/ PhoneNumber = contact.from(raw.phone)

  (maybePerson |@| maybePhone)(Domainuser)
}

object RawData {

  def generateRawUsers : Seq[RawUser] = Iterator(
    RawUser("Mahesh Sameera", "1-098-098-3000"),
    RawUser("Vijay Surasetti", "1-098-098-3000"),
  ).toSeq
}

case class Domainuser (person: Person, phoneNumber: PhoneNumber)

case class PhoneNumber(countryCode: Int, areaCode: Int, prefix:Int, line: Int )

case class Results(successes: Int, failuers: Int)
case class TransformError(error: String)

object contact {
  private val pattern = """(\d{1})-(\d{3})-(\d{3})-(\d{4})""".r

  def toInt(s: String) : TransformError \/ Int = {
    Try(s.toInt).toDisjunction.leftMap(e=> TransformError(e.getMessage))
  }

  def from(phonestring: String) : TransformError \/ PhoneNumber ={
      phonestring match{
        case pattern(code, area, prefix, line) => {
          (toInt(code) |@| toInt(area) |@| toInt(prefix) |@| toInt(line))(PhoneNumber)
        }
        case _ => -\/(TransformError(s"phonenumber isnt match ${phonestring}"))
      }
  }
}

case class Person (firstname: String, lastname: String)

case class RawUser(
                  fullname: String,
                  phone: String
                  ) {
  lazy  val person :  TransformError \/ Person = {
    fullname.split(" ").toList match {
      case first :: last :: Nil => \/-(Person(first,last))
      case _ => -\/(TransformError("Failed to parse person"))
    }
  }
}