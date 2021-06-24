import scalaz._

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

  def from(phonenumber: String) : TransformError \/ Phonenumber ={

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