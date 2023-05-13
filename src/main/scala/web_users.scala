import java.util.Date

sealed trait Visitor {
  def id: String
  // Unique id assigned to each user
  def createdAt: Date // Date this user first visited the site
  // How long has this visitor been around?
  def age: Long = new Date().getTime - createdAt.getTime
}

final case class Anonymous(id: String, createdAt: Date = new Date()) extends Visitor
final case class User(
                 id: String,
                 email: String,
                 createdAt: Date = new Date()
               ) extends Visitor

def older(v1: Visitor, v2: Visitor): Boolean =
  v1.createdAt.before(v2.createdAt)

def missingCase(v: Visitor): String =
  v match {
    case User(_, _, _) => "Got a user"
    case Anonymous(_, _) => "Anonym user"
  }


/**
Recall the Visitor trait we looked at earlier: a website Visitor is either
Anonymous or a signed-in User. Now imagine we wanted to add the ability
to send emails to visitors. We can only email signed-in users, and sending an
email requires a lot of knowledge about SMTP settings, MIME headers, and
so on. Would an email method be be er implemented using polymorphism
on the Visitor trait or using pa ern matching in an EmailService object?

Me - On the class, because we need to add some data to it.

 Correct response - I would implement the method in an EmailService object. There are a lot of
details to do with sending an email that have nothing to do with our Visitor
class. I would rather keep these details in a separate abstract on.

 Me - If the e-mail send data is not associated with the user, if not, the class would need modification too.
*/

