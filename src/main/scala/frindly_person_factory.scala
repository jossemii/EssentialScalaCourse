/*
3.3.2 Exercises
3.3.2.1 Friendly Person Factory
Implement a companion object for Person containing an apply method that
accepts a whole name as a single string rather than individual ﬁrst and last
names.
 */

class Person(val firstName: String, val secondName: String, val age: Int = 0) {
  def name: String = s"$firstName $secondName"
}

object Person {
  def apply(name: String): Person = {
    val arr_name: Array[String] = name.split(" ")
    new Person(firstName = arr_name(0), secondName = arr_name(1))
  }
}