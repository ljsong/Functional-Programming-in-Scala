case class Book(title: String, authors: List[String])

val books: Set[Book] = Set(
  Book(title = "Structure and Interpretation of Computer Programs",
    authors = List("Abelson, Harald", "Sussman, Gerald J")),
  Book(title = "Introduction to Functional Programming",
    authors = List("Bird, Richard", "Wadler, Phil")),
  Book(title = "Effecitive Java",
    authors = List("Bloch, Joshua")),
  Book(title = "Java Puzzlers",
    authors = List("Bloch, Joshua", "Gafter, Neal")),
  Book(title = "Programming in Scala",
    authors = List("Odersky, Martin", "Spoon Lex", "Venners, Bill")),
  Book(title = "test",
    authors = List("Bloch, Joshua"))
)

for (book <- books; a <- book.authors if a startsWith "Bird")
  yield book.title

books flatMap (book => (book.authors withFilter (author => author startsWith "Bird") map (_ => book.title)))

for (book <- books if book.title.indexOf("Program") >= 0)
  yield book.title

for {
  b1 <- books
  b2 <- books
  if b1 != b2
  a1 <- b1.authors
  a2 <- b2.authors
  if a1 == a2
} yield a1