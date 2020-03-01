def reverse[T](l: List[T]): List[T] = l match {
  case Nil => Nil
  case _ => l.tail.foldLeft(List(l.head))((acc, el) => el :: acc)
}


reverse(List(3, 1, 8, 5, 2))



def foldRight[A,B](base:B,l: List[A])(f: (A,B) => B): B =
  l.reverse.foldLeft(base)((b, a) => f(a, b))


