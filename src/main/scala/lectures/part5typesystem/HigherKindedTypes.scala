package lectures.part5typesystem



object HigherKindedTypes extends App {

  trait AHigherKindedType[F[_]] // The F here is higher kinded type

  //all the below generics have the same methods
  trait MyList[T] {
    def flatMap[B](f: T => B): MyList[B]
  }

  trait MyOption[T] {
    def flatMap[B](f: T => B): MyOption[B]
  }

  trait MyFuture[T] {
    def flatMap[B](f: T => B): MyFuture[B]
  }

  //combine/multiply List(1,2) * List('a', 'b', 'c') => List(1a, 1b, 1c, 2a, 2b, 2c)

  def multiple[A, B](listA: List[A], listB: List[B]): List[(A, B)] =
    for {
      a <- listA
      b <- listB
    } yield (a, b)

  def multiple[A, B](optionA: Option[A], optionB: Option[B]): Option[(A, B)] =
    for {
      a <- optionA
      b <- optionB
    } yield (a, b)

  //Higher Kinded Type Class
  object HKTTypeClass {

    trait Monad[F[_], A] {
      def flatMap[B](f: A => F[B]): F[B]
      def map[B](f: A => B): F[B]
    }

    implicit class MonadList[A](list: List[A]) extends Monad[List, A] {
      override def flatMap[B](f: A => List[B]): List[B] = list.flatMap(f)
      override def map[B](f: A => B): List[B] = list.map(f)
    }

    implicit class MonadOption[A](option: Option[A]) extends Monad[Option, A] {
      override def flatMap[B](f: A => Option[B]): Option[B] = option.flatMap(f)
      override def map[B](f: A => B): Option[B] = option.map(f)
    }

    def multiply[F[_], A, B](monadA: Monad[F, A], monadB: Monad[F, B]): F[(A, B)] =
      for {
        a <- monadA
        b <- monadB
      } yield (a, b)

  }


  import lectures.part5typesystem.HigherKindedTypes.HKTTypeClass._

  println(multiply(new MonadList(List(1,2)), new MonadList(List('a', 'b', 'c'))))
  println(multiply(new MonadOption(Some(2)), new MonadOption(Some("scala"))))

  println(multiply(Some(2), Some("scala")))

  object Scala3HKTTypeClass {

    trait Monad[F[_]] {
      extension [A](fa: F[A])
        def flatMap[B](f: A => F[B]): F[B]
        def map[B](f: A => B): F[B]
    }

    given Monad[List] with
      extension [A](list: List[A])
        def flatMap[B](f: A => List[B]): List[B] = list.flatMap(f)
        def map[B](f: A => B): List[B] = list.map(f)


    given Monad[Option] with
      extension [A](option: Option[A])
        def flatMap[B](f: A => Option[B]): Option[B] = option.flatMap(f)
        def map[B](f: A => B): Option[B] = option.map(f)


    def multiply[F[_], A, B](fa: F[A], fb: F[B])(using f: Monad[F]): F[(A, B)] =
      for {
        a <- fa
        b <- fb
      } yield (a, b)

  }

  import lectures.part5typesystem.HigherKindedTypes.Scala3HKTTypeClass.multiply
  println(multiply(List(1, 2), List("a", "b", "c")))
  println(multiply[Option, Int, String](Some(2), Some("scala")))


  object Scala3HKTTypeClassV2 {

    trait Monad[F[_]] {
        def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
        def map[A, B](fa: F[A])(f: A => B): F[B]
    }

    given Monad[List] with {
        def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list.flatMap(f)
        def map[A, B](list: List[A])(f: A => B): List[B] = list.map(f)
    }

    given Monad[Option] with {
        def flatMap[A, B](option: Option[A])(f: A => Option[B]): Option[B] = option.flatMap(f)
        def map[A, B](option: Option[A])(f: A => B): Option[B] = option.map(f)
    }

    extension [F[_], A](fa: F[A])(using monad: Monad[F]) {
      def flatMap[B](f: A => F[B]): F[B] = monad.flatMap(fa)(f)
      def map[B](f: A => B): F[B] = monad.map(fa)(f)
    }

    def multiply[F[_], A, B](fa: F[A], fb: F[B])(using f: Monad[F]): F[(A, B)] =
      for {
        a <- fa
        b <- fb
      } yield (a, b)

  }

  import lectures.part5typesystem.HigherKindedTypes.Scala3HKTTypeClassV2.multiply as mult
  println(mult(List(1, 2), List("a", "b", "c")))
  println(mult[Option, Int, String](Some(2), Some("scala")))






}
