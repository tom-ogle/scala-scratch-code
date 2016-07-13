val transformOne: PartialFunction[Int, String] = {
  case 1 => "1"
}
val transformFour: PartialFunction[Int, String] = {
  case 4 => "4"
}
val transformOneAgain: PartialFunction[Int, String] = {
  case 1 => "1"
}

val ints = List(1,2,3,4,5)

// Combine, applying the first partial function which matches
val onesAndFours = ints flatMap { (transformOne orElse transformFour orElse transformOneAgain).lift(_) }
val onesAndFours2 = ints collect (transformOne orElse transformFour orElse transformOneAgain)

// Combine, applying all that match
val onesAndFoursAllApplied = ints flatMap { thisInt =>
  Seq(transformOne, transformFour, transformOneAgain) flatMap(_.lift(thisInt))
}
