object ZebraPuzzle:

   sealed trait Resident
   case object Englishman extends Resident
   case object Spaniard extends Resident
   case object Ukrainian extends Resident
   case object Norwegian extends Resident
   case object Japanese extends Resident

   case class Solution(waterDrinker: Resident, zebraOwner: Resident)

   lazy val solve: Solution =
      (for
         residents <- List(Englishman, Spaniard, Ukrainian, Norwegian, Japanese).permutations
         if residents.indexOf(Norwegian) == 0 // statement 10
         pets <- List("Dog", "Snail", "Fox", "Horse", "Zebra").permutations
         if pets.indexOf("Dog") == residents.indexOf(Spaniard) // statement 3
         hobbies <- List("Dancing", "Reading", "Painter", "Chess", "Football").permutations
         if hobbies.indexOf("Chess") == residents.indexOf(Japanese) // statement 14
            && hobbies.indexOf("Dancing") == pets.indexOf("Snail") // statement 7
            && (hobbies.indexOf("Reading") == pets.indexOf("Fox") - 1
               || hobbies.indexOf("Reading") == pets.indexOf("Fox") + 1) // statement 11
            && (hobbies.indexOf("Painter") == pets.indexOf("Horse") - 1
               || hobbies.indexOf("Painter") == pets.indexOf("Horse") + 1) // statement 12
         colors <- List("Red", "Green", "Ivory", "Yellow", "Blue").permutations
         if colors.indexOf("Blue") == 1 // statement 15
            && colors.indexOf("Red") == residents.indexOf(Englishman) // statement 2
            && colors.indexOf("Green") == colors.indexOf("Ivory") + 1 // statement 6
            && colors.indexOf("Yellow") == hobbies.indexOf("Painter") // statement 8
         beverages <- List("Coffee", "Tea", "Milk", "Orange juice", "Water").permutations
         if beverages.indexOf("Milk") == 2 // statement 9
            && beverages.indexOf("Coffee") == colors.indexOf("Green") // statement 4
            && beverages.indexOf("Tea") == residents.indexOf(Ukrainian) // Statement 5
            && beverages.indexOf("Orange juice") == hobbies.indexOf("Football") // statement 13
      yield Solution(residents(beverages.indexOf("Water")), residents(pets.indexOf("Zebra")))).next()
