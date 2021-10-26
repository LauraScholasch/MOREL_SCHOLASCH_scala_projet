import scala.io.Source
import java.io.FileNotFoundException

// Le constructeur
class Mow(var x : Int, var y:Int, var or:Char, x_area : Int, y_area : Int){

  // Procedure (pas de retour indiqué) donc on indique Unit
  def moveUp(): Unit = { y = y + 1 }
  def moveDown(): Unit = { y = y - 1 }
  def moveLeft(): Unit = { x = x - 1 }
  def moveRight(): Unit = { x = x + 1 }

  def printInfo(): Unit ={
    println("x = "+ x + ", y = " + y + ", or = " + or )
  }

  // getter
  def getInfo: (Int, Int, Char) ={
    (x,y,or)
  }

  // Changement direction
  def directionManagement(action: Char, currentDirection : Char): Unit = {

    // Map pour retrouver la bonne lettre
    val orientationMap = Map(0 -> 'N', 1 -> 'E', 2 -> 'S', 3 -> 'W')

    // Liste pour mettre à jour l'orientation
    val keys = List (0, 1, 2, 3)

    // L'inverse de Map pour retrouver la bonne valeur
    val reverse = for ((i, j) <- orientationMap) yield (j, i)

    // Position initiale en fonction de ce qu'on importe : on veut le num
    val positionActuelle = reverse(currentDirection)

    // Quand on tourne à droite on fait +1 dans la liste, quand on tourne à gauche on fait +3 dans la liste
    val moving = if (action == 'D') {1} else {3}

    // Sera mis à jour à chaque instruction D/G
    val positionUpdated = keys ((positionActuelle + moving) % orientationMap.size)

    // Changer l'orientation de la tondeuse
    or = orientationMap(positionUpdated)
  }

  // Avancer
  def forward(): Unit = {
    if ((or == 'N') && (y < y_area)) {
      moveUp()
    } else if ((or == 'E') && (x < x_area)){
      moveRight()
    } else if ((or == 'S') && (y > 0)){
      moveDown()
    } else if ((or == 'W') && (x > 0)){
      moveLeft()
    } else {
      println("La position après mouvement est en dehors de la pelouse : la tondeuse ne bouge pas ")
    }
  }
}

object Run extends App {

  println("Tondeuses à gazon MowItNow")

  // On lit les variables dans le fichier, sous forme de List
  try {
    val importingTxt = Source.fromResource("scala.txt")
    val importTxt = importingTxt.getLines.toList

    // Fermeture du fichier
    importingTxt.close()

    // Import des limites du terrain

    // 5
    val areaX = importTxt.head.split(" ").toList.head
    // 5
    val areaY = importTxt.head.split(" ").toList(1)

    // On vérifie si la taille de la surface est cohérente (position en x et y supérieures à 0)
    if (areaX.toInt < 0 || areaY.toInt < 0) {
      println("Erreur : La position de la tondeuse doit être cohérente (position en x et y supérieures à 0)")
    } else {

      // La 1ère ligne définit le terrain donc on l'exclut
      val nb_tondeuses = (importTxt.length - 1) / 2

      // Ordre de passage des tondeuses
      for (t <- 1 to nb_tondeuses) {
        println("\nPassage de la tondeuse n° " + t +"\n")

        val add = t - 1

        // Ex tondeuse1 = 1
        val mowX = importTxt(t + add).split(" ").toList.head

        // Ex tondeuse 1 = 2
        val mowY = importTxt(t + add).split(" ").toList(1)

        // Ex tondeuse 1 = N
        val orientation = importTxt(t + add).split(" ").toList(2)

        // Erreur si la tondeuse n'est pas dans les limites du terrain
        if ((mowX.toInt > areaX.toInt) || (mowY.toInt > areaY.toInt)) {
          println("Erreur : La position de la tondeuse doit être dans la surface")
        } else {
          if ((mowX.toInt < 0) || (mowY.toInt < 0)) {
            println("Erreur : la position de la tondeuse ne peut pas être en négatif (ni en abscisse ni en ordonnée)")
          } else {

            // Ex tondeuse 1 = GAGAGAGAA
            val ride = importTxt(t + add + 1)

            // Lecture des instructions de la tondeuse A

            // On crée un objet Tondeuse
            val activeMow = {
              new Mow(x = mowX.toInt, y = mowY.toInt, or = orientation.charAt(0), x_area = areaX.toInt, y_area = areaY.toInt)
            }
            println("Position initiale tondeuse " + t + " : ")
            activeMow.printInfo()

            // Action selon l'input
            for (i <- ride) {

              i match {
                case 'G' | 'D' => activeMow.directionManagement(action = i, currentDirection = activeMow.or)
                case 'A' => activeMow.forward()
                case _ => println("Invalid action")
              }
            }

            println("Position finale tondeuse " + t + ":")
            activeMow.printInfo()
          } // fin de l'erreur : position de la tondeuse en négatif
        } // fin de l'erreur : position de la tondeuse en dehors de la surface
      } // fin de la boucle for qui gère les différentes tondeuses
    } // fin de l'erreur concernant la taille de la surface (supérieure à 0)
  } // fin du try
    // Gestion des exceptions
  catch {
  case fileNotFoundException: FileNotFoundException => println("Erreur : Fichier introuvable")
  case indexOutOfBoundsException: IndexOutOfBoundsException => println("Erreur : La ligne d'initialisation doit comporter 2 nombres séparés par un espace \nLes lignes de position doivent comporter 2 nombres puis une orientation séparés par un espace")
  case noSuchElementException: NoSuchElementException => println("Erreur : L'orientation de la tondeuse doit être N,E,W,S")
  case numberFormatException: NumberFormatException => println("Erreur : Les positions doivent être des nombres entiers")
  case exception: Exception => println("Erreur : Le contenu n'est pas valide")
}
}
