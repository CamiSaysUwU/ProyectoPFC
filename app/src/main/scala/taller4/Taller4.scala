/**
  * Taller 3 - Programación Funcional
  * Autores: Camilo Valencia Romero - 2259497, Cristian Rivera Torres - 2259742
  * Profesor: Carlos A Delgado
  */
package taller4
//import common._

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer
import scala.util.Random
import common._

import scala.annotation.tailrec


//import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable.ParVector

import scala.collection.parallel._
import scala.collection.parallel.CollectionConverters._


object Taller4 {

  
// Definición de tipo para el oráculo que verifica la existencia de subcadenas.
type Oraculo = Seq[Char] => Boolean

/**
 * Genera todas las posibles subcadenas de un determinado tamaño a partir de un alfabeto.
 *
 * @param alfabeto El alfabeto de los caracteres que forman las subcadenas.
 * @param longitud El tamaño de las subcadenas.
 * @return Una secuencia con todas las subcadenas generadas.
 */
  def generarSubcadenas(caracteres: Seq[Char], longitud: Int): Seq[Seq[Char]] = {
    def subcadenasIter(subs: Seq[Seq[Char]], n: Int): Seq[Seq[Char]] = {
      if (n > longitud) subs
      else subcadenasIter(subs.flatMap(subcadena => caracteres.map(subcadena :+ _)), n + 1)
    }
    subcadenasIter(caracteres.map(Seq(_)), 1)
  }


  /**
  * Busca la primera subcadena que coincida con una cadena objetivo, usando un oráculo.
  *
  * @param cadenaObjetivo La cadena objetivo que se quiere encontrar.
  * @param oraculo El oráculo que evalúa si una subcadena está presente en la cadena objetivo.
  * @return Una opción que contiene la primera subcadena encontrada, si existe.
  */
  def buscarCadenaIngenuo(n: Int, oraculo: Oraculo): Option[Seq[Char]] = {
    // Define el alfabeto como una secuencia de los caracteres 'a', 'c', 'g' y 't'
    val alfabeto = Seq('a', 'c', 'g', 't')
    // Obtiene la longitud de la cadena objetivo
    // Genera todas las posibles subcadenas de longitud n a partir del alfabeto
    val subcadenas = generarSubcadenas(alfabeto, n-1 )
    // Busca la primera subcadena que satisfaga el oráculo
    subcadenas.find(oraculo)
  }

  /**
  * Algoritmo de búsqueda PRC (Pattern Recognition Code) Mejorado.
  *
  * @param cadenaObjetivo La cadena objetivo que se quiere encontrar.
  * @param oraculo El oráculo que evalúa si una subcadena está presente en la cadena objetivo.
  * @return Lista de subcadenas encontradas.
  */
  def buscarCadenaMejorado(n: Int, oraculo: Oraculo): Seq[Seq[Char]]  = {
    // Define el alfabeto como una secuencia de los caracteres 'a', 'c', 'g' y 't'
    val alfabeto = Seq('a', 'c', 'g', 't')
    // Obtiene la longitud de la cadena objetivo
    // Inicializa la lista de subcadenas con las de tamaño 1
    val subcadenas = alfabeto.map(Seq(_))
    // Define una función auxiliar recursiva que busca las subcadenas que satisfacen el oráculo
    def buscarSubcadenaMejoradoAux(sck: Seq[Seq[Char]],oraculo:Oraculo): Seq[Seq[Char]]  = {
      // Si la lista de subcadenas está vacía, devuelve una lista vacía
      if (sck.isEmpty) Seq(Seq()) 
      else {
        // Genera las combinaciones de las subcadenas con el alfabeto y las filtra por el oráculo
        val combinaciones = sck.flatMap(subcadena => alfabeto.map(subcadena :+ _)).filter(oraculo(_))
        // Si alguna de las combinaciones tiene el tamaño deseado, devuelve las combinaciones
        if (combinaciones.exists(_.size == n)) combinaciones
        // Si no, llama recursivamente a la función auxiliar con las combinaciones
        else buscarSubcadenaMejoradoAux(combinaciones,oraculo) 
      }
    }
    // Llama a la función auxiliar con la lista inicial de subcadenas
    buscarSubcadenaMejoradoAux(subcadenas,oraculo)
  }

/**
 * Combina dos listas de cadenas generando todas las posibles combinaciones.
 *
 * @param lista1 Primera lista de cadenas.
 * @param lista2 Segunda lista de cadenas.
 * @return Lista de todas las combinaciones posibles.
 */
def combinar(lista1: Seq[Seq[Char]], lista2: Seq[Seq[Char]]): Seq[Seq[Char]] = { 
  if (lista1.isEmpty) Nil 
  else lista2.map { cadena => cadena ++ lista1.head } ++ combinar(lista1.tail, lista2)
}

/**
 * Algoritmo de búsqueda de subcadenas rápido.
 *
 * @param n Tamaño deseado de las subcadenas a buscar.
 * @param oraculo El oráculo que evalúa si una subcadena está presente en la cadena objetivo.
 * @return Lista de subcadenas encontradas.
 */
// Cambia el nombre de la función de PRC_turbo a buscarSubcadenasRapido
def buscarCadenaTurbo(n: Int, oraculo: Oraculo): Seq[Seq[Char]] = {
  // Define el alfabeto como una secuencia de los caracteres 'a', 'c', 'g' y 't'
  val alfabeto = Seq('a', 'c', 'g', 't')
  // Inicializa la lista de subcadenas con las de tamaño 1
  val alfabetoForma = alfabeto.map(Seq(_))
  // Define una función auxiliar recursiva que busca las subcadenas que satisfacen el oráculo
  def buscarSubcadenasTurboAux(sck: Seq[Seq[Char]], n: Int,oraculo: Oraculo,cadenaVieja: Seq[Seq[Char]] = Seq(Seq())): Seq[Seq[Char]]  = {
    if (sck.head.size + 1 == n) {
      val combinaciones = combinar(sck, sck.flatten.distinct.map(Seq(_)))
      combinaciones.filter(oraculo(_))
    } else if (sck.head.size >= n) sck
    else if (sck.head.size*2 > n) {
      val combinaciones = combinar(sck, cadenaVieja).filter(oraculo(_))
      buscarSubcadenasTurboAux(combinaciones, n,oraculo,sck)
    }
    else {
      val combinaciones = combinar(sck, sck).filter(oraculo(_))
      buscarSubcadenasTurboAux(combinaciones, n,oraculo,sck) 
    }
  }
  if (n == 1) alfabetoForma.filter(oraculo(_))
  else if (n % 2 == 0) {
    val sub_cadenas = buscarSubcadenasTurboAux(alfabetoForma, n / 2,oraculo)
    val combinaciones = combinar(sub_cadenas, sub_cadenas).filter(oraculo(_))
    combinaciones
  } 
  else {
    val cadenas1 = buscarSubcadenasTurboAux(alfabetoForma, n/ 2,oraculo)
    val cadenas2 = buscarSubcadenasTurboAux(alfabetoForma, n-(n/2),oraculo)
    val combinaciones = combinar(cadenas1, cadenas2).filter(oraculo(_))
    combinaciones
  }
}


/**
 * Filtra las cadenas que cumplen con ciertas condiciones.
 *
 * @param cadena Cadena a filtrar.
 * @param subcadenas Lista de subcadenas.
 * @param tamaño Longitud de subcadena a verificar.
 * @return Booleano que indica si la cadena cumple con las condiciones.
 */
def filtro_cadenas(cadena: Seq[Char], subcadenas: Seq[Seq[Char]], tamaño: Int): Boolean = {
  // Define una función auxiliar recursiva que verifica si una cadena está contenida en una lista de subcadenas
  def estaContenido(cadena: Seq[Char], subcadenas: Seq[Seq[Char]]): Boolean = {
    if (subcadenas.isEmpty) false
    else if (subcadenas.head == cadena) true
    else estaContenido(cadena, subcadenas.tail)
  }
  // Si la cadena tiene el mismo tamaño que el deseado, verifica si está contenida en la lista de subcadenas
  if (cadena.length == tamaño) estaContenido(cadena.take(tamaño), subcadenas)
  // Si no, toma una subcadena del mismo tamaño y verifica si está contenida, y luego llama recursivamente a la función con el resto de la cadena
  else {
    val subcadena = cadena.take(tamaño)
    if (estaContenido(subcadena, subcadenas)) filtro_cadenas(cadena.tail, subcadenas, tamaño)
    else false
  } 
}

/**
 * Algoritmo de búsqueda de subcadenas turbo mejorado.
 *
 * @param tamaño Tamaño deseado de las subcadenas a buscar.
 * @param oraculo El oráculo que evalúa si una subcadena está presente en la cadena objetivo.
 * @return Lista de subcadenas encontradas.
 */
// Cambia el nombre de la función de PRC_turbo_mejorada a buscarSubcadenasTurboMejorado
def buscarCadenaTurboMejorado(tamaño: Int, oraculo: Oraculo): Seq[Seq[Char]]  = {
  // Define el alfabeto como una secuencia de los caracteres 'a', 'c', 'g' y 't'
  val alfabeto = Seq('a', 'c', 'g', 't')
  // Inicializa la lista de subcadenas con las de tamaño 1
  val alfabetoForma = alfabeto.map(Seq(_))
  // Define una función auxiliar recursiva que busca las subcadenas que satisfacen el oráculo
  def buscarSubcadenasTurboMejoradoAux(sck: Seq[Seq[Char]], n: Int,oraculo: Oraculo, cadenaVieja: Seq[Seq[Char]] = Seq(Seq())): Seq[Seq[Char]]  = {
    if (sck.isEmpty) Seq() 
    else if (sck.head.size >= n) sck
    else if (sck.head.size + 1 == n) {
      val combinaciones = combinar(sck, sck.flatten.distinct.map(Seq(_)))
      combinaciones.filter(oraculo(_))
    } else if (sck.head.size*2 > n) {
      val combinaciones = combinar(sck, cadenaVieja).filter(filtro_cadenas(_, sck, sck.head.size)).filter(oraculo(_))
      buscarSubcadenasTurboMejoradoAux(combinaciones, n,oraculo,sck)
    }
    else {
      val combinaciones = combinar(sck, sck).filter(filtro_cadenas(_, sck, sck.head.size)).filter(oraculo(_))
      if (combinaciones.filter(_.size == n).size != 0) combinaciones
      else buscarSubcadenasTurboMejoradoAux(combinaciones, n,oraculo,sck) 
    }
  }
  // Si el tamaño es 1, filtra las subcadenas por el oráculo
  if (tamaño == 1) alfabetoForma.filter(oraculo(_))
  // Si el tamaño es par, busca las subcadenas de la mitad del tamaño y luego las combina
  else if (tamaño % 2 == 0) {
    val sub_cadenas = buscarSubcadenasTurboMejoradoAux(alfabetoForma, tamaño / 2,oraculo(_))
    val combinaciones = combinar(sub_cadenas, sub_cadenas).filter(oraculo(_))
    combinaciones
  // Si el tamaño es impar, busca las subcadenas de la mitad superior e inferior del tamaño y luego las combina
  } else {
    val cadenas1 = buscarSubcadenasTurboMejoradoAux(alfabetoForma, tamaño/ 2,oraculo)
    val cadenas2 = buscarSubcadenasTurboMejoradoAux(alfabetoForma, tamaño -(tamaño/ 2),oraculo)
    val combinaciones = combinar(cadenas1, cadenas2).filter(oraculo(_))
    combinaciones
  }
}
  abstract class Trie
  case class Nodo(car: Char, marcada: Boolean, hijos: List[Trie]) extends Trie
  case class Hoja(car: Char, marcada: Boolean) extends Trie

  def raiz(t: Trie): Char = {
    t match {
      case Nodo(c, _, _) => c
      case Hoja(c, _) => c
    }
  }

  def cabezas(t: Trie): Seq[Char] = {
    t match {
      case Nodo(_, _, hijos) => hijos.map(t => raiz(t))
      case Hoja(c, _) => Seq[Char](c)
    }
  }


  /**
   * Verifica si una cadena pertenece a un árbol de sufijos.
   *
   * @param c Cadena a verificar.
   * @param t Árbol de sufijos.
   * @return Booleano que indica si la cadena pertenece al árbol.
   */
  def pertenece(c: Seq[Char], t: Trie): Boolean = {
    t match {
      case Nodo(car, marcada, hijos) => {
        if (c.isEmpty) marcada
        else {
          val cabeza = c.head
          val cola = c.tail
          val hijosCabeza = hijos.filter(hijo => raiz(hijo) == cabeza)
          if (hijosCabeza.isEmpty) false
          else pertenece(cola, hijosCabeza.head)
        }
      }
      case Hoja(car, marcada) => {
        if (c.isEmpty) marcada
        else false
      }
    }
  }

  /**
   * Agrega una cadena al árbol de sufijos.
   *
   * @param c Cadena a agregar.
   * @param t Árbol de sufijos.
   * @return Árbol de sufijos modificado.
   */
  def agregar(c: Seq[Char], t: Trie): Trie = {
    t match {
      case Nodo(car, marcada, hijos) => {
        if (c.isEmpty) Nodo(car, true, hijos)
        else {
          val cabeza = c.head
          val cola = c.tail
          val hijosCabeza = hijos.filter(hijo => raiz(hijo) == cabeza)
          if (hijosCabeza.isEmpty) {
            val nuevoHijo = if (cola.isEmpty) Hoja(cabeza, true) else agregar(cola, Nodo(cabeza, false, List()))
            Nodo(car, marcada, nuevoHijo :: hijos)
          } else {
            val nuevoHijo = agregar(cola, hijosCabeza.head)
            val otrosHijos = hijos.filter(hijo => raiz(hijo) != cabeza)
            Nodo(car, marcada, nuevoHijo :: otrosHijos)
          }
        }
      }
      case Hoja(car, marcada) => {
        if (c.isEmpty) Hoja(car, true)
        else {
          val cabeza = c.head
          val cola = c.tail
          if (car == cabeza) Nodo(car, marcada, List(if (cola.isEmpty) Hoja(cabeza, true) else agregar(cola, Nodo(cabeza, false, List()))))
          else Nodo(car, marcada, List(if (cola.isEmpty) Hoja(cabeza, true) else agregar(cola, Nodo(cabeza, false, List()))))
        }
      }
    }
  }

  /**
   * Agrega una secuencia de cadenas al árbol de sufijos.
   *
   * @param ss Lista de secuencias a agregar.
   * @param t Árbol de sufijos.
   * @return Árbol de sufijos modificado.
   */
  def agregarSecuancia_arbol(ss: Seq[Seq[Char]], t: Trie): Trie = {
    if (ss.isEmpty) t
    else agregarSecuancia_arbol(ss.tail, agregar(ss.head, t))
  }

  /**
   * Construye un árbol de sufijos a partir de una lista de secuencias de cadenas.
   *
   * @param ss Lista de secuencias de cadenas.
   * @return Árbol de sufijos construido.
   */
  def arbolDeSufijos(ss: Seq[Seq[Char]]): Trie = {
    val t: Trie = agregarSecuancia_arbol(ss, Nodo(' ', false, List()))
    t
  }


  /**
   * Recorre un árbol de sufijos y obtiene todas las secuencias presentes.
   *
   * @param t Árbol de sufijos.
   * @return Lista de secuencias presentes en el árbol.
   */
  def cadenas_del_arbol(t: Trie): Seq[Seq[Char]] = {
    t match {
      case Nodo(c, _, hijos) => 
        val secuenciasHijos = hijos.flatMap(hijo => cadenas_del_arbol(hijo))
        secuenciasHijos.map(secuencia => c +: secuencia).map(_.filter(_ != ' '))
      case Hoja(c, _) => Seq(Seq(c))
    }
  }

/**
  * Función auxiliar ramaValida que verifica si una rama tiene hijos.
  *
  * @param t: Trie: Nodo o Hoja del árbol de sufijos.
  * @return Boolean: true si la rama tiene hijos, false en caso contrario.
  */
  def ramaValida(t: Trie): Boolean = {
    t match {
      case Nodo(_, _, hijos) => hijos.nonEmpty
      case Hoja(_, _) => true
    }
  }

  /**
   * Algoritmo de búsqueda PRC (Pattern Recognition Code) Turbo Acelerada.
   *
   * @return Lista de subcadenas encontradas.
   */
  /**
 * Algoritmo de búsqueda de subcadenas turbo acelerada.
 *
 * @param tamaño Tamaño deseado de las subcadenas a buscar.
 * @param oraculo El oráculo que evalúa si una subcadena está presente en la cadena objetivo.
 * @return Lista de subcadenas encontradas.
 */
// Cambia el nombre de la función de turboAcelerada a buscarSubcadenasTurboAcelerada
def buscarCadenaTurboAcelerada(tamaño: Int, oraculo: Oraculo): Seq[Seq[Char]] = {
/**
  * Función auxiliar que realiza la reconstrucción de secuencias en paralelo.
  *
  * @param arbol Arbol de sufijos.
  * @param secuencias Secuencias a combinar.
  * @param acumulada Secuencia acumulada.
  * @return Arbol de sufijos actualizado.
  */

  def buscarSubcadenasTurboAceleradaAux(t: Trie, secuencias: Seq[Seq[Char]], acumulada: Seq[Char],oraculo: Oraculo): Trie = {
      t match {
        case Nodo(valor, esFinal, hijos) =>
          val nuevosHijos = hijos.map { hijo =>
            if (ramaValida(hijo)) buscarSubcadenasTurboAceleradaAux(hijo, secuencias, acumulada :+ valor,oraculo)
            else hijo
          }
          Nodo(valor, esFinal, nuevosHijos)
        case Hoja(valor, esFinal) =>
          if (!esFinal) Nodo(valor, esFinal, List())
          else {
            val cadenasNuevas: Seq[Seq[Char]] = secuencias.map(cadena => (acumulada.filter(_ != ' ') :+ valor) ++ cadena).filter(filtro_cadenas(_, secuencias, secuencias.head.size)).filter(oraculo(_))
            if (cadenasNuevas.isEmpty) Nodo(valor, esFinal, List())
            else {
              agregarSecuancia_arbol(cadenasNuevas.map(_.drop((acumulada.filter(_ != ' ') :+ valor).length)), t)
            }
          }
      
    }
  }

  /**
    * Función auxiliar evaluar_arbol que evalúa y actualiza el árbol de sufijos.
    *
    * @param arbol Arbol de sufijos.
    * @param n Longitud de las secuencias.
    * @return Seq[Seq[Char]] Secuencias encontradas que cumplen con el oráculo.
    */
  def evaluar_arbol(arbol: Trie, n: Int,oraculo: Oraculo,cadenaVieja: Seq[Seq[Char]]= Seq(Seq())): Seq[Seq[Char]] = {
    
    val todas_secuencias_arbol = cadenas_del_arbol(arbol)
    if (todas_secuencias_arbol.head.length + 1 == n) {
      val alfabeto = Seq('a', 'c', 'g', 't').map(Seq(_))
      combinar(todas_secuencias_arbol, alfabeto).filter(oraculo(_))
    }
    else if (todas_secuencias_arbol.head.size >= n) todas_secuencias_arbol
    else if (todas_secuencias_arbol.head.size*2 > n){
      val arbolNuevo = buscarSubcadenasTurboAceleradaAux(arbol, cadenaVieja, Seq(),oraculo)
      evaluar_arbol(arbolNuevo, n,oraculo,todas_secuencias_arbol)
    }
    else {
      val arbolNuevo = buscarSubcadenasTurboAceleradaAux(arbol, todas_secuencias_arbol, Seq(),oraculo)
      evaluar_arbol(arbolNuevo, n,oraculo,todas_secuencias_arbol)
    }
  }
  val alfabeto = Seq('a', 'c', 'g', 't')
  val arbol_inicial = arbolDeSufijos(alfabeto.map(cadena => Seq(cadena)))
  if (tamaño % 2 == 0) {
    val sub_cadenas = evaluar_arbol(arbol_inicial, tamaño / 2,oraculo)
    val combinaciones = combinar(sub_cadenas, sub_cadenas).filter(oraculo(_))
    combinaciones
  } else {
    val c1 = evaluar_arbol(arbol_inicial, tamaño / 2,oraculo)
    val c2 = evaluar_arbol(arbol_inicial, tamaño - (tamaño/ 2),oraculo)
    val combinaciones = combinar(c1, c2).filter(oraculo(_))
    combinaciones
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////
///////////////////////////////////////////////////
///////////////////////////////////////////////////   funciones paralelas
///////////////////////////////////////////////////
///////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * Función paralela que busca una secuencia de un tamaño dado que cumpla con el oráculo.
 *
 * @param tamaño Tamaño deseado de la secuencia a buscar.
 * @param oraculo El oráculo que evalúa si una secuencia está presente en la cadena objetivo.
 * @return Option[Seq[Char]]: La primera secuencia encontrada que cumpla con el oráculo, o None si no hay ninguna.
 */
// Cambia el nombre de la función de PRC_IngenuoPar a buscarSecuenciaParalelo
def buscarCadenaIngenuaPar(tamaño: Int, oraculo: Oraculo): Option[Seq[Char]] = {
  val alfabeto = Seq('a', 'c', 'g', 't')
  // Crea una lista de secuencias de un solo caracter con el alfabeto
  //val alfabetoForma = alfabeto.map(cadena => Seq(cadena))
  // Genera todas las combinaciones posibles de tamaño n-1 con el alfabeto
  val resultado = generarSubcadenas(alfabeto, tamaño -1)
  val (c1, c2) = parallel(resultado.take(tamaño / 2).find(oraculo(_)), resultado.drop(tamaño / 2).find(oraculo(_)))
  // Devuelve la primera secuencia encontrada, o None si no hay ninguna
  if (c1.isEmpty) c2 else c1
}

/**
 * Función paralela que busca secuencias mejoradas de un tamaño dado que cumplan con el oráculo.
 *
 * @param tamaño Tamaño deseado de las secuencias a buscar.
 * @param oraculo El oráculo que evalúa si una secuencia está presente en la cadena objetivo.
 * @return Seq[Seq[Char]]: Conjunto de secuencias encontradas que cumplen con el oráculo.
 */
// Cambia el nombre de la función de PRC_MejoradoPar a buscarSecuenciasMejoradasPar
def buscarCadenaMejoradasPar(tamaño: Int, oraculo: Oraculo): Seq[Seq[Char]] = {
  // Función auxiliar que busca secuencias mejoradas a partir de una lista de subsecuencias
  def buscarSecuenciasMSubs(subsecuencias: Seq[Seq[Char]],oraculo:Oraculo): Seq[Seq[Char]] = {
    // Agrega un caracter del alfabeto a cada subsecuencia y filtra las que cumplan con el oráculo
    val c1 = combinar(subsecuencias.take(tamaño / 2), Seq('a', 'c', 'g', 't').map(Seq(_))).find(oraculo(_))
    // Devuelve la primera secuencia encontrada, o una secuencia vacía si no hay ninguna
    if (c1.isDefined) c1.toSeq else Seq(Seq())
  }

  // Función auxiliar que busca secuencias mejoradas a partir de una lista de secuencias
  def buscarSecuenciasMejoradasAux(secuencias: Seq[Seq[Char]],oraculo: Oraculo): Seq[Seq[Char]] = {
    // Si la lista de secuencias está vacía, devuelve una secuencia vacía
    if (secuencias.head.isEmpty) Seq(Seq())
    else {
      // Divide la lista de secuencias en dos partes y busca en paralelo secuencias mejoradas a partir de cada una
      val (c1, c2) = parallel(buscarSecuenciasMSubs(secuencias.take(secuencias.length / 2),oraculo), buscarSecuenciasMSubs(secuencias.drop(secuencias.length / 2),oraculo))
      // Une las secuencias encontradas
      val total = c1 ++ c2
      // Si hay alguna secuencia de tamaño n, devuelve el conjunto de secuencias
      if (total.filter(_.size == tamaño).nonEmpty) total
      // Si no, llama recursivamente a la función auxiliar con el conjunto de secuencias
      else buscarSecuenciasMejoradasAux(total,oraculo)
    }
  }
  val alfabeto = Seq('a', 'c', 'g', 't')
  // Crea una lista de secuencias de un solo caracter con el alfabeto
  val alfabetoForma = alfabeto.map(cadena => Seq(cadena))
  // Llama a la función auxiliar con la lista de secuencias
  buscarSecuenciasMejoradasAux(alfabetoForma,oraculo)
}

/**
  * Función que busca una secuencia de un tamaño dado que cumpla con el oráculo, usando paralelismo y combinaciones iterativas.
  *
  * @param tamaño Tamaño deseado de la secuencia a buscar.
  * @param oraculo El oráculo que evalúa si una secuencia está presente en la cadena objetivo.
  * @return Option[Seq[Char]]: La primera secuencia encontrada que cumpla con el oráculo, o None si no hay ninguna.
  */
  // Cambia el nombre de la función de PRC_turboPar a buscarSecuenciaTurbo
  def buscarCadenaTurboPar(tamaño: Int, oraculo: Oraculo): Seq[Seq[Char]]  = {
    // Si el tamaño es 2, llama a la función buscarSecuencia
    if (tamaño == 2) buscarCadenaTurbo(tamaño, oraculo)
    // Si el tamaño es par, busca secuencias de tamaño n-1 y les agrega un caracter del alfabeto
    else if (tamaño % 2 == 0) {
      val subsecuencias = buscarCadenaTurboPar(tamaño - 1, oraculo)
      val secuencias = combinar(subsecuencias, Seq('a', 'c','g','t').map(Seq(_))).filter(oraculo(_))
      secuencias
    // Si el tamaño es impar, busca secuencias de tamaño (n+1)/2 y (n-1)/2 y las combina
    } else {
      val secuencias1 = task { buscarCadenaTurbo((tamaño + 1) / 2, oraculo) }
      val secuencias2 = task { buscarCadenaTurbo((tamaño - 1) / 2, oraculo) }
      val secuencias = combinar(secuencias1.join, secuencias2.join).filter(oraculo(_))
      secuencias
    }
  }

  /**
  * Función que busca secuencias mejoradas de un tamaño dado que cumplan con el oráculo, usando paralelismo y combinaciones iterativas.
  *
  * @param tamaño Tamaño deseado de las secuencias a buscar.
  * @param oraculo El oráculo que evalúa si una secuencia está presente en la cadena objetivo.
  * @return Seq[Seq[Char]]: Secuencias reconstruidas que cumplen con el oráculo.
  */
  // Cambia el nombre de la función de PRC_turbo_mejoradaPar a reconstruirSecuencias
  def buscarCadenaTurboMejoPar(tamaño: Int, oraculo: Oraculo): Seq[Seq[Char]]  = {
    // Si el tamaño es 2, llama a la función buscarSecuencia
    if (tamaño == 2) buscarCadenaTurboMejorado(tamaño, oraculo)
    // Si el tamaño es par, busca secuencias de tamaño n-1 y les agrega un caracter del alfabeto
    else if (tamaño % 2 == 0) {
      val subsecuencias = buscarCadenaTurboMejoPar(tamaño - 1, oraculo)
      val secuencias = combinar(subsecuencias, Seq('a','c','g','t').map(Seq(_))).filter(oraculo(_))
      secuencias
    // Si el tamaño es impar, busca secuencias de tamaño (n+1)/2 y (n-1)/2 y las combina
    } else {
      val secuencias1 = task { buscarCadenaTurboMejorado((tamaño + 1) / 2, oraculo) }
      val secuencias2 = task { buscarCadenaTurboMejorado((tamaño - 1) / 2, oraculo) }
      val secuencias = combinar(secuencias1.join, secuencias2.join).filter(oraculo(_))
      secuencias
    }
  }


/**
  * Función que reconstruye secuencias utilizando la propiedad de concatenación de subsecuencias y paralelismo.
  *
  * @param tamaño Tamaño deseado de las secuencias a reconstruir.
  * @param oraculo El oráculo que evalúa si una secuencia está presente en la cadena objetivo.
  * @return Seq[Seq[Char]]: Secuencias reconstruidas que cumplen con el oráculo.
  */
  // Cambia el nombre de la función de turboAceleradaPar a reconstruirSecuenciasParalelo
  def buscarCadenaTurboAceleradaPar(tamaño: Int, oraculo: Oraculo): Seq[Seq[Char]] = {
  /**
    * Función auxiliar que realiza la reconstrucción de secuencias en paralelo a partir de un árbol de sufijos.
    *
    * @param arbol Árbol de sufijos.
    * @param secuencias Secuencias a combinar.
    * @param acumulada Secuencia acumulada.
    * @return Trie: Árbol de sufijos actualizado.
    */
  
    def reconstruirSecuenciasParaleloAux(t: Trie, secuencias: Seq[Seq[Char]], acumulada: Seq[Char],oraculo: Oraculo): Trie = {
      t match {
        case Nodo(valor, esFinal, hijos) =>
          val parVector: ParVector[Trie] = hijos.toVector.par
          val nuevosHijos = parVector.map { hijo =>
            if (ramaValida(hijo)) reconstruirSecuenciasParaleloAux(hijo, secuencias, acumulada :+ valor,oraculo)
            else hijo
          }
          Nodo(valor, esFinal, nuevosHijos.seq.toList)
        case Hoja(valor, esFinal) =>
          if (!esFinal) Nodo(valor, esFinal, List())
          else {
            val cadenasNuevas: Seq[Seq[Char]] = secuencias.map(cadena => (acumulada.filter(_ != ' ') :+ valor) ++ cadena).filter(filtro_cadenas(_, secuencias, secuencias.head.size)).filter(oraculo(_))
            if (cadenasNuevas.isEmpty) Nodo(valor, esFinal, List())
            else {
              agregarSecuancia_arbol(cadenasNuevas.map(_.drop((acumulada.filter(_ != ' ') :+ valor).length)), t)
            }
          }
      }
    }

    /**
      * Función auxiliar que evalúa y actualiza el árbol de sufijos.
      *
      * @param arbol Árbol de sufijos.
      * @param n Longitud de las secuencias.
      * @return Seq[Seq[Char]]: Secuencias encontradas que cumplen con el oráculo.
      */
    def evaluarArbol(arbol: Trie, n: Int,oraculo: Oraculo,cadenaVieja: Seq[Seq[Char]] = Seq(Seq())): Seq[Seq[Char]] = {
      val todasSecuenciasArbol = cadenas_del_arbol(arbol)
      if (todasSecuenciasArbol.head.length + 1 == n) combinar(todasSecuenciasArbol, Seq('a', 'c','g','t').map(Seq(_))).filter(oraculo(_))
      else if (todasSecuenciasArbol.head.size == n) todasSecuenciasArbol
      else if (todasSecuenciasArbol.head.size*2 > n){
        val arbolNuevo = reconstruirSecuenciasParaleloAux(arbol, cadenaVieja, Seq(),oraculo)
        evaluarArbol(arbolNuevo, n,oraculo,todasSecuenciasArbol)
      }
      else {
        val arbolNuevo = reconstruirSecuenciasParaleloAux(arbol, todasSecuenciasArbol, Seq(),oraculo)
        evaluarArbol(arbolNuevo, n,oraculo,todasSecuenciasArbol)
      }
    }

    // Crea un árbol de sufijos con el alfabeto
    val alfabeto =  Seq('a', 'c','g','t')
    val arbolInicial = arbolDeSufijos(alfabeto.map(cadena => Seq(cadena)))
    // Si el tamaño es par, busca secuencias de tamaño n/2 y las combina
    if (tamaño % 2 == 0) {
      val subsecuencias = evaluarArbol(arbolInicial, tamaño / 2,oraculo)
      val secuencias = combinar(subsecuencias, subsecuencias).filter(oraculo(_))
      secuencias
    // Si el tamaño es impar, busca secuencias de tamaño (n+1)/2 y (n-1)/2 y las combina
    } else {
      val secuencias1 = evaluarArbol(arbolInicial, tamaño / 2,oraculo)
      val secuencias2 = evaluarArbol(arbolInicial,tamaño-(tamaño/ 2),oraculo)
      val secuencias = combinar(secuencias1, secuencias2).filter(oraculo(_))
      secuencias
    }
  }



  def compararAlgoritmos[T](funcion1: => T, funcion2: => T, numEjecuciones: Int = 1): (Double, Double, Double) = {

    def medirTiempo(funcion: => T): Double = {
      val tiempos = (1 to numEjecuciones).map { _ =>
        val inicio = System.nanoTime()
        try {
          funcion
        } catch {
          case _: Throwable => // Manejar cualquier excepción
        }
        val fin = System.nanoTime()
        (fin - inicio).toDouble / 1e6  // Convertir a milisegundos
      }
      tiempos.sum / numEjecuciones  // Promedio de tiempos
    }

    val tiempoFuncion1 = medirTiempo(funcion1)
    val tiempoFuncion2 = medirTiempo(funcion2)

    val aceleracion = tiempoFuncion1 / tiempoFuncion2

    (tiempoFuncion1, tiempoFuncion2, aceleracion)
  }

  def generarCadenaAleatoria(tamanio: Int, alfabeto: Seq[Char]): String = {
    val random = new Random
    (1 to tamanio).map(_ => alfabeto(random.nextInt(alfabeto.length))).mkString
  }

  def main(args: Array[String]): Unit = {
    /*
    val alfabeto = "acgt".toSeq
    // Comparaciones de rendimiento para cada función
    for (n <- 2 to 13) {
      val cadenaObjetivo = generarCadenaAleatoria(n, alfabeto)
      val oraculo: Oraculo = (subcadena: Seq[Char]) => cadenaObjetivo.contains(subcadena.mkString)

      println(s"Pruebas con cadena de tamaño $n para hallar $cadenaObjetivo")

      println("Pruebas PRC_Ingenuo vs. PRC_IngenuoPar")
      val resultadosIngenuo = compararAlgoritmos(buscarCadenaIngenuo(n,oraculo), buscarCadenaIngenuaPar(n,oraculo))
      println(resultadosIngenuo)

      println("Pruebas PRC_Mejorado vs. PRC_turbo_mejorada")
      val resultadosMejorado = compararAlgoritmos(buscarCadenaMejorado(n,oraculo), buscarCadenaMejoradasPar(n,oraculo))
      println(resultadosMejorado)

      println("Pruebas PRC_turbo vs. PRC_turboPar")
      val resultadosTurbo = compararAlgoritmos(buscarCadenaTurbo(n,oraculo), buscarCadenaTurboPar(n,oraculo))
      println(resultadosTurbo)


      println("Pruebas PRC_turbo_mejorada vs. PRC_turbo_mejoradaPar")
      val resultadosTurboMejorada = compararAlgoritmos(buscarCadenaTurboMejorado(n,oraculo), buscarCadenaTurboMejoPar(n,oraculo))
      println(resultadosTurboMejorada)

      println("Pruebas turboAcelerada vs. turboAceleradaPar")
      val resultadosTurboAcelerada = compararAlgoritmos(buscarCadenaTurboAcelerada(n,oraculo), buscarCadenaTurboAceleradaPar(n,oraculo))
      println(resultadosTurboAcelerada)

      println()
    }
    */
    
    val alfabeto = "acgt".toSeq
    
    // prueba de funcionamiento
    for (n <- 7 to 7) {
      val cadenaObjetivo = generarCadenaAleatoria(n, alfabeto)
      //val buscadorCadena1 = new BuscadorCadena(cadenaObjetivo, alfabeto)
      val oraculo: Oraculo = (subcadena: Seq[Char]) => cadenaObjetivo.contains(subcadena.mkString)

      println(s"Pruebas con cadena de tamaño $n para hallar $cadenaObjetivo")

      // Caso de prueba 1: buscarCadenaIngenuo
      val resultadoIngenuo = buscarCadenaIngenuo(n,oraculo)
      if (resultadoIngenuo.getOrElse(Seq()) != cadenaObjetivo.toSeq) {
        println("buscarCadenaIngenuo no paso el test.")
      }

      // Caso de prueba 2: buscarCadenaMejorado
      val resultadoMejorado = buscarCadenaMejorado(n,oraculo)
      if (resultadoMejorado.headOption.getOrElse(Seq()) != cadenaObjetivo.toSeq) {
        println("PRC_Mejorado no paso el test.")
      }
      // Caso de prueba 3: buscarCadenaTurbo
      val resultadoTurbo = buscarCadenaTurbo(n,oraculo)
      if (resultadoTurbo.headOption.getOrElse(Seq()) != cadenaObjetivo.toSeq) {
        println(s"buscarCadenaTurbo no paso el test. ${resultadoTurbo} != ${cadenaObjetivo.toSeq}")
      }

      // Caso de prueba 4: buscarCadenaTurboMejorado
      val resultadoTurboMejorada = buscarCadenaTurboMejorado(n,oraculo)
      if (resultadoTurboMejorada.headOption.getOrElse(Seq()) != cadenaObjetivo.toSeq) {
        println("buscarCadenaTurboMejorado no paso el test.")
      } 
      // Caso de prueba 10xd: buscarCadenaTurboAcelerada
      val resultadoTurboAcelerada = buscarCadenaTurboAcelerada(n,oraculo)
      if (resultadoTurboAcelerada.headOption.getOrElse(Seq()) != cadenaObjetivo.toSeq) {
        println(s"turboAcelerada no paso el test. ${resultadoTurboAcelerada} != ${cadenaObjetivo.toSeq}")
      } 

      // Caso de prueba 5: buscarCadenaIngenuaPar
      val resultadoIngenuoPar = buscarCadenaIngenuaPar(n,oraculo)
      if (resultadoIngenuoPar.getOrElse(Seq()) != cadenaObjetivo.toSeq) {
        println(s"PRC_IngenuoPar no paso el test. $resultadoIngenuoPar")
      } 

      // Caso de prueba 6: buscarCadenaMejoradasPar
      val resultadoMejoradoPar = buscarCadenaMejoradasPar(n,oraculo)
      if (resultadoIngenuoPar.getOrElse(Seq()) != cadenaObjetivo.toSeq) {
        println("PRC_MejoradoPar no paso el test.")
      } 

      // Caso de prueba 7: buscarCadenaTurboPar
      val resultadoTurboPar = buscarCadenaTurboPar(n,oraculo)
      if (resultadoTurboPar.headOption.getOrElse(Seq()) != cadenaObjetivo.toSeq) {
        println("PRC_turboPar no paso el test.")
      } 

      // Caso de prueba 6: buscarCadenaTurboMejoPar
      val resultadoTurboMejoradaPar = buscarCadenaTurboMejoPar(n,oraculo)
      if (resultadoTurboMejoradaPar.headOption.getOrElse(Seq()) != cadenaObjetivo.toSeq) {
        println(s"buscarCadenaTurboMejoPar no paso el test. ${resultadoTurboMejoradaPar} != ${cadenaObjetivo.toSeq}")
      } 

      // Caso de prueba 7: buscarCadenaTurboAceleradaPar
      val resultadoTurboAceleradaPar = buscarCadenaTurboAceleradaPar(n,oraculo)
      if (resultadoTurboAceleradaPar.headOption.getOrElse(Seq()) != cadenaObjetivo.toSeq) {
        println(s"turboAceleradaPar no paso el test. ${resultadoTurboAceleradaPar.head} != ${cadenaObjetivo.toSeq}")
      }
      println()
    }
    
  }
}