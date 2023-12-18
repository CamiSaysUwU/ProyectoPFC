/**
 * Plantilla para pruebas
* @author Camilo Valencia Romero, Cristian Rivera
* @version 1.0
 */
package taller4

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner
// Importar el objeto Taller4 que contiene la función multMatriz y las funciones auxiliares
import Taller4._

@RunWith(classOf[JUnitRunner])
class testMultMatriz extends AnyFunSuite {
  // Definir una función para mostrar las matrices de forma más legible

  // Prueba 1: matrices de 4x4
  test("Prueba1 de 2 a 8") {
    // Crear instancias de BuscadorCadena con dassertrentes parámetros de entrada
    val alfabeto = "acgt".toSeq
    
    // prueba de funcionamiento
    for (n <- 2 to 6) {
      val cadenaObjetivo = generarCadenaAleatoria(n, alfabeto)
      //val buscadorCadena1 = new BuscadorCadena(cadenaObjetivo, alfabeto)
      val oraculo: Oraculo = (subcadena: Seq[Char]) => cadenaObjetivo.contains(subcadena.mkString)

      println(s"Pruebas con cadena de tamaño $n para hallar $cadenaObjetivo")

      // Caso de prueba 1: buscarCadenaIngenuo
      val resultadoIngenuo = buscarCadenaIngenuo(n,oraculo)
      assert(resultadoIngenuo.getOrElse(Seq()) == cadenaObjetivo.toSeq)

      // Caso de prueba 2: buscarCadenaMejorado
      val resultadoMejorado = buscarCadenaMejorado(n,oraculo)
      assert(resultadoMejorado.headOption.getOrElse(Seq()) == cadenaObjetivo.toSeq) 

      // Caso de prueba 3: buscarCadenaTurbo
      val resultadoTurbo = buscarCadenaTurbo(n,oraculo)
      assert(resultadoTurbo.headOption.getOrElse(Seq()) == cadenaObjetivo.toSeq) 
      // Caso de prueba 4: buscarCadenaTurboMejorado
      val resultadoTurboMejorada = buscarCadenaTurboMejorado(n,oraculo)
      assert(resultadoTurboMejorada.headOption.getOrElse(Seq()) == cadenaObjetivo.toSeq) 

      // Caso de prueba 10xd: buscarCadenaTurboAcelerada
      val resultadoTurboAcelerada = buscarCadenaTurboAcelerada(n,oraculo)
      assert(resultadoTurboAcelerada.headOption.getOrElse(Seq()) == cadenaObjetivo.toSeq)

      // Caso de prueba 5: buscarCadenaIngenuaPar
      val resultadoIngenuoPar = buscarCadenaIngenuaPar(n,oraculo)
      assert(resultadoIngenuoPar.getOrElse(Seq()) == cadenaObjetivo.toSeq) 

      // Caso de prueba 6: buscarCadenaMejoradasPar
      val resultadoMejoradoPar = buscarCadenaMejoradasPar(n,oraculo)
      assert(resultadoIngenuoPar.getOrElse(Seq()) == cadenaObjetivo.toSeq) 

      // Caso de prueba 7: buscarCadenaTurboPar
      val resultadoTurboPar = buscarCadenaTurboPar(n,oraculo)
      assert(resultadoTurboPar.headOption.getOrElse(Seq()) == cadenaObjetivo.toSeq)

      // Caso de prueba 6: buscarCadenaTurboMejoPar
      val resultadoTurboMejoradaPar = buscarCadenaTurboMejoPar(n,oraculo)
      assert(resultadoTurboMejoradaPar.headOption.getOrElse(Seq()) == cadenaObjetivo.toSeq) 

      // Caso de prueba 7: buscarCadenaTurboAceleradaPar
      val resultadoTurboAceleradaPar = buscarCadenaTurboAceleradaPar(n,oraculo)
      assert(resultadoTurboAceleradaPar.headOption.getOrElse(Seq()) == cadenaObjetivo.toSeq)
      println()
    }
  }
  test("Prueba2 tamaño 11") {
    // Crear instancias de BuscadorCadena con dassertrentes parámetros de entrada
    val alfabeto = "acgt".toSeq
    
    // prueba de funcionamiento
    for (n <- 11 to 11) {
      val cadenaObjetivo = generarCadenaAleatoria(n, alfabeto)
      //val buscadorCadena1 = new BuscadorCadena(cadenaObjetivo, alfabeto)
      val oraculo: Oraculo = (subcadena: Seq[Char]) => cadenaObjetivo.contains(subcadena.mkString)

      println(s"Pruebas con cadena de tamaño $n para hallar $cadenaObjetivo")

      // Caso de prueba 1: buscarCadenaIngenuo
      val resultadoIngenuo = buscarCadenaIngenuo(n,oraculo)
      assert(resultadoIngenuo.getOrElse(Seq()) == cadenaObjetivo.toSeq)

      // Caso de prueba 2: buscarCadenaMejorado
      val resultadoMejorado = buscarCadenaMejorado(n,oraculo)
      assert(resultadoMejorado.headOption.getOrElse(Seq()) == cadenaObjetivo.toSeq) 

      // Caso de prueba 3: buscarCadenaTurbo
      val resultadoTurbo = buscarCadenaTurbo(n,oraculo)
      assert(resultadoTurbo.headOption.getOrElse(Seq()) == cadenaObjetivo.toSeq) 
      // Caso de prueba 4: buscarCadenaTurboMejorado
      val resultadoTurboMejorada = buscarCadenaTurboMejorado(n,oraculo)
      assert(resultadoTurboMejorada.headOption.getOrElse(Seq()) == cadenaObjetivo.toSeq) 

      // Caso de prueba 10xd: buscarCadenaTurboAcelerada
      val resultadoTurboAcelerada = buscarCadenaTurboAcelerada(n,oraculo)
      assert(resultadoTurboAcelerada.headOption.getOrElse(Seq()) == cadenaObjetivo.toSeq)

      // Caso de prueba 5: buscarCadenaIngenuaPar
      val resultadoIngenuoPar = buscarCadenaIngenuaPar(n,oraculo)
      assert(resultadoIngenuoPar.getOrElse(Seq()) == cadenaObjetivo.toSeq) 

      // Caso de prueba 6: buscarCadenaMejoradasPar
      val resultadoMejoradoPar = buscarCadenaMejoradasPar(n,oraculo)
      assert(resultadoIngenuoPar.getOrElse(Seq()) == cadenaObjetivo.toSeq) 

      // Caso de prueba 7: buscarCadenaTurboPar
      val resultadoTurboPar = buscarCadenaTurboPar(n,oraculo)
      assert(resultadoTurboPar.headOption.getOrElse(Seq()) == cadenaObjetivo.toSeq)

      // Caso de prueba 6: buscarCadenaTurboMejoPar
      val resultadoTurboMejoradaPar = buscarCadenaTurboMejoPar(n,oraculo)
      assert(resultadoTurboMejoradaPar.headOption.getOrElse(Seq()) == cadenaObjetivo.toSeq) 

      // Caso de prueba 7: buscarCadenaTurboAceleradaPar
      val resultadoTurboAceleradaPar = buscarCadenaTurboAceleradaPar(n,oraculo)
      assert(resultadoTurboAceleradaPar.headOption.getOrElse(Seq()) == cadenaObjetivo.toSeq)
      println()
    }
  }
}
