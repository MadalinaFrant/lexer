/* Pentru a putea construi un NFA cu starile denumite corespunzator, se va tine minte
urmatorul nume disponibil pentru o stare, acesta fiind obtinut prin apelul functiei f
(pentru a asigura genericitate); se retine si vectorul corespunzator prenex-ului pentru
a itera corect prin acesta */
class NfaBuilder[A](var splitP: Array[String], var nextStateName: A, f: A => A) {

	/* NFA limbajul vid = o stare initiala si una finala, fara legaturi intre acestea */
	def voidNfa: Nfa[A] = {
		var aut = Map[(A, Boolean), Set[(Char, A)]]()

		val initState = nextStateName
		nextStateName = f (nextStateName)
		val finalState = nextStateName
		nextStateName = f (nextStateName)

		aut += (initState, false) -> Set()
		aut += (finalState, true) -> Set()

		new Nfa[A](aut, initState)
	}

	/* NFA pentru un caracter = o stare initiala cu tranzitie pe caracter in starea finala */
	def charNfa(char: Char): Nfa[A] = {
		var aut = Map[(A, Boolean), Set[(Char, A)]]()

		val initState = nextStateName
		nextStateName = f (nextStateName)
		val finalState = nextStateName
		nextStateName = f (nextStateName)

		aut += (initState, false) -> Set((char, finalState))
		aut += (finalState, true) -> Set()

		new Nfa[A](aut, initState)
	}

	/* NFA concatenare: se adauga legatura intre starea finala a NFA1 cu starea initiala a NFA2 */
	def concatNfa(nfa1: Nfa[A], nfa2: Nfa[A]): Nfa[A] = {
		val initState = nfa1.InitState

		var aut1 = nfa1.Autom
		var aut2 = nfa2.Autom

		val finalState1 = nfa1.FinalState
		val initState2 = nfa2.InitState

		var v = aut1((finalState1, true))
		v += (('ε', initState2))
		aut1 -= ((finalState1, true))
		aut1 += (finalState1, false) -> v

		var aut = Map[(A, Boolean), Set[(Char, A)]]()

		for (x <- aut1) {
			aut += x
		}
		for (x <- aut2) {
			aut += x
		}

		new Nfa[A](aut, initState)
	}

	/* NFA reuniune: se adauga 2 stari noi reprezentand noua stare initiala respectiv finala
	cu legaturile corespunzatoare */
	def unionNfa(nfa1: Nfa[A], nfa2: Nfa[A]): Nfa[A] = {
		val initState = nfa1.InitState

		val Nfa1 = nfa1.map(f)
		val Nfa2 = nfa2.map(f)
		nextStateName = f (nextStateName)

		val finalState = nextStateName
		nextStateName = f (nextStateName)

		var aut1 = Nfa1.Autom
		var aut2 = Nfa2.Autom

		val initState1 = Nfa1.InitState
		val finalState1 = Nfa1.FinalState
		val v1 = aut1((finalState1, true))
		aut1 -= ((finalState1, true))
		aut1 += (finalState1, false) -> v1

		val initState2 = Nfa2.InitState
		val finalState2 = Nfa2.FinalState
		val v2 = aut2((finalState2, true))
		aut2 -= ((finalState2, true))
		aut2 += (finalState2, false) -> v2

		var aut = Map[(A, Boolean), Set[(Char, A)]]()
		aut += (initState, false) -> Set(('ε', initState1), ('ε', initState2))
		aut += (finalState, true) -> Set()

		for ((k, v) <- aut1) {
			var V = v
			if (k._1 == finalState1) {
				V += (('ε', finalState))
			}
			aut += k -> V
		}
		for ((k, v) <- aut2) {
			var V = v
			if (k._1 == finalState2) {
				V += (('ε', finalState))
			}
			aut += k -> V
		}

		new Nfa[A](aut, initState)
	}

	/* NFA Kleene star: se adauga 2 stari noi reprezentand noua stare initiala respectiv finala
	cu legaturile corespunzatoare */
	def starNfa(nfa: Nfa[A]): Nfa[A] = {
		val newInitState = nfa.InitState

		val Nfa = nfa.map(f)
		nextStateName = f (nextStateName)

		val newFinalState = nextStateName
		nextStateName = f (nextStateName)

		var aut = Nfa.Autom

		val initState = Nfa.InitState
		val finalState = Nfa.FinalState

		var v = aut((finalState, true))
		v += (('ε', newFinalState))
		v += (('ε', initState))
		aut -= ((finalState, true))
		aut += (finalState, false) -> v

		aut += (newInitState, false) -> Set(('ε', initState), ('ε', newFinalState))
		aut += (newFinalState, true) -> Set()

		new Nfa[A](aut, newInitState)
	}

	/* NFA plus: concatenare intre nfa si STAR nfa */
	def plusNfa(nfa: Nfa[A]): Nfa[A] = {
		var nfa1 = nfa
		var nfa2 = starNfa(nfa)

		/* Intrucat NFA-ul corespunzator primului element al concatenarii a fost deja creat, iar
		NFA-ul reprezentand STAR al acestui NFA nu poate contine aceleasi stari, trebuie aplicat
		map pe acesta de un numar de ori egal cu numarul de stari din primul NFA cu functia de
		generare a numelor starilor */
		for (_ <- nfa1.getStates) {
			nfa2 = nfa2.map(f)
			nextStateName = f (nextStateName)
		}

		concatNfa(nfa1, nfa2)
	}

	/* NFA maybe: reuniune intre nfa si nfa epsilon */
	def maybeNfa(nfa: Nfa[A]): Nfa[A] = {
		unionNfa(nfa, charNfa('ε'))
	}

	/* Intoarce un caracter din string-ul dat, reprezentand un atom */
	def strToChar(str: String): Char = {
		var c = str(0)
		if (c == '\'') { // daca caracterul este continut intre ghilimele, acestea se elimina
			c = str(1)
		}
		if (str == "eps") { // caracter corespunzator epsilon
			c = 'ε'
		}
		c
	}

	/* Construieste NFA-ul apeland functiile corespunzatoare operatiei/atomului curent */
	def genNfa: Nfa[A] = {
		val str = splitP(0)
		splitP = splitP.drop(1)

		/* Se realizeaza legaturile corespunzatoare si se adauga starile noi necesare pentru a
		obtine noile automate in functie de operatie, conform algoritmului Thompson */

		if (str == "CONCAT") {
			return concatNfa(genNfa, genNfa)
		}
		if (str == "UNION") {
			return unionNfa(genNfa, genNfa)
		}
		if (str == "STAR") {
			return starNfa(genNfa)
		}
		if (str == "PLUS") {
			return plusNfa(genNfa)
		}
		if (str == "MAYBE") {
			return maybeNfa(genNfa)
		}

		if (str == "void") {
			return voidNfa
		}

		/* Elementul curent este un atom, deci se intoarce automatul corespunzator caracterului */
		val c = strToChar(str)
		charNfa(c)
	}

}

object NfaBuilder {

	/* Intoarce un vector de String-uri din String-ul reprezentand prenex-ul, fiecare element
	reprezentand un atom sau o operatie */
	def splitPrenex(prenex: String): Array[String] = {
		var splitP = prenex.split(" ")
		var i = 0
		while (i < splitP.length) {
			if (splitP(i) == "'") { // caz special caracterul spatiu
				splitP(i) += " " ++ splitP(i + 1)
				splitP = splitP.take(i + 1) ++ splitP.drop(i + 2)
			}
			i += 1
		}
		splitP
	}

	/* Construieste NFA-ul din prenex-ul dat, care va avea urmatorul nume de stare 0, iar
	functia de generare a urmatorului nume de stare (+1) */
	def buildNfa(prenex: String): NfaBuilder[Int] = {
		new NfaBuilder[Int](splitPrenex(prenex), 0, x => x + 1)
	}
}