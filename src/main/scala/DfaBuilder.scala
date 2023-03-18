/* Pentru a putea construi un DFA cu starile denumite corespunzator, se va tine minte
urmatorul nume disponibil pentru o stare, acesta fiind obtinut prin apelul functiei f
(pentru a asigura genericitate) */
class DfaBuilder[A](nfa: Nfa[A], var nextStateName: A, f: A => A) {

	var NfaGroupToDfaState: Map[Set[A], A] = Map[Set[A], A]() // map pentru asocierea grup stari NFA -> nume stare DFA

	/* Construieste DFA-ul aplicand algoritmul Subset Construction */
	def genDfa: Dfa[A] = {

		var autom = Map[(A, Boolean), Set[(Char, A)]]()
		val initState = nextStateName // stare initiala DFA

		var visited = Set[Set[A]]() // tine minte ce grupuri de stari din NFA au fost deja vizitate
		visited += nfa.epsClosure(nfa.InitState)

		var currNfaStatesSet = Set[Set[A]]() // grupurile de stari curente din NFA
		currNfaStatesSet += nfa.epsClosure(nfa.InitState)

		NfaGroupToDfaState += (nfa.epsClosure(nfa.InitState) -> nextStateName) // adaugare asociere grup stari initiale
		nextStateName = f (nextStateName)

		var continue = 1
		while (continue == 1) {
			var nextNfaStatesSet = Set[Set[A]]()

			continue = 0
			for (currNfaStates <- currNfaStatesSet) {
				var transitions = Set[(Char, A)]() // tranzitiile ce vor fi create din starea DFA curenta

				var isFinal = false
				/* Daca grupul de stari contine o stare finala, starea DFA va fi si ea finala */
				for (currState <- currNfaStates) {
					isFinal ||= nfa.isFinal(currState)
				}

				for (c <- nfa.getAlphabet) { // pentru fiecare caracter din alfabet

					var nextStates = Set[A]()
					/* Se adauga epsilon closure de fiecare stare urmatoare obtinuta pe caracterul curent */
					for (currState <- currNfaStates) {
						for (nextState <- nfa.next(currState, c)) {
							nextStates ++= nfa.epsClosure(nextState)
						}
					}

					if (nextStates != Set.empty) { // Daca exista tranzitie pe caracter
						if (!visited.contains(nextStates)) { // Daca grupul de stari nu a mai fost vizitat
							continue = 1 // Algoritmul continua pana nu se mai obtin grupuri noi de stari
							visited += nextStates
						}
						/* Daca nu exista o asociere pentru grupul de stari curent, se adauga la urmatoarea
						stare DFA disponibila */
						if (!NfaGroupToDfaState.contains(nextStates)) {
							NfaGroupToDfaState += (nextStates -> nextStateName)
							nextStateName = f(nextStateName)
						}

						transitions += ((c, NfaGroupToDfaState(nextStates)))

						nextNfaStatesSet += nextStates
					}
				}
				/* Se adauga in automat starea DFA curenta cu tranzitiile obtinute */
				autom += (NfaGroupToDfaState(currNfaStates), isFinal) -> transitions
			}
			/* Noile grupuri de stari curente vor fi cele obtinute din grupurile precedente pe fiecare caracter din alfabet */
			currNfaStatesSet = nextNfaStatesSet
		}

		new Dfa[A](autom, initState)
	}
}

object DfaBuilder {

	/* Construieste DFA-ul din NFA-ul dat, care va avea urmatorul nume de stare 0, iar
	functia de generare a urmatorului nume de stare (+1) */
	def buildDfa(nfa: Nfa[Int]): DfaBuilder[Int] = {
		new DfaBuilder[Int](nfa, 0, x => x + 1)
	}
}