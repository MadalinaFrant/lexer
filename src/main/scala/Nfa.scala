/* Automatul va fi reprezentat drept un map cu cheia (stare, Boolean reprezentand daca starea e finala
sau nu) si valoarea un set de (caracter, stare) reprezentand toate tranzitiile din starea curenta */
class Nfa[A] (autom: Map[(A, Boolean), Set[(Char, A)]], initState: A) {

	def Autom: Map[(A, Boolean), Set[(Char, A)]] = autom
	def InitState: A = initState
	// NFA-ul construit va avea mereu o singura stare finala
	def FinalState: A = {
		var finalState = null.asInstanceOf[A]
		for ((k, _) <- autom) {
			if (isFinal(k._1)) {
				finalState = k._1
			}
		}
		finalState
	}

	/* Construieste un automat nou din cel primit ca parametru, aplicand functia data pe fiecare
	stare a acestuia, atat din cheia map-ului cat si pentru fiecare tranzitie corespondenta */
	def map[B](f: A => B): Nfa[B] = {
		var newAutom = Map[(B, Boolean), Set[(Char, B)]]()
		for ((k, v) <- autom) {
			newAutom += ((f (k._1), k._2) -> v.map(x => (x._1, f (x._2))))
		}
		new Nfa[B](newAutom, f (initState))
	}

	/* Intoarce toate starile in care se poate ajunge din starea data, pe caracterul dat */
	def next(state: A, c: Char): Set[A] = {
		for ((k, v) <- autom) {
			if (k._1 == state) {
				return v.filter(x => x._1 == c).map(x => x._2)
			}
		}
		Set[A]()
	}

	/* Verifica daca automatul accepta cuvantul dat */
	def accepts(str: String): Boolean = {
		/* In currStates se salveaza toate starile curente in care s-a ajuns din starea anterioara,
		consumand un caracter din cuvant sau pe epsilon (neconsumand niciun caracter). Astfel, pentru
		a tine cont de cat din cuvant s-a consumat, starea curenta va avea asociat si cat din cuvant
		s-a consumat pana la aceasta. Initial, starea curenta va fi starea initala, cu cuvantul intreg */
		var currStates = Set[(A, String)]()
		currStates += ((initState, str))

		var Str = str

		/* Se executa tranzitii pana cand nu se mai poate realiza niciuna */
		while (currStates != Set.empty) {
			var nextStates = Set[(A, String)]()

			var c = null.asInstanceOf[Char]
			if (Str.nonEmpty) {
				c = Str(0) // preia urmatorul caracter din cuvant
			}

			for (currState <- currStates) {
				/* Daca cuvantul nu este gol si daca exista tranzitii din starea curenta prin urmatorul caracter
				al cuvantului acestea se adauga la starile noi generate si se consuma caracterul din cuvant */
				if (Str.nonEmpty) {
					if (next(currState._1, c) != Set.empty) {
						Str = Str.drop(1) // Consuma caracter
						for (nextState <- next(currState._1, c)) {
							nextStates += ((nextState, Str))
						}
					}
				}

				/* Se adauga la starile noi generate si toate starile in care se ajunge din starea curenta
				neconsumand input-ul, pe epsilon */
				for (nextState <- next(currState._1, 'ε')) {
					nextStates += ((nextState, currState._2))
				}
			}
			currStates = nextStates // Starile curente vor deveni noile stari gasite

			/* Daca s-a terminat de parcurs tot cuvantul se verifica daca in starile in care s-a ajuns
			exista o stare finala si pana la aceasta s-a consumat tot cuvantul */
			if (Str.isEmpty) {
				for (currState <- currStates) {
					if (isFinal(currState._1) && currState._2.isEmpty) {
						return true
					}
				}
			}
		}
		false
	}

	/* Intoarce toate starile automatului */
	def getStates: Set[A] = {
		var states = Set[A]()
		for ((k, _) <- autom) {
			states += k._1
		}
		states
	}

	/* Verifica daca starea data este finala */
	def isFinal(state: A): Boolean = {
		for ((k, _) <- autom) {
			if (k._1 == state) {
				return k._2
			}
		}
		false
	}

	/* Intoarce epsilon-closure al starii date */
	def epsClosure(state: A): Set[A] = {
		var statesSet = Set[A]()

		var currStates = Set[A](state)
		while (currStates != Set.empty) { // Se adauga pana nu mai exista tranzitii pe epsilon din starea curenta
			var continue = 0 // Daca nu s-a ajuns in nicio stare noua, au fost gasite toate starile cautate
			for (currState <- currStates) {
				if (!statesSet.contains(currState)) {
					continue = 1
				}
			}
			if (continue == 0) {
				return statesSet
			}
			statesSet ++= currStates
			var nextStates = Set[A]()
			for (currState <- currStates) {
				nextStates ++= next(currState, 'ε')
			}
			currStates = nextStates
		}
		statesSet
	}

	/* Intoarce alfabetul corespondent NFA-ului */
	def getAlphabet: Set[Char] = {
		var alphabet = Set[Char]()
		for ((_, v) <- autom) {
			alphabet ++= v.map(x => x._1)
		}
		alphabet -= 'ε'
		alphabet
	}
}

object Nfa {
	def fromPrenex(str: String): Nfa[Int] = {
		NfaBuilder.buildNfa(str).genNfa // construieste NFA-ul din prenex-ul dat
	}
}