/* Automatul va fi reprezentat drept un map cu cheia (stare, Boolean reprezentand daca starea e finala
sau nu) si valoarea un set de (caracter, stare) reprezentand toate tranzitiile din starea curenta */
class Dfa[A] (autom: Map[(A, Boolean), Set[(Char, A)]], initState: A) {

	def Autom: Map[(A, Boolean), Set[(Char, A)]] = autom
	def InitState: A = initState

	/* Construieste un automat nou din cel primit ca parametru, aplicand functia data pe fiecare
	stare a acestuia, atat din cheia map-ului cat si pentru fiecare tranzitie corespondenta */
	def map[B](f: A => B): Dfa[B] = {
		var newAut = Map[(B, Boolean), Set[(Char, B)]]()
		for ((k, v) <- autom) {
			newAut += ((f (k._1), k._2) -> v.map(x => (x._1, f (x._2))))
		}
		new Dfa[B](newAut, f (initState))
	}

	/* Intoarce starea in care se ajunge din starea data, pe caracterul dat, daca exista */
	def next(state: A, c: Char): A = {
		for ((k, v) <- autom) {
			if (k._1 == state) {
				val nextStateSet = v.filter(x => x._1 == c).map(x => x._2)
				if (nextStateSet != Set.empty) {
					return nextStateSet.head
				}
			}
		}
		null.asInstanceOf[A]
	}

	/* Verifica daca automatul accepta cuvantul dat */
	def accepts(str: String): Boolean = {
		var currState = initState
		for (c <- str) {
			currState = next(currState, c)
			if (currState == null) {
				return false
			}
		}
		isFinal(currState)
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
}

object Dfa {
	def fromPrenex(str: String): Dfa[Int] = {
		DfaBuilder.buildDfa(Nfa.fromPrenex(str)).genDfa // construieste DFA-ul din NFA-ul construit din prenex-ul dat
	}
}