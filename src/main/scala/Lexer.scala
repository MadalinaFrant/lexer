import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Lexer (spec: String) {

	/* Map (care pastreaza ordinea insertiei, necesara in cazul departajarii cuvintelor ce satisfac mai multe regex-uri)
	in care se retine asocierea dintre un token si NFA-ul corespunzator regex-ului acestuia */
	var tokenToNfaMap: mutable.LinkedHashMap[String, Nfa[Int]] = genNfas

	/* Creeaza map-ul definit mai sus, generand cate un NFA pentru fiecare token */
	def genNfas: mutable.LinkedHashMap[String, Nfa[Int]] = {
		var map = mutable.LinkedHashMap[String, Nfa[Int]]()

		/* Intrucat ulterior toate aceste NFA-uri vor face parte dintr-un nou NFA, pentru a pastra
		unicitatea starilor acestea vor fi numite in continuarea starilor NFA-ului creat anterior,
		adaugand la numele acestora numarul de stari deja folosite; initial, va exista o stare folosita
		care va reprezenta starea initiala (0) a noului automat */
		var usedStates = 1

		for (line <- spec.split("\n")) { // Se trece prin fiecare linie a specificatiei
			var token_regex = line.split(" *: *")

			/* Se extrag token-ul si regex-ul */
			var token = token_regex.head
			var regex = token_regex.last.split(";").head

			var nfa = Nfa.fromPrenex(Regex.toPrenex(regex)) // Se creeaza un NFA din regex-ul citit

			/* In map se va adauga asocierea token -> NFA, asupra caruia s-a aplicat maparea pentru obtinerea unor nume de stari unice */
			map += token -> nfa.map(x => x + usedStates)

			usedStates += nfa.getStates.size // Se actualizeaza numarul de stari folosite
		}

		map
	}

	/* Primeste o stare din DFA si intoarce grupul de stari NFA care o compun */
	def DfaStateToNfaGroup(nfaGroupToDfaStateMap: Map[Set[Int], Int], dfaState: Int): Set[Int] = {
		for ((k, v) <- nfaGroupToDfaStateMap) {
			if (v == dfaState) {
				return k
			}
		}

		Set()
	}

	/* Primeste o stare din DFA si intoarce token-ul corespunzator acesteia */
	def getToken(nfaGroupToDfaStateMap: Map[Set[Int], Int], dfaState: Int): String = {
		var nfaGroup = DfaStateToNfaGroup(nfaGroupToDfaStateMap, dfaState)

		/* Pentru fiecare asociere token -> NFA se verifica daca NFA-ul corespunzator token-ului contine o stare
		din grupul de stari NFA ce compun starea DFA data */

		for ((k, v) <- tokenToNfaMap) {
			for (nfaState <- nfaGroup) {
				if (v.getStates.contains(nfaState)) {
					return k
				}
			}
		}

		""
	}

	/* Imparte cuvantul dat intr-o lista de lexeme de tipul (lexema, token), respectiv intoarce un mesaj
	de eroare daca acest lucru nu este posibil */
	def lex(word: String): Either[String, List[(String, String)]] = {

		/* Creeaza un NFA nou compus din toate NFA-urile create, adaugand o noua stare initiala cu ε-tranzitii
		catre starile initiale ale acestor NFA-uri */

		var aut = Map[(Int, Boolean), Set[(Char, Int)]]()

		for ((_, v) <- tokenToNfaMap) {
			for (a <- v.Autom) {
				aut += a
			}
		}

		var transitions = Set[(Char, Int)]()
		for ((_, v) <- tokenToNfaMap) {
			transitions += (('ε', v.InitState))
		}

		aut += (0, false) -> transitions

		var newNfa = new Nfa[Int](aut, 0)

		/* Creeaza un DFA din NFA-ul creat */

		var dfaBuilder = new DfaBuilder[Int](newNfa, 1, x => x + 1)

		var dfa = dfaBuilder.genDfa

		var nfaGroupToDfaStateMap = dfaBuilder.NfaGroupToDfaState // Asocierea dintre un grup de stari din NFA si starea din DFA

		var lexemes = ListBuffer[(String, String)]()

		var k = 0 // Pozitia pana la care s-a gasit cel mai lung subsir

		while (k < word.length) { // Algoritmul continua pana s-au gasit subsiruri pana la finalul cuvantului

			var i = k // Indicele curent in cuvant; porneste de la k
			var currState = dfa.InitState // Starea curenta in DFA; porneste din starea initiala
			var finalState = null.asInstanceOf[Int] // Starea la care s-a ajuns obtinand cel mai lung subsir

			var longest = "" // Cel mai lung subsir ce satisface un regex
			var newK = k // Noul indice de la care va porni cautarea dupa gasirea unui subsir

			while (i < word.length) { // Se verifica existenta unui subsir pana la finalul cuvantului

				currState = dfa.next(currState, word(i)) // Trece in urmatoarea stare, consumand caracterul curent

				/* Daca exista tranzitie pe caracter si daca starea in care s-a ajuns este finala se actualizeaza
				cel mai lung subsir, retinand starea si pozitia la care acesta a fost gasit */
				if (currState != null.asInstanceOf[Int]) {

					if (dfa.isFinal(currState)) {
						longest = word.slice(k, i + 1) // Cel mai lung subsir, pornind de la pozitia k din cuvant
						finalState = currState
						newK = i + 1 // Noul indice va porni de la pozitia urmatoare
					}
				}
				/* Daca nu exista tranzitie pe caracter si anterior nu s-a ajuns la o stare finala se semnaleaza eroarea */
				else if (finalState == null.asInstanceOf[Int]) {
					return Left("No viable alternative at character " + i + ", line " + word.take(i).count(_ == '\n'))
				}

				i += 1
			}
			k = newK

			/* Daca nu s-a ajuns la o stare finala dupa parcurgerea intregului cuvant se semnaleaza eroarea */
			if (finalState == null.asInstanceOf[Int]) {
				return Left("No viable alternative at character EOF" + ", line " + word.count(_ == '\n'))
			}

			/* Adauga in lista lexemelor cel mai lung subsir gasit cu token-ul corespunzator starii finale in care s-a ajuns */
			lexemes += ((longest, getToken(nfaGroupToDfaStateMap, finalState)))
		}

		Right(lexemes.toList)
	}
}