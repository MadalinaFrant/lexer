import scala.collection.mutable

object Regex {

	/* Verifica daca caracterul dat este un operator */
	private def isOperator(char: Char): Boolean = {
		if (char == '|' || char == '*' || char == '+' || char == '?' || char == '(' || char == ')') {
			true
		} else {
			false
		}
	}

	/* Intoarce prioritatea asociata operatorului */
	private def priority(op: Char): Int = {
		if (op == '*' || op == '+' || op == '?') {
			return 3
		}
		if (op == '.') {
			return 2
		}
		if (op == '|') {
			return 1
		}

		0
	}

	/* Intoarce sirul corespunzator operatorului dat, pentru forma prenex */
	private def opToString(op: Char): String = {
		if (op == '.') {
			return "CONCAT"
		}
		if (op == '|') {
			return "UNION"
		}
		if (op == '*') {
			return "STAR"
		}
		if (op == '+') {
			return "PLUS"
		}
		if (op == '?') {
			return "MAYBE"
		}

		""
	}

	/* Functia primeste regex-ul, sub forma unui string, si intoarce o lista in care caracterele
	care reprezinta operatori vor avea tipul Left, iar cele obisnuite tipul Right */
	private def preprocess(regex: String): List[Either[Char, Char]] = {
		var input = regex

		// Se inlocuiesc syntactic sugars cu expresiile corespondente
		input = input.replaceAll("\\[0-9]", "(0|1|2|3|4|5|6|7|8|9)")
		input = input.replaceAll("\\[a-z]", "(a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z)")
		input = input.replaceAll("\\[A-Z]", "(A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z)")

		// Operatorii vor fi de tipul Left, iar operanzii de tipul Right
		val expr = mutable.ListBuffer[Either[Char, Char]]()

		var i = 0
		while (i < input.length) { // se itereaza prin intregul sir
			val c = input(i) // caracterul curent

			if (isOperator(c)) { // caracterul este un operator, deci va fi adaugat in lista cu tipul Left
				expr += Left(c)
			} else if (c == '\'') { // caracterul curent este o ghilimea, deci urmeaza un caracter escapat
				if (input(i + 1) == '\\') { // se trateaza cazul unui caracter special compus din caracterul '\', pentru a fi escapat corect
					input(i + 2) match {
						case 'n' => expr += Right('\n')
						case 't' => expr += Right('\t')
					}
					i += 3 // se sare peste a doua ghilimea
				} else {
					expr += Right(input(i + 1)) // se adauga caracterul escapat, cu tipul Right (rol de caracter obisnuit)
					i += 2 // se sare peste a doua ghilimea
				}
			} else if (((i + 2) < input.length) && (c == 'e') && (input(i + 1) == 'p') && (input(i + 2) == 's')) {
				expr += Right('ε') // daca este gasit cuvantul cheie "eps", se adauga simbolul corespunzator
				i += 2
			} else { // caracter obisnuit
				expr += Right(c)
			}

			i += 1
		}

		addConcatChars(expr)
	}

	/* Intrucat operatia de concatenare nu are asociata niciun simbol, se va adauga caracterul '.' pentru a
	marca unde trebuie sa aiba loc o astfel de operatie */
	private def addConcatChars(expr: mutable.ListBuffer[Either[Char, Char]]): List[Either[Char, Char]] = {

		var i = 0
		while (i < (expr.length - 1)) {
			/* Intre 2 caractere obisnuite */
			if (expr(i).isRight && expr(i + 1).isRight) {
				expr.insert(i + 1, Left('.'))
			}
			/* Intre ')' si un caracter obisnuit */
			if (expr(i).left.getOrElse(' ') == ')' && expr(i + 1).isRight) {
				expr.insert(i + 1, Left('.'))
			}
			/* Intre un caracter obisnuit si '(' */
			if (expr(i).isRight && expr(i + 1).left.getOrElse(' ') == '(') {
				expr.insert(i + 1, Left('.'))
			}
			/* Intre ')' si '(' */
			if (expr(i).left.getOrElse(' ') == ')' && expr(i + 1).left.getOrElse(' ') == '(') {
				expr.insert(i + 1, Left('.'))
			}
			/* Intre un operator '*' sau '+' sau '?' si un caracter obisnuit */
			if ("*+?".contains(expr(i).left.getOrElse(' ')) && expr(i + 1).isRight) {
				expr.insert(i + 1, Left('.'))
			}
			/* Intre un operator '*' sau '+' sau '?' si '(' */
			if ("*+?".contains(expr(i).left.getOrElse(' ')) && expr(i + 1).left.getOrElse(' ') == '(') {
				expr.insert(i + 1, Left('.'))
			}

			i += 1
		}

		expr.toList
	}

	/* Construieste o expresie prenex din regex-ul dat, aplicand algoritmul shunting yard */
	def toPrenex(regex: String): String = {
		val prenex = mutable.ListBuffer[Either[Char, Char]]()
		val stack = mutable.Stack[Char]() // stiva in care se vor pastra operatorii

		for (c <- preprocess(regex).reverse) {
			if (c.isRight) { // Un operand se adauga direct in prenex
				prenex += c
			}
			/* In cazul unei paranteze deschise se scot din stiva operatorii, adaugandu-i in prenex,
			pana este gasita paranteza corespunzatoare */
			else if (c.left.getOrElse(' ') == '(') {
				var fromStack = stack.pop()
				while (fromStack != ')') {
					prenex += Left(fromStack)
					fromStack = stack.pop()
				}
			}
			else if (c.left.getOrElse(' ') == ')') { // O paranteza inchisa se adauga direct in stiva
				stack.push(c.left.getOrElse(' '))
			}
			else if (c.isLeft) { // operator, dar nu paranteza
				if (stack.isEmpty) { // In cazul unei stive goale se adauga direct in aceasta
					stack.push(c.left.getOrElse(' '))
				}
				else { // Altfel, operatorii trebuie adaugati conform prioritatilor
					/* Daca prioritatea operatorului curent este mai mare sau egala cu varful stivei,
					se poate adauga in aceasta */
					if (priority(c.left.getOrElse(' ')) >= priority(stack.top)) {
						stack.push(c.left.getOrElse(' '))
					}
					/* Daca prioritatea operatorului curent este mai mica decat varful stivei, atunci se
					vor scoate operatorii din stiva si adaugati in prenex, pana se gaseste un operator cu
					prioritate mai mica decat cea a operatorului curent (sau egala), sau stiva devine goala */
					else {
						prenex += Left(stack.pop())
						while (stack.nonEmpty && (priority(c.left.getOrElse(' ')) < priority(stack.top))) {
							prenex += Left(stack.pop())
						}
						stack.push(c.left.getOrElse(' '))
					}

				}
			}

		}

		while (stack.nonEmpty) { // Se adauga in prenex operatorii ramasi in stiva
			prenex += Left(stack.pop())
		}

		var prenexStr = ""

		for (c <- prenex.reverse) {
			if (c.isLeft) { // In cazul unui operator, in forma finala a prenex-ului se va adauga string-ul asociat acestuia
				prenexStr += opToString(c.left.getOrElse(' '))
				prenexStr += ' '
			} else { // Un caracter obisnuit va fi adaugat intre ghilimele
				prenexStr += '\''
				prenexStr += c.getOrElse(' ')
				prenexStr += '\''
				prenexStr += ' '
			}
		}

		prenexStr
	}
}
