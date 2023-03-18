package interpreter

import scala.collection.immutable

/*
 * VEUILLEZ INSCRIRE CI-DESSOUS VOTRE NOM ET VOTRE PRENOM :
 * 
 * ETUDIANT 1 :
 * 
 * ETUDIANT 2 :
 * 
 */

/**
 * définition d'une exception pour le cas des listes vides
 */
case object ExceptionListeVide extends Exception

/**
 * définition d'une exception pour le cas des listes de tailles différentes
 */
case object ExceptionListesDeLongueursDifferentes extends Exception

object Interpreter {

  /**
   * UN INTERPRETER POUR LE LANGAGE WHILE
   *
   */

  /**
   *  GESTION DE LA MEMOIRE DE L'INTERPRETEUR
   */

  /**
   *  définition d'un type Memory pour représenter une mémoire
   */
  type Memory = List[(Variable, Value)]

  /**
   * @param v : une variable
   * @param mem : une mémoire
   * @return m(v), c'est-à-dire la valeur de la variable v dans la mémoire mem,
   * la valeur par défaut si la variable v n'est pas présente dans la mémoire mem
   */
  def lookUp(v: Variable, mem: Memory): Value = {
    mem match {
      case head :: next => if(EqualsVar(head._1,v))head._2 else lookUp(v,next)
      case Nil => NlValue
    }
  }

  def EqualsVar(v1:Variable,v2:Variable):Boolean={
    v1 match {
      case Var(nameV1) => v2 match {
        case Var(nameV2) => nameV1.equals(nameV2)
      }
    }
  }

  /**
   * @param v : une variable
   * @param d : une valeur
   * @param mem : une mémoire
   * @return la mémoire modifiée par l'affectation [v->d]
   */
  def assign(v: Variable, d: Value, mem: Memory): Memory = {
    mem match {
      case head :: next => if(EqualsVar(head._1,v)){
        (v,d)::next
      }else head::assign(v,d,next)
      case Nil => (v,d)::Nil
    }
  }

  /**
   *  TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
   */

   def EqualsVal(v1: Value, v2: Value): Boolean = {
  (v1, v2) match {
    case (NlValue, NlValue) => true
    case (CstValue(n1), CstValue(n2)) => n1 == n2
    case (ConsValue(a1, b1), ConsValue(a2, b2)) => EqualsVal(a1, a2) && EqualsVal(b1, b2)
    case _ => false
  }
}

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return la valeur de l'expression
   */
  // TODO TP2
  def interpreterExpr(expression: Expression, mem: Memory): Value = {
    expression match {
      case Eq(arg1, arg2) => {
        if(EqualsVal(interpreterExpr(arg1,mem),interpreterExpr(arg2,mem)))ConsValue(NlValue,NlValue)
        else NlValue 
      }
      case Tl(arg) => {arg match {
        case Cons(arg1, arg2) => interpreterExpr(arg2,mem)
        case _ => {interpreterExpr(arg,mem) match {
          case ConsValue(arg1, arg2) => arg2
          case _ => NlValue
        }}
      }}
      case Cst(name) => CstValue(name)
      case Hd(arg) => {arg match {
        case Cons(arg1, arg2) => interpreterExpr(arg1,mem)
        case _ => {interpreterExpr(arg,mem) match {
          case ConsValue(arg1, arg2) => arg1
          case _ => NlValue
        }}
      }}
      case Nl => NlValue
      case VarExp(name) => lookUp(new Var(name),mem)
      case Cons(arg1, arg2) => ConsValue(interpreterExpr(arg1,mem),interpreterExpr(arg2,mem))
    }
  }

  /**
   * la fonction interpreterExpr ci-dessus calcule la valeur associée à une expression
   * il peut être utile de produire à l'inverse une expression associée à une valeur
   * la fonction valueToExpression ci-dessous construira l'expression la plus simple associée à une valeur
   *
   * @param value : une valeur du langage WHILE
   * @return l'AST décrivant l'expression de cette valeur
   */
  def valueToExpression(value: Value): Expression = {
    value match {
      case ConsValue(arg1, arg2) => Cons(valueToExpression(arg1),valueToExpression(arg2))
      case CstValue(name) => Cst(name)
      case NlValue => Nl
    }
  }
  

  /**
   *
   *  TRAITEMENT DES COMMANDES DU LANGAGE WHILE
   */

  /**
   * @param command : un AST décrivant une commande du langage WHILE
   * @param memory : une mémoire
   * @return la mémoire après l'interprétation de command
   */
  // TODO TP2
  def interpreterCommand(command: Command, memory: Memory): Memory = {
    command match {
      case Nop => memory
      case Set(variable, expression) => {
        val value = interpreterExpr(expression, memory)
        assign(variable, value, memory)
      }
      case While(condition, body) => {
        var mem = memory
        while (interpreterExpr(condition, mem) != NlValue) {
          for (cmd <- body) {
            mem = interpreterCommand(cmd, mem)
          }
        }
        mem
      }
      case For(count, body) => ???

      case If(condition, then_commands, else_commands) => {
        if (interpreterExpr(condition, memory) != NlValue) {
          interpreterCommands(then_commands, memory)
        } else {
          interpreterCommands(else_commands, memory)
        }
      }
    }
  }

  
  
  /**
   * @param commands : une liste non vide d'AST décrivant une liste non vide de commandes du langage WHILE
   * @param memory : une mémoire
   * @return la mémoire après l'interprétation de la liste de commandes
   */
  // TODO TP2
  def interpreterCommands(commands: List[Command], memory: Memory): Memory = {
    commands match {
      case Nil => throw ExceptionListeVide
      case head :: Nil => interpreterCommand(head, memory)
      case head :: next => interpreterCommands(next, interpreterCommand(head, memory))
    }
  }
  

  /**
   *
   *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
   */

  /**
   * @param vars : une liste non vide décrivant les variables d'entrée d'un programme du langage WHILE
   * @param vals : une liste non vide de valeurs
   * @return une mémoire associant chaque valeur à la variable d'entrée correspondant
   */
  // TODO TP2
  def interpreterMemorySet(vars: List[Variable], vals: List[Value]): Memory = {
    if (vars.length != vals.length) {
      throw ExceptionListesDeLongueursDifferentes
    } else if (vars.isEmpty || vals.isEmpty) {
      throw ExceptionListeVide
    }  else {
      vars.zip(vals)
    }
  }
  

  /**
   * @param vars : une liste non vide décrivant les variables de sortie d'un programme du langage WHILE
   * @param memory : une mémoire
   * @return la liste des valeurs des variables de sortie
   */
  // TODO TP2
  def interpreterMemoryGet(vars: List[Variable], memory: Memory): List[Value] = {
    if(vars.isEmpty) throw  ExceptionListeVide
  vars.map { v =>
    memory.find(_._1 == v) match {
      case Some((_, value)) => value
      case None => throw new Exception(s"Variable $v not found in memory")
    }
  }
}



  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param vals : une liste de valeurs
   * @return la liste des valeurs des variables de sortie
   */
  // TODO TP2
def interpreter(program: Program, vals: List[Value]): List[Value] = {

  program match {
    case Progr(in, body, out) => {val inputVars = in
  val inputMemory = interpreterMemorySet(inputVars, vals)
  val outputMemory = interpreterCommands(body, inputMemory)
  val outputVars = out
  interpreterMemoryGet(outputVars, outputMemory)
    }}
  }
  
  
  

  /**
   * UTILISATION D'UN ANALYSEUR SYNTAXIQUE POUR LE LANGAGE WHILE
   *
   * les 3 fonctions suivantes permettent de construire un arbre de syntaxe abstraite
   * respectivement pour une expression, une commande, un programme
   */

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une expression du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette expression
   */
  def readWhileExpression(s: String): Expression = { WhileParser.analyserexpression(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une commande du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette commande
   */
  def readWhileCommand(s: String): Command = { WhileParser.analysercommand(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'un programme du langage WHILE
   * @return un arbre de syntaxe abstraite pour ce programme
   */
  def readWhileProgram(s: String): Program = { WhileParser.analyserprogram(s) }

}