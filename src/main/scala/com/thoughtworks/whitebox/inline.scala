package com.thoughtworks.whitebox

import scala.annotation.StaticAnnotation
import scala.reflect.macros.{TypecheckException, whitebox}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class inline extends StaticAnnotation {

  inline def apply(method: Any): Any = meta {
    import scala.meta._

    val Defn.Def(mods, methodName, tparams, paramss, decltpe, body) = method
    for (tparam <- tparams) {
      abort(tparam.pos, "@whitebox.inline does not support type parameter")
    }
    val expandMethodName = Term.Name(s"expand${paramss.length}")
    val macroArguments = for (i <- paramss.indices) yield {
      collection.immutable.Seq(param"${Term.Name(s"arguments$i")}: _root_.scala.Any*")
    }
    q"""
      import scala.language.experimental.macros
      @_root_.com.thoughtworks.whitebox.inline.methodBody(${Lit(method.syntax)})
      def $methodName(...$macroArguments): _root_.scala.Any = macro _root_.com.thoughtworks.whitebox.inline.Macros.$expandMethodName
    """
  }

}

object inline {

  final class methodBody(code: String) extends StaticAnnotation

  final class Macros(val c: whitebox.Context) {

    import c.universe._

    final class ThisTransformer(prefix: Tree) extends Transformer {
      override def transform(tree: Tree): Tree = {
        tree match {
          case This(typeNames.EMPTY) =>
            prefix
          case _: ClassDef | _: ModuleDef =>
            tree
          case _ =>
            super.transform(tree)
        }
      }
    }

    private val methodBodySymbol = typeOf[methodBody].typeSymbol

    private def expand(): Tree = {
      val q"$methodTree(...$argumentLists)" = c.macroApplication
      val Some(inlineTree) = methodTree.symbol.annotations.iterator.map(_.tree).collectFirst {
        case q"""new $annotationClass(${code: String})""" if annotationClass.symbol == methodBodySymbol =>
          // TODO: provide pos when parsing code
          c.parse(code) match {
            case q"def $methodName(...$argumentDefLists): $returnType = $body" =>
              val argumentAssignments =
                (for {
                  (arguments, argumentDefs) <- argumentLists.view.zip(argumentDefLists)
                  i <- {
                    for (additionalArgument <- arguments.view(argumentDefs.length, arguments.length)) {
                      c.error(additionalArgument.pos, s"Too many arguments for method $methodName")
                    }
                    argumentDefs.indices
                  }
                } yield {
                  val ValDef(mods, argumentName, tpt, rhs) = argumentDefs(i)
                  val argument = if (i < arguments.length) {
                    arguments(i)
                  } else {
                    rhs
                  }
                  q"""val $argumentName = ${try {
                    c.typecheck(argument, pt = c.typecheck(tpt, mode = c.TYPEmode).tpe)
                  } catch {
                    case e: TypecheckException =>
                      c.error(e.pos.asInstanceOf[Position], e.msg)
                      argument
                  }}"""
                }).force
              val inlineTree = methodTree match {
                case q"$prefix.$methodName" =>
                  val thisTransformer = new ThisTransformer(prefix)
                  val thisName = TermName(c.freshName("this"))
                  q"""
                    ..$argumentAssignments
                    val $thisName = $prefix
                    import $thisName._
                    ${thisTransformer.transform(body)}
                  """
                case _ =>
                  q"""
                    ..$argumentAssignments
                    $body
                  """
              }

              if (returnType.isEmpty) {
                inlineTree
              } else {
                try {
                  c.typecheck(inlineTree, pt = c.typecheck(returnType, mode = c.TYPEmode).tpe)
                } catch {
                  case e: TypecheckException =>
                    c.error(e.pos.asInstanceOf[Position], e.msg)
                    inlineTree
                }
              }

          }
      }
      inlineTree

    }

    def expand0: Tree = {
      expand()
    }

    def expand1(arguments0: Tree*): Tree = {
      expand()
    }

    def expand2(arguments0: Tree*)(arguments1: Tree*): Tree = {
      expand()
    }

    def expand3(arguments0: Tree*)(arguments1: Tree*)(arguments2: Tree*): Tree = {
      expand()
    }

    def expand4(arguments0: Tree*)(arguments1: Tree*)(arguments2: Tree*)(arguments3: Tree*): Tree = {
      expand()
    }

    def expand5(arguments0: Tree*)(arguments1: Tree*)(arguments2: Tree*)(arguments3: Tree*)(arguments4: Tree*): Tree = {
      expand()
    }

    def expand6(arguments0: Tree*)(arguments1: Tree*)(arguments2: Tree*)(arguments3: Tree*)(arguments4: Tree*)(arguments5: Tree*): Tree = {
      expand()
    }

    def expand7(arguments0: Tree*)(arguments1: Tree*)(arguments2: Tree*)(arguments3: Tree*)(arguments4: Tree*)(arguments5: Tree*)(arguments6: Tree*): Tree = {
      expand()
    }

    def expand8(arguments0: Tree*)(arguments1: Tree*)(arguments2: Tree*)(arguments3: Tree*)(arguments4: Tree*)(arguments5: Tree*)(arguments6: Tree*)(arguments7: Tree*): Tree = {
      expand()
    }

    def expand9(arguments0: Tree*)(arguments1: Tree*)(arguments2: Tree*)(arguments3: Tree*)(arguments4: Tree*)(arguments5: Tree*)(arguments6: Tree*)(arguments7: Tree*)(arguments8: Tree*): Tree = {
      expand()
    }

    def expand10(arguments0: Tree*)(arguments1: Tree*)(arguments2: Tree*)(arguments3: Tree*)(arguments4: Tree*)(arguments5: Tree*)(arguments6: Tree*)(arguments7: Tree*)(arguments8: Tree*)(arguments9: Tree*): Tree = {
      expand()
    }
  }

}
