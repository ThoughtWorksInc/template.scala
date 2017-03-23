package com.thoughtworks

import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox
import scala.collection.immutable.Queue
import scala.language.experimental.macros

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class template extends StaticAnnotation {

  inline def apply(method: Any): Any = meta {
    import scala.meta._

    method match {
      case Defn.Def(mods, methodName, tparams, paramss, decltpe, body) =>
        for (tparam <- tparams) {
          abort(tparam.pos, "@template does not support type parameter")
        }
        val expandMethodName = Term.Name(s"expand${paramss.length}")
        val macroArguments = for (i <- paramss.indices) yield {
          collection.immutable.Seq(param"${Term.Name(s"arguments$i")}: _root_.scala.Any*")
        }
        q"""
          import scala.language.experimental.macros
          @_root_.com.thoughtworks.template.methodBody(${Lit(method.syntax)})
          def $methodName(...$macroArguments): ${decltpe.getOrElse(t"_root_.scala.Any")} = macro _root_.com.thoughtworks.template.Macros.$expandMethodName
        """
      case _ =>
        abort("@template must be set on methods")
    }

  }

}

object template {

  def the[T](t: T): T = macro Macros.the

  final class methodBody(code: String) extends StaticAnnotation

  final class Macros(val c: whitebox.Context) {

    import c.universe._

    def the(t: Tree): Tree = t

    private[Macros] class CallByNameTransformer(protected val callByNames: Map[TermName, Tree]) extends Transformer {

      final class Extractor {
        def unapply(name: TermName) = {
          callByNames.get(name.decodedName.toTermName)
        }
      }

      def exclude(name: TermName): CallByNameTransformer = {
        new CallByNameTransformer(callByNames - name)
      }

      override def transform(tree: Tree): Tree = {
        val extractor = new Extractor

        tree match {
          case Ident(extractor(replacedTo)) =>
            replacedTo
          case Block(defs, value) =>
            def loop(defs: List[Tree],
                     transformer: CallByNameTransformer,
                     acc: Queue[Tree]): (Transformer, Queue[Tree]) = {
              defs match {
                case Nil =>
                  (transformer, acc)
                case head :: tail =>
                  head match {
                    case valOrDefDef: ValOrDefDef if callByNames.contains(valOrDefDef.name.decodedName.toTermName) =>
                      val nextTransformer = transformer.exclude(valOrDefDef.name.decodedName.toTermName)
                      loop(tail, nextTransformer, acc :+ nextTransformer.transform(valOrDefDef))
                    case _ =>
                      loop(tail, transformer, acc :+ transformer.transform(head))
                  }
              }
            }
            val (transformer, transformedDefs) = loop(defs, this, Queue.empty)
            Block(transformedDefs.toList, transformer.transform(value))
          case _ =>
            super.transform(tree)
        }
      }

    }

    private[Macros] final class ThisTransformer(prefix: Tree, callByNames0: Map[TermName, Tree])
        extends CallByNameTransformer(callByNames0) {

      override def exclude(name: TermName): ThisTransformer = {
        new ThisTransformer(prefix, callByNames - name)
      }

      override def transform(tree: Tree): Tree = {
        tree match {
          case _: ClassDef | _: ModuleDef =>
            new CallByNameTransformer(callByNames).transform(tree)
          case This(typeNames.EMPTY) =>
            prefix
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
              val (argumentAssignments, callByNameArguments) =
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

                  (argumentName, tpt, argument)
                }).foldRight[(List[Tree], Map[TermName, Tree])](Nil, Map.empty) { (argumentMappings, pair) =>
                  val (argumentAssignments, callByNameArguments) = pair
                  val (argumentName, tpt, argument) = argumentMappings
                  tpt match {
                    case tq"_root_.scala.${TypeName("<byname>")}[$constrait]" =>
                      (argumentAssignments,
                       callByNameArguments.updated(argumentName.decodedName.toTermName,
                                                   q"_root_.com.thoughtworks.template.the[$constrait]($argument)"))
                    case _ =>
                      (q"final val $argumentName = _root_.com.thoughtworks.template.the[$tpt]($argument)" :: argumentAssignments,
                       callByNameArguments)
                  }
                }

              def checkReturnType(returnTree: Tree): Tree = {
                if (returnType.isEmpty) {
                  returnTree
                } else {
                  q"_root_.com.thoughtworks.template.the[$returnType]($returnTree)"
                }
              }
              methodTree match {
                case q"$prefix.$methodName" =>
                  val thisTransformer = new ThisTransformer(prefix, callByNameArguments)
                  val thisName = TermName(c.freshName("this"))
                  q"""
                    final val $thisName = $prefix
                    import $thisName._
                    ..$argumentAssignments
                    ${checkReturnType(thisTransformer.transform(body))}
                  """
                case _ =>
                  val callByNameTransformer = new CallByNameTransformer(callByNameArguments)
                  q"""
                    ..$argumentAssignments
                    ${checkReturnType(callByNameTransformer.transform(body))}
                  """
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

    def expand6(arguments0: Tree*)(arguments1: Tree*)(arguments2: Tree*)(arguments3: Tree*)(arguments4: Tree*)(
        arguments5: Tree*): Tree = {
      expand()
    }

    def expand7(arguments0: Tree*)(arguments1: Tree*)(arguments2: Tree*)(arguments3: Tree*)(arguments4: Tree*)(
        arguments5: Tree*)(arguments6: Tree*): Tree = {
      expand()
    }

    def expand8(arguments0: Tree*)(arguments1: Tree*)(arguments2: Tree*)(arguments3: Tree*)(arguments4: Tree*)(
        arguments5: Tree*)(arguments6: Tree*)(arguments7: Tree*): Tree = {
      expand()
    }

    def expand9(arguments0: Tree*)(arguments1: Tree*)(arguments2: Tree*)(arguments3: Tree*)(arguments4: Tree*)(
        arguments5: Tree*)(arguments6: Tree*)(arguments7: Tree*)(arguments8: Tree*): Tree = {
      expand()
    }

    def expand10(arguments0: Tree*)(arguments1: Tree*)(arguments2: Tree*)(arguments3: Tree*)(arguments4: Tree*)(
        arguments5: Tree*)(arguments6: Tree*)(arguments7: Tree*)(arguments8: Tree*)(arguments9: Tree*): Tree = {
      expand()
    }
  }

}
