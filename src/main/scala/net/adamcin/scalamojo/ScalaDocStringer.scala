/*
 * This is free and unencumbered software released into the public domain.
 *
 * Anyone is free to copy, modify, publish, use, compile, sell, or
 * distribute this software, either in source code form or as a compiled
 * binary, for any purpose, commercial or non-commercial, and by any
 * means.
 *
 * In jurisdictions that recognize copyright laws, the author or authors
 * of this software dedicate any and all copyright interest in the
 * software to the public domain. We make this dedication for the benefit
 * of the public at large and to the detriment of our heirs and
 * successors. We intend this dedication to be an overt act of
 * relinquishment in perpetuity of all present and future rights to this
 * software under copyright law.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 * For more information, please refer to <http://unlicense.org/>
 */

package net.adamcin.scalamojo

import tools.nsc.doc.model._
import tools.nsc.doc.model.comment._

import xml.NodeSeq

/**
 * Methods useful for generating html strings from text in scaladoc comments, mostly
 * ripped straight from the ScalaDoc tool's HTML generator logic
 * @since 0.6.0
 * @author Mark Adamcin
 */
object ScalaDocStringer {

  def commentInheritor(member: MemberEntity)(getter: Option[Comment] => Option[String]): Option[String] = {
    getter(member.comment) match {
      case Some(value) => Some(value)
      case None => {
        member.inDefinitionTemplates match {
          case Nil => None
          case tmpl :: tmpls => {
            if (tmpl.isDocTemplate) {
              tmpl.asInstanceOf[DocTemplateEntity].members.find { _.name == member.name } match {
                case Some(inherited) => getter(inherited.comment)
                case None => None
              }
            } else {
              None
            }
          }
        }
      }
    }
  }

  def getDescription(comment: Option[Comment]): Option[String] = {
    comment match {
      case None => None
      case Some(c) => Option(toHtmlString(bodyToHtml(c.body)))
    }
  }

  def getSince(comment: Option[Comment]): Option[String] = {
    comment match {
      case None => None
      case Some(c) => {
        c.since match {
          case None => None
          case Some(since) => {
            val sinceNodes = since.blocks.flatMap {
              (block) => block match {
                case Paragraph(in) => Option(inlineToHtml(in))
                case Code(data) => Option(xml.Text(data))
                case _ => None
              }
            }.flatten
            Option(toHtmlString(sinceNodes))
          }
        }
      }
    }
  }

  def getDeprecated(traitDef: Trait): Option[String] = {
    traitDef.deprecation match {
      case Some(deprecation) => Option(toHtmlString(bodyToHtml(deprecation)))
      case None => traitDef.comment match {
        case None => None
        case Some(comment) => comment.deprecated match {
          case None => None
          case Some(deprecated) => Option(toHtmlString(bodyToHtml(deprecated)))
        }
      }
    }
  }

  def getDeprecated(member: MemberEntity): Option[String] = {
    member.deprecation match {
      case Some(deprecation) => Option(toHtmlString(bodyToHtml(deprecation)))
      case None => member.comment match {
        case None => None
        case Some(comment) => comment.deprecated match {
          case None => None
          case Some(deprecated) => Option(toHtmlString(bodyToHtml(deprecated)))
        }
      }
    }
  }

  def toHtmlString(el: NodeSeq): String = xml.Xhtml.toXhtml(el)

  /** Transforms an optional comment into an styled HTML tree representing its body if it is defined, or into an empty
    * node sequence if it is not. */
  def commentToHtml(comment: Option[Comment]): NodeSeq =
    (comment map (commentToHtml(_))) getOrElse NodeSeq.Empty

  /** Transforms a comment into an styled HTML tree representing its body. */
  def commentToHtml(comment: Comment): NodeSeq =
    bodyToHtml(comment.body)

  def bodyToHtml(body: Body): NodeSeq =
    body.blocks flatMap (blockToHtml(_))

  def blockToHtml(block: Block): NodeSeq = block match {
    case Title(in, 1) => <h3>{ inlineToHtml(in) }</h3>
    case Title(in, 2) => <h4>{ inlineToHtml(in) }</h4>
    case Title(in, 3) => <h5>{ inlineToHtml(in) }</h5>
    case Title(in, _) => <h6>{ inlineToHtml(in) }</h6>
    case Paragraph(in) => <div>{ inlineToHtml(in) }</div>
    case Code(data) =>
      <pre>{ xml.Text(data) }</pre>
    case UnorderedList(items) =>
      <ul>{ listItemsToHtml(items) }</ul>
    case OrderedList(items, listStyle) =>
      <ol class={ listStyle }>{ listItemsToHtml(items) }</ol>
    case DefinitionList(items) =>
      <dl>{items map { case (t, d) => <dt>{ inlineToHtml(t) }</dt><dd>{ blockToHtml(d) }</dd> } }</dl>
    case HorizontalRule() =>
      <hr/>
  }

  def listItemsToHtml(items: Seq[Block]) =
    items.foldLeft(xml.NodeSeq.Empty){ (xmlList, item) =>
      item match {
        case OrderedList(_, _) | UnorderedList(_) =>  // html requires sub ULs to be put into the last LI
          xmlList.init ++ <li>{ xmlList.last.child ++ blockToHtml(item) }</li>
        case Paragraph(inline) =>
          xmlList :+ <li>{ inlineToHtml(inline) }</li>  // LIs are blocks, no need to use Ps
        case block =>
          xmlList :+ <li>{ blockToHtml(block) }</li>
      }
  }

  def inlineToHtml(inl: Inline): NodeSeq = inl match {
    case Chain(items) => items flatMap (inlineToHtml(_))
    case Italic(in) => <i>{ inlineToHtml(in) }</i>
    case Bold(in) => <b>{ inlineToHtml(in) }</b>
    case Underline(in) => <u>{ inlineToHtml(in) }</u>
    case Superscript(in) => <sup>{ inlineToHtml(in) }</sup>
    case Subscript(in) => <sub>{ inlineToHtml(in) }</sub>
    case Link(raw, title) => <a href={ raw }>{ inlineToHtml(title) }</a>
    case EntityLink(entity) => templateToHtml(entity)
    case Monospace(in) => <code>{ inlineToHtml(in) }</code>
    case Text(text) => xml.Text(text)
    case Summary(in) => inlineToHtml(in)
    case HtmlTag(tag) => xml.Unparsed(tag)
  }

  def typeToHtml(tpe: TypeEntity, hasLinks: Boolean): NodeSeq = {
    val string = tpe.name
    def toLinksOut(inPos: Int, starts: List[Int]): NodeSeq = {
      if (starts.isEmpty && (inPos == string.length))
        NodeSeq.Empty
      else if (starts.isEmpty)
        xml.Text(string.slice(inPos, string.length))
      else if (inPos == starts.head)
        toLinksIn(inPos, starts)
      else {
        xml.Text(string.slice(inPos, starts.head)) ++ toLinksIn(starts.head, starts)
      }
    }
    def toLinksIn(inPos: Int, starts: List[Int]): NodeSeq = {
      val (tpl, width) = tpe.refEntity(inPos)
      (tpl match {
        case dtpl:DocTemplateEntity if hasLinks =>
          <span class="extype" name={ tpl.qualifiedName }>{ string.slice(inPos, inPos + width) }</span>
          /*<a href={ relativeLinkTo(dtpl) } class="extype" name={ dtpl.qualifiedName }>{
            string.slice(inPos, inPos + width)
          }</a>*/
        case tpl =>
          <span class="extype" name={ tpl.qualifiedName }>{ string.slice(inPos, inPos + width) }</span>
      }) ++ toLinksOut(inPos + width, starts.tail)
    }
    if (hasLinks)
      toLinksOut(0, tpe.refEntity.keySet.toList)
    else
      xml.Text(string)
  }

  def typesToHtml(tpess: List[TypeEntity], hasLinks: Boolean, sep: NodeSeq): NodeSeq = tpess match {
    case Nil         => NodeSeq.Empty
    case tpe :: Nil  => typeToHtml(tpe, hasLinks)
    case tpe :: tpes => typeToHtml(tpe, hasLinks) ++ sep ++ typesToHtml(tpes, hasLinks, sep)
  }

  def hasPage(e: DocTemplateEntity) = {
    e.isPackage || e.isTrait || e.isClass || e.isObject || e.isCaseClass
  }

  /** Returns the HTML code that represents the template in `tpl` as a hyperlinked name. */
  def templateToHtml(tpl: TemplateEntity) = tpl match {
    case dTpl: DocTemplateEntity =>
      if (hasPage(dTpl)) {
        xml.Text(dTpl.name)
        //<a href={ relativeLinkTo(dTpl) } class="extype" name={ dTpl.qualifiedName }>{ dTpl.name }</a>
      } else {
        xml.Text(dTpl.name)
      }
    case ndTpl: NoDocTemplate =>
      xml.Text(ndTpl.name)
  }

  /** Returns the HTML code that represents the templates in `tpls` as a list of hyperlinked names. */
  def templatesToHtml(tplss: List[TemplateEntity], sep: NodeSeq): NodeSeq = tplss match {
    case Nil         => NodeSeq.Empty
    case tpl :: Nil  => templateToHtml(tpl)
    case tpl :: tpls => templateToHtml(tpl) ++ sep ++ templatesToHtml(tpls, sep)
  }
}