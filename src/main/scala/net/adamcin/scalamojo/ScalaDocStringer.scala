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
import reflect.NameTransformer

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

  def kindToString(mbr: MemberEntity) =
    mbr match {
      case c: Class => if (c.isCaseClass) "case class" else "class"
      case _: Trait => "trait"
      case _: Package => "package"
      case _: Object => "object"
      case _: AbstractType => "type"
      case _: AliasType => "type"
      case _: Constructor => "new"
      case v: Def => "def"
      case v: Val if (v.isLazyVal) => "lazy val"
      case v: Val if (v.isVal) => "val"
      case v: Val if (v.isVar) => "var"
      case _ => sys.error("Cannot create kind for: " + mbr + " of class " + mbr.getClass)
    }

  def templateToPath(tpl: TemplateEntity): List[String] = {
    def doName(tpl: TemplateEntity): String =
      (if (tpl.inPackageObject) "package$$" else "") + NameTransformer.encode(tpl.name) + (if (tpl.isObject) "$" else "")
    def downPacks(pack: Package): List[String] =
      if (pack.isRootPackage) Nil else (doName(pack) :: downPacks(pack.inTemplate))
    def downInner(nme: String, tpl: TemplateEntity): (String, Package) = {
      tpl.inTemplate match {
        case inPkg: Package => (nme + ".html", inPkg)
        case inTpl => downInner(doName(inTpl) + "$" + nme, inTpl)
      }
    }
    val (file, pack) =
      tpl match {
        case p: Package => ("package.html", p)
        case _ => downInner(doName(tpl), tpl)
      }
    file :: downPacks(pack)
  }

  /** A relative link from this page to some destination class entity.
    * @param destClass The class or object entity that the link will point to. */
  def relativeLinkTo(destClass: TemplateEntity): String =
    relativeLinkTo(templateToPath(destClass))

  /** A relative link from this page to some destination path.
    * @param destPath The path that the link will point to. */
  def relativeLinkTo(destPath: List[String]): String = {
    destPath.reverse.mkString("/")
  }

  def inlineToHtml(inl: Inline): NodeSeq = inl match {
    case Chain(items) => items flatMap (inlineToHtml(_))
    case Italic(in) => <i>{ inlineToHtml(in) }</i>
    case Bold(in) => <b>{ inlineToHtml(in) }</b>
    case Underline(in) => <u>{ inlineToHtml(in) }</u>
    case Superscript(in) => <sup>{ inlineToHtml(in) }</sup>
    case Subscript(in) => <sub>{ inlineToHtml(in) }</sub>
    case Link(raw, title) => <a href={ raw } target="_blank">{ inlineToHtml(title) }</a>
    case Monospace(in) => <code>{ inlineToHtml(in) }</code>
    case Text(text) => scala.xml.Text(text)
    case Summary(in) => inlineToHtml(in)
    case HtmlTag(tag) => scala.xml.Unparsed(tag)
    case EntityLink(target, link) => linkToHtml(target, link, hasLinks = true)
  }

  def linkToHtml(text: Inline, link: LinkTo, hasLinks: Boolean) = link match {
    case LinkToTpl(dtpl: TemplateEntity) =>
      if (hasLinks)
        <a href={ relativeLinkTo(dtpl) } class="extype" name={ dtpl.qualifiedName }>{ inlineToHtml(text) }</a>
      else
        <span class="extype" name={ dtpl.qualifiedName }>{ inlineToHtml(text) }</span>
    case LinkToMember(mbr: MemberEntity, inTpl: TemplateEntity) =>
      if (hasLinks)
        <a href={ relativeLinkTo(inTpl) + "#" + mbr.signature } class="extmbr" name={ mbr.qualifiedName }>{ inlineToHtml(text) }</a>
      else
        <span class="extmbr" name={ mbr.qualifiedName }>{ inlineToHtml(text) }</span>
    case Tooltip(tooltip) =>
      <span class="extype" name={ tooltip }>{ inlineToHtml(text) }</span>
    case LinkToExternal(name, url) =>
      <a href={ url } class="extype" target="_top">{ inlineToHtml(text) }</a>
    case _ =>
      inlineToHtml(text)
  }

  def typeToHtml(tpes: List[TypeEntity], hasLinks: Boolean): NodeSeq = tpes match {
    case Nil =>
      NodeSeq.Empty
    case List(tpe) =>
      typeToHtml(tpe, hasLinks)
    case tpe :: rest =>
      typeToHtml(tpe, hasLinks) ++ scala.xml.Text(" with ") ++ typeToHtml(rest, hasLinks)
  }

  def typeToHtml(tpe: TypeEntity, hasLinks: Boolean): NodeSeq = {
    val string = tpe.name
    def toLinksOut(inPos: Int, starts: List[Int]): NodeSeq = {
      if (starts.isEmpty && (inPos == string.length))
        NodeSeq.Empty
      else if (starts.isEmpty)
        scala.xml.Text(string.slice(inPos, string.length))
      else if (inPos == starts.head)
        toLinksIn(inPos, starts)
      else {
        scala.xml.Text(string.slice(inPos, starts.head)) ++ toLinksIn(starts.head, starts)
      }
    }
    def toLinksIn(inPos: Int, starts: List[Int]): NodeSeq = {
      val (link, width) = tpe.refEntity(inPos)
      val text = comment.Text(string.slice(inPos, inPos + width))
      linkToHtml(text, link, hasLinks) ++ toLinksOut(inPos + width, starts.tail)
    }
    if (hasLinks)
      toLinksOut(0, tpe.refEntity.keySet.toList)
    else
      scala.xml.Text(string)
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
  def templateToHtml(tpl: TemplateEntity, name: String = null) = tpl match {
    case dTpl: DocTemplateEntity =>
      if (hasPage(dTpl)) {
        <a href={ relativeLinkTo(dTpl) } class="extype" name={ dTpl.qualifiedName }>{ if (name eq null) dTpl.name else name }</a>
      } else {
        scala.xml.Text(if (name eq null) dTpl.name else name)
      }
    case ndTpl: NoDocTemplate =>
      scala.xml.Text(if (name eq null) ndTpl.name else name)
  }

  /** Returns the HTML code that represents the templates in `tpls` as a list of hyperlinked names. */
  def templatesToHtml(tplss: List[TemplateEntity], sep: NodeSeq): NodeSeq = tplss match {
    case Nil         => NodeSeq.Empty
    case tpl :: Nil  => templateToHtml(tpl)
    case tpl :: tpls => templateToHtml(tpl) ++ sep ++ templatesToHtml(tpls, sep)
  }

  /** Returns the _big image name corresponding to the DocTemplate Entity (upper left icon) */
  def docEntityKindToBigImage(ety: DocTemplateEntity) =
    if (ety.isTrait && !ety.companion.isEmpty && ety.companion.get.visibility.isPublic && ety.companion.get.inSource != None) "trait_to_object_big.png"
    else if (ety.isTrait) "trait_big.png"
    else if (ety.isClass && !ety.companion.isEmpty && ety.companion.get.visibility.isPublic && ety.companion.get.inSource != None) "class_to_object_big.png"
    else if (ety.isClass) "class_big.png"
    else if ((ety.isAbstractType || ety.isAliasType) && !ety.companion.isEmpty && ety.companion.get.visibility.isPublic && ety.companion.get.inSource != None) "type_to_object_big.png"
    else if ((ety.isAbstractType || ety.isAliasType)) "type_big.png"
    else if (ety.isObject && !ety.companion.isEmpty && ety.companion.get.visibility.isPublic && ety.companion.get.inSource != None && ety.companion.get.isClass) "object_to_class_big.png"
    else if (ety.isObject && !ety.companion.isEmpty && ety.companion.get.visibility.isPublic && ety.companion.get.inSource != None && ety.companion.get.isTrait) "object_to_trait_big.png"
    else if (ety.isObject && !ety.companion.isEmpty && ety.companion.get.visibility.isPublic && ety.companion.get.inSource != None && (ety.companion.get.isAbstractType || ety.companion.get.isAliasType)) "object_to_trait_big.png"
    else if (ety.isObject) "object_big.png"
    else if (ety.isPackage) "package_big.png"
    else "class_big.png"  // FIXME: an entity *should* fall into one of the above categories, but AnyRef is somehow not
}