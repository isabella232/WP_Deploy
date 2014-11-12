package com.whitepages.cmdb.ui

import com.vaadin.ui._
import com.persist.JsonOps._
import com.vaadin.data.Property
import com.vaadin.data.Property.ValueChangeEvent
import scala.language.postfixOps

case class UIUtil(set: (Component) => Unit) {

  val BORDER = 10

  def empty = {
    val l = new Label("")
    l.setWidth(0, com.vaadin.server.Sizeable.Unit.PIXELS)
    l.setHeight(0, com.vaadin.server.Sizeable.Unit.PIXELS)
    l
  }

  def xSpace(x: Int = BORDER) = {
    val l = new Label("")
    l.setWidth(x, com.vaadin.server.Sizeable.Unit.PIXELS)
    l
  }

  def ySpace(y: Int = BORDER) = {
    val l = new Label("")
    l.setHeight(y, com.vaadin.server.Sizeable.Unit.PIXELS)
    l
  }

  def button(name: String, act: => Unit) = {
    val b = new Button(name)
    b.addClickListener(
      new Button.ClickListener {
        def buttonClick(e: Button.ClickEvent) = {
          act
        }
      }
    )
    b
  }

  def vertical(border: Int = 0)(body: Component*): Component = {
    val v = new VerticalLayout()
    if (border != 0) v.addComponent(ySpace(border))
    for (b <- body) {
      v.addComponent(b)
    }
    if (border != 0) v.addComponent(ySpace(border))
    v
  }

  def horizontal(border: Int = 0)(body: Component*): Component = {
    val v = new HorizontalLayout()
    if (border != 0) v.addComponent(xSpace(border))
    for (b <- body) {
      v.addComponent(b)
    }
    if (border != 0) v.addComponent(xSpace(border))
    v
  }

  def verticalBorder(b: Int = BORDER)(body: Component*): Component = {
    horizontal(BORDER)(
      vertical(BORDER)(
        body: _*
      )
    )
  }

  def item(title: String, value: String): Component = {
    val h = new HorizontalLayout()
    h.addComponent(new Label(title + ":"))
    val l = new Label(value)
    h.addComponent(l)
    h
  }

  def panel(name: String)(content: Component) = {
    val p = new Panel(name)
    p.setSizeFull()
    val v = verticalBorder() {
      content
    }
    p.setContent(v)
    p
  }

  def debug(j: Json) = {
    val t = new TextArea()
    t.setRows(30)
    t.setWidth(400f, com.vaadin.server.Sizeable.Unit.PIXELS)
    t.setValue(Pretty(j))
    t
  }


  class SLink(name: String, val sort:String, act: => Component) extends Button(name) with Comparable[SLink] {
    setStyleName("link")
    addClickListener(
      new Button.ClickListener {
        def buttonClick(e: Button.ClickEvent) = {
          set(act)
        }
      }
    )

    @Override def compareTo(l1: SLink) = {
      //val c = getCaption
      //val c1 = l.getCaption
      //c.compareTo(c1)
      sort.compareTo(l1.sort)
    }
  }

  def slink(nameId: JsonObject, page: (String) => Component): AnyRef = {
    val name = jgetString(nameId, "name")
    val id = jgetString(nameId, "id")
    val sort = jgetString(nameId, "sort")
    val sort1 = if (sort == "") name else sort
    new SLink(name, sort1, page(id))
    //slink(name, page(id))
  }

  def list1(title: String, links: Seq[Json], act: (String) => Component): Component = {
    if (links.size > 0) {
      val t = new Table()
      t.setData()
      t.setPageLength(links.size)
      t.addContainerProperty(title, classOf[SLink], null)
      for ((link, idx) <- links zipWithIndex) {
        val l = slink(jgetObject(link), act)
        t.addItem(Seq(l).toArray, new Integer(idx + 1))
      }
      t.sort(Array(title), Array(true))
      t
    } else {
      new Label("No " + title)
    }
  }

  def link1(title: String, j: JsonObject, act: (String) => Component): Component = {

    def b = {
      val b = new Button(jgetString(j, "name"))
      b.setStyleName("link")
      b.addClickListener(
        new Button.ClickListener {
          def buttonClick(e: Button.ClickEvent) = {
            set(act(jgetString(j, "id")))
          }
        }
      )
      b
    }
    if (title == "") {
      b
    } else {
      horizontal()(
        if (title == "") empty else (new Label(title + ": ")),
        b
      )
    }
  }

  def data1(title: String, links: Seq[JsonObject], act: String => Component) = {
    panel(title) {
      list1(title, links, act)
    }
  }

  case class P2(name: String, id: String) {
    override def toString = name
  }

  def select1(name: String, refs: Seq[JsonObject], act: (JsonObject) => Unit, default: JsonObject = emptyJsonObject) = {
    var default1: P2 = null
    val s = new NativeSelect(name)
    val x = for (ref <- refs) yield {
      val p = P2(jgetString(ref, "name"), jgetString(ref, "id"))
      if (jgetString(ref, "id") == jgetString(default, "id")) {
        default1 = p
      }
      s.addItem(p)

    }
    s.setImmediate(true)
    s.addValueChangeListener(
      new Property.ValueChangeListener() {
        def valueChange(event: ValueChangeEvent) {
          val v0 = s.getValue.asInstanceOf[P2]
          val v = if (v0 == null) emptyJsonObject else JsonObject("name" -> v0.name, "id" -> v0.id)
          act(v)
        }
      })
    if (default != emptyJsonObject) {
      s.setValue(default1)
    }
    s
  }


}
