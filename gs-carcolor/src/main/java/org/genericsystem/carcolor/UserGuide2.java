package org.genericsystem.carcolor;

import org.genericsystem.reactor.htmltag.HtmlHyperLink;
import org.genericsystem.reactor.htmltag.HtmlLi;
import org.genericsystem.reactor.htmltag.HtmlUl;

import org.genericsystem.reactor.gscomponents.GSDiv;

import org.genericsystem.carcolor.UserGuide2.ModalContent;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.StyleClass;

@StyleClass("modal")
@Children(ModalContent.class)
public class UserGuide2 extends GSDiv {
	@Override
	public void init() {
		createInitializedDisplayProperty("none");
		bindStyle(DISPLAY, DISPLAY);
	}

	@Children({ CloseLink.class, TextContent.class })
	@Style(name = "-webkit-border-radius", value = "30px")
	@Style(name = "border-radius", value = "30px")
	@Style(name = "-moz-border-radius", value = "30px")
	@Style(name = "max-width", value = "40%")
	@StyleClass("modal-content")
	public static class ModalContent extends GSDiv {
	}

	@Children(value = { HtmlUl.class, HtmlUl.class, HtmlUl.class, GSDiv.class })
	@Children(path = HtmlUl.class, pos = 0, value = { HtmlLi.class, HtmlLi.class, HtmlLi.class })
	@Children(path = HtmlUl.class, pos = 1, value = HtmlLi.class)
	@Children(path = HtmlUl.class, pos = 2, value = HtmlLi.class)
	@SetText(path = HtmlUl.class, pos = 0, value = "How to use CarColor Demo")
	@SetText(path = { HtmlUl.class, HtmlLi.class }, pos = { 0, 0 }, value = "Insert Car model")
	@SetText(path = { HtmlUl.class, HtmlLi.class }, pos = { 0, 1 }, value = "Select color in the ComboBox")
	@SetText(path = { HtmlUl.class, HtmlLi.class }, pos = { 0, 2 }, value = "Use \"add Button\" to update data")
	@SetText(path = HtmlUl.class, pos = 1, value = "Color Management")
	@SetText(path = { HtmlUl.class, HtmlLi.class }, pos = { 1, 0 }, value = "Add new color of any CSS color style (ex : Black, rgb(0,0,0), #000000, ...)")
	@SetText(path = { HtmlUl.class, HtmlLi.class }, pos = { 1, 1 }, value = "Select car in the ComboBox")
	@SetText(path = { HtmlUl.class, HtmlLi.class }, pos = { 1, 2 }, value = "Use \"add Button\" to update data")
	@SetText(path = HtmlUl.class, pos = 2, value = "General Tips")
	@SetText(path = { HtmlUl.class, HtmlLi.class }, pos = { 2, 0 }, value = "Click \"Add Button\" to add an entry in the cache")
	@SetText(path = { HtmlUl.class, HtmlLi.class }, pos = { 2, 1 }, value = "Click the \"Remove Button\" to delete the entry in your cache")
	@SetText(path = { HtmlUl.class, HtmlLi.class }, pos = { 2, 2 }, value = "Click \"Save Button\" to persist the cache")
	@SetText(path = { HtmlUl.class, HtmlLi.class }, pos = { 2, 3 }, value = "Click \"Cancel Button\" to release the cache")
	@Style(path = GSDiv.class, name = "text-align", value = "center")
	@SetText(path = GSDiv.class, value = "To plenty enjoy the power of GS-REACTOR, go to Learning / Get Started")
	@Style(name = "padding", value = "35px")
	public static class TextContent extends GSDiv {
	}

	@StyleClass("close")
	@SetText("x")
	public static class CloseLink extends HtmlHyperLink {
		@Override
		public void init() {
			bindAction(model -> getDisplayProperty(model).setValue("none"));
		}
	};
}
