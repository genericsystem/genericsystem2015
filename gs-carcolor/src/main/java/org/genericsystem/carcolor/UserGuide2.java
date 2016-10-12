package org.genericsystem.carcolor;

import org.genericsystem.reactor.htmltag.HtmlHyperLink;

import org.genericsystem.reactor.gscomponents.GSDiv;

import org.genericsystem.carcolor.UserGuide2.ModalContent;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.StyleClass;

@StyleClass("modal")
@ReactorDependencies(ModalContent.class)
public class UserGuide2 extends GSDiv {
	@Override
	public void init() {
		createInitializedDisplayProperty("none");
		bindStyle(DISPLAY, DISPLAY);
	}

	@ReactorDependencies(CloseLink.class)
	@Style(name = "-webkit-border-radius", value = "30px")
	@Style(name = "border-radius", value = "30px")
	@Style(name = "-moz-border-radius", value = "30px")
	@Style(name = "max-width", value = "40%")
	@StyleClass("modal-content")
	public static class ModalContent extends GSDiv {

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
