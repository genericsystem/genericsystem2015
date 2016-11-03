package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.htmltag.HtmlLabel.GSLabelDisplayer;

import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.gscomponents.Composite.Content;
import org.genericsystem.reactor.gscomponents.Composite.Footer;
import org.genericsystem.reactor.gscomponents.Composite.Header;

@Style(name = "flex", value = "1 1 0%")
@Style(name = "overflow", value = "hidden")
@Style(path = FlexDiv.class, name = "flex", value = "1 1 0%")
@Style(path = FlexDiv.class, name = "overflow", value = "hidden")
@Children({ Composite.Content.class })
@Children(path = Content.class, value = GSLabelDisplayer.class)
@Children(path = Header.class, value = GSLabelDisplayer.class)
@Children(path = Footer.class, value = GSLabelDisplayer.class)
public abstract class Composite extends FlexDiv {

	public static class Content extends FlexDiv {
	}

	public static class Header extends FlexDiv {
	}

	public static class Footer extends FlexDiv {
	}
}