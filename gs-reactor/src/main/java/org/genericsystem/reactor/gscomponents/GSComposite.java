package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.htmltag.HtmlLabel.GSLabelDisplayer;

import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.gscomponents.GSComposite.Content;
import org.genericsystem.reactor.gscomponents.GSComposite.Footer;
import org.genericsystem.reactor.gscomponents.GSComposite.Header;

@Style(name = "flex", value = "1 1 0%")
@Style(name = "overflow", value = "hidden")
@Style(path = GSDiv.class, name = "flex", value = "1 1 0%")
@Style(path = GSDiv.class, name = "overflow", value = "hidden")
@Children({ GSComposite.Content.class })
@Children(path = Content.class, value = GSLabelDisplayer.class)
@Children(path = Header.class, value = GSLabelDisplayer.class)
@Children(path = Footer.class, value = GSLabelDisplayer.class)
public abstract class GSComposite extends GSDiv {

	public static class Content extends GSDiv {
	}

	public static class Header extends GSDiv {
	}

	public static class Footer extends GSDiv {
	}
}