package org.genericsystem.reactor.gscomponents3;

import org.genericsystem.reactor.htmltag.HtmlLabel.GSLabelDisplayer;

import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.GSDiv;
import org.genericsystem.reactor.gscomponents3.GSComposite.Content;
import org.genericsystem.reactor.gscomponents3.GSComposite.Footer;
import org.genericsystem.reactor.gscomponents3.GSComposite.Header;

@Style(name = "flex", value = "1 1 0%")
@Style(name = "overflow", value = "hidden")
@Style(path = GSDiv.class, name = "flex", value = "1 1 0%")
@Style(path = GSDiv.class, name = "overflow", value = "hidden")
@Children({ GSComposite.Content.class })
@FlexDirectionStyle(path = Content.class, value = FlexDirection.COLUMN)
@FlexDirectionStyle(path = Header.class, value = FlexDirection.COLUMN)
@FlexDirectionStyle(path = Footer.class, value = FlexDirection.COLUMN)
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