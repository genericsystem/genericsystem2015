package org.genericsystem.reactor.gscomponents3;

import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.ReverseFlexDirection;
import org.genericsystem.reactor.gscomponents.GSDiv;
import org.genericsystem.reactor.gscomponents3.Table.ContentRow;

@Style(name = "flex", value = "1 1 0%")
@Style(name = "overflow", value = "hidden")
@ReverseFlexDirection(path = GSComposite.class)
@Children(ContentRow.class)
public class Table extends GSDiv {

	public static class HeaderRow extends GSComposite {
	}

	public static class ContentRow extends GSComposite {
	}

	public static class FooterRow extends GSComposite {
	}
}