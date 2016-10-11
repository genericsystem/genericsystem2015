package org.genericsystem.reactor.gscomponents2;

import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Styles.ReverseFlexDirection;
import org.genericsystem.reactor.annotations.Styles.Style;
import org.genericsystem.reactor.gscomponents.GSDiv;
import org.genericsystem.reactor.gscomponents2.Table.ContentRow;

@Style(name = "flex", value = "1 1 0%")
@Style(name = "overflow", value = "hidden")
@ReactorDependencies(ContentRow.class)
public class Table extends GSDiv {

	@ReverseFlexDirection
	public static class HeaderRow extends GSComposite {

	}

	@ReverseFlexDirection
	public static class ContentRow extends GSComposite {

	}

	@ReverseFlexDirection
	public static class FooterRow extends GSComposite {

	}
}