package org.genericsystem.reactor.gscomponents3;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.GSDiv;

@FlexDirectionStyle(FlexDirection.ROW)
@Style(name = "flex-wrap", value = "wrap")
public class Responsive extends GSDiv {
	@Override
	public void init() {
		for (Tag tag : getObservableChildren()) {
			tag.addStyle("margin", "10px");
			tag.addStyle("padding", "10px");
			tag.addStyle("border-radius", "10px");
			tag.addStyle("background-color", "white");
			tag.addStyle("min-width", "10cm");
		}
	}
}
