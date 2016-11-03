package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;

@FlexDirectionStyle(FlexDirection.ROW)
@Style(name = "flex-wrap", value = "wrap")
public class Responsive extends FlexDiv {
	@Override
	public void init() {
		for (Tag tag : getObservableChildren()) {
			tag.addStyle("margin", "10px");
			tag.addStyle("padding", "10px");
			tag.addStyle("border-radius", "10px");
			tag.addStyle("background-color", "white");
			tag.addStyleClass("screenResponsive");
			tag.addStyle("max-width", "100%");
		}
	}
}
