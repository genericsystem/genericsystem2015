package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.contextproperties.FlexDirectionDefaults;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;

@Style(name = "display", value = "flex")
@Style(name = "flex-wrap", value = "nowrap")
@FlexDirectionStyle(FlexDirection.COLUMN)
public class FlexDiv extends HtmlDiv implements FlexDirectionDefaults {

	@FlexDirectionStyle(FlexDirection.ROW)
	public static class FlexRow extends FlexDiv {
	}
}