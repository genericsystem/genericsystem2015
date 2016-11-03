package org.genericsystem.reactor.gscomponents2;

import org.genericsystem.reactor.htmltag.HtmlHyperLink;

import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Style.GenericValueBackgroundColor;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDiv;

@Style(name = "flex", value = "1")
@Style(name = "overflow", value = "hidden")
public class CellDiv extends FlexDiv {

	@Style(name = "flex", value = "1")
	@Style(name = "justify-content", value = "center")
	@Style(name = "align-items", value = "center")
	@Style(name = "margin-right", value = "1px")
	@Style(name = "margin-bottom", value = "1px")
	@Style(name = "color", value = "#ffffff")
	@Style(name = "background-color", value = "#ea0084")
	public static class TitleLineCellDiv extends FlexDiv {
	}

	@FlexDirectionStyle(FlexDirection.ROW)
	@Style(name = "flex", value = "1")
	@Style(name = "margin-right", value = "1px")
	@Style(name = "margin-bottom", value = "1px")
	@Style(name = "color", value = "#ffffff")
	public static class SubcellEditorDiv extends FlexDiv {
	}

	@Style(name = "justify-content", value = "center")
	@Style(name = "align-items", value = "center")
	public static class ComponentEditorDiv extends SubcellEditorDiv {
	}

	@Style(name = "color", value = "#000000")
	@GenericValueBackgroundColor("#e5ed00")
	public static class SubcellDiv extends ComponentEditorDiv {
	}

	@Style(name = "flex", value = "1")
	@FlexDirectionStyle(FlexDirection.COLUMN)
	@Style(name = "flex-wrap", value = "wrap")
	@Style(name = "overflow", value = "auto")
	public static class WrappedColumnDiv extends FlexDiv {
	}

	@Style(name = "flex", value = "1 0 auto")
	@FlexDirectionStyle(FlexDirection.ROW)
	public static class SubcellEditorContainerDiv extends FlexDiv {
	}

	@Style(name = "flex", value = "1")
	@Style(name = "justify-content", value = "center")
	@Style(name = "align-items", value = "center")
	public static class CenteredFlexDiv extends FlexDiv {
	}

	@Style(name = "justify-content", value = "center")
	@Style(name = "height", value = "100%")
	@Style(name = "text-decoration", value = "none")
	public static class ActionLink extends HtmlHyperLink {
	}
}
