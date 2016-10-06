package org.genericsystem.reactor.gscomponents2;

import org.genericsystem.reactor.annotations.Styles.AlignItems;
import org.genericsystem.reactor.annotations.Styles.BackgroundColor;
import org.genericsystem.reactor.annotations.Styles.Color;
import org.genericsystem.reactor.annotations.Styles.Flex;
import org.genericsystem.reactor.annotations.Styles.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Styles.FlexWrap;
import org.genericsystem.reactor.annotations.Styles.GenericValueBackgroundColor;
import org.genericsystem.reactor.annotations.Styles.Height;
import org.genericsystem.reactor.annotations.Styles.JustifyContent;
import org.genericsystem.reactor.annotations.Styles.MarginBottom;
import org.genericsystem.reactor.annotations.Styles.MarginRight;
import org.genericsystem.reactor.annotations.Styles.Overflow;
import org.genericsystem.reactor.annotations.Styles.Style;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.GSDiv;

import org.genericsystem.reactor.htmltag.HtmlHyperLink;

@Flex("1")
@Overflow("hidden")
public class GSCellDiv extends GSDiv {

	@Flex("1")
	@JustifyContent("center")
	@AlignItems("center")
	@MarginRight("1px")
	@MarginBottom("1px")
	@Color("#ffffff")
	@BackgroundColor("#ea0084")
	public static class GSTitleLineCellDiv extends GSDiv {
	}

	@FlexDirectionStyle(FlexDirection.ROW)
	@Flex("1")
	@MarginRight("1px")
	@MarginBottom("1px")
	@Color("#ffffff")
	public static class GSSubcellEditorDiv extends GSDiv {
	}

	@JustifyContent("center")
	@AlignItems("center")
	public static class GSComponentEditorDiv extends GSSubcellEditorDiv {
	}

	@Color("#000000")
	@GenericValueBackgroundColor("#e5ed00")
	public static class GSSubcellDiv extends GSComponentEditorDiv {
	}

	@Flex("1")
	@FlexDirectionStyle(FlexDirection.COLUMN)
	@FlexWrap("wrap")
	@Overflow("auto")
	public static class WrappedColumnDiv extends GSDiv {
	}

	@Flex("1 0 auto")
	@FlexDirectionStyle(FlexDirection.ROW)
	public static class SubcellEditorContainerDiv extends GSDiv {
	}

	@Flex("1")
	@JustifyContent("center")
	@AlignItems("center")
	public static class CenteredFlexDiv extends GSDiv {
	}

	@JustifyContent("center")
	@Height("100%")
	@Style(name = "text-decoration", value = "none")
	public static class GSActionLink extends HtmlHyperLink {
	}
}
