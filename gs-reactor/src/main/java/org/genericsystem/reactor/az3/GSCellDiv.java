package org.genericsystem.reactor.az3;

import org.genericsystem.reactor.annotations.Styles.AlignItems;
import org.genericsystem.reactor.annotations.Styles.BackgroundColor;
import org.genericsystem.reactor.annotations.Styles.Color;
import org.genericsystem.reactor.annotations.Styles.Flex;
import org.genericsystem.reactor.annotations.Styles.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Styles.FlexWrap;
import org.genericsystem.reactor.annotations.Styles.GenericBackgroundColor;
import org.genericsystem.reactor.annotations.Styles.Height;
import org.genericsystem.reactor.annotations.Styles.JustifyContent;
import org.genericsystem.reactor.annotations.Styles.KeepFlexDirection;
import org.genericsystem.reactor.annotations.Styles.MarginBottom;
import org.genericsystem.reactor.annotations.Styles.MarginRight;
import org.genericsystem.reactor.annotations.Styles.Overflow;
import org.genericsystem.reactor.annotations.Styles.Style;
import org.genericsystem.reactor.az.FlexDirection;
import org.genericsystem.reactor.az.GSDiv;
import org.genericsystem.reactor.gstag.HtmlHyperLink;

@Flex("1")
@Overflow("hidden")
public class GSCellDiv extends GSDiv {

	@BackgroundColor("#EA4500")
	@MarginRight("1px")
	@MarginBottom("1px")
	@Color("White")
	@JustifyContent("center")
	@AlignItems("center")
	public static class GSTitleDiv extends GSDiv {
	}

	@Flex("1")
	@JustifyContent("center")
	@AlignItems("center")
	@MarginRight("1px")
	@MarginBottom("1px")
	@Color("#ffffff")
	@BackgroundColor("#ea0084")
	public static class GSTitleLineCellDiv extends GSDiv {
	}

	@KeepFlexDirection
	@BackgroundColor("#ea0084")
	@MarginRight("1px")
	@MarginBottom("1px")
	public static class ButtonDiv extends GSDiv {

		@Override
		public void init() {
			if (FlexDirection.ROW.equals(getDirection())) {
				addStyle("flex", "0");
				addStyle("min-width", "100px");
			} else {
				addStyle("flex", "1");
			}
			getDirectionProperty().addListener((o, v, nv) -> {
				if (FlexDirection.ROW.equals(nv)) {
					addStyle("flex", "0");
					addStyle("min-width", "100px");
				} else {
					addStyle("flex", "1");
				}
			});
		}
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
	@GenericBackgroundColor
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
	@Style(propertyName = "text-decoration", propertyValue = "none")
	public static class GSActionLink extends HtmlHyperLink {
	}
}
