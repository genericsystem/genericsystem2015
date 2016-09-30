package org.genericsystem.reactor.az3;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.Styles;
import org.genericsystem.reactor.annotations.Styles.AlignItems;
import org.genericsystem.reactor.annotations.Styles.BackgroundColor;
import org.genericsystem.reactor.annotations.Styles.Flex;
import org.genericsystem.reactor.annotations.Styles.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Styles.GenericValueBackgroundColor;
import org.genericsystem.reactor.annotations.Styles.Height;
import org.genericsystem.reactor.annotations.Styles.JustifyContent;
import org.genericsystem.reactor.annotations.Styles.KeepFlexDirection;
import org.genericsystem.reactor.annotations.Styles.MarginBottom;
import org.genericsystem.reactor.annotations.Styles.MarginRight;
import org.genericsystem.reactor.annotations.Styles.Overflow;
import org.genericsystem.reactor.annotations.Styles.ReverseFlexDirection;
import org.genericsystem.reactor.annotations.Styles.Style;
import org.genericsystem.reactor.annotations.Styles.Width;
import org.genericsystem.reactor.az.FlexDirection;
import org.genericsystem.reactor.az.GSCheckBoxWithValue.GSCheckBoxDisplayer;
import org.genericsystem.reactor.az.GSDiv;
import org.genericsystem.reactor.az3.GSComposite.Content;
import org.genericsystem.reactor.az3.GSComposite.Header;
import org.genericsystem.reactor.az3.GSComposite.Header.HeaderLabel;
import org.genericsystem.reactor.az3.Table.ContentRow;
import org.genericsystem.reactor.gstag.HtmlButton;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.ObservableValueSelector;

@Flex("1 1 0%")
@Overflow("hidden")
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

	@Style(name = "margin", value = "4px")
	@Styles.Color(path = HeaderRow.class, value = "white")
	@BackgroundColor(path = { HeaderRow.class, GSValueComponents.class, Content.class }, value = "#ea0084")
	@BackgroundColor(path = { HeaderRow.class, GSValueComponents.class, Header.class }, value = "#ea0084")
	@BackgroundColor(path = { HeaderRow.class, Content.class, GSValueComponents.class, Content.class }, value = "#ea0084")
	@BackgroundColor(path = { HeaderRow.class, Content.class, GSValueComponents.class, Header.class }, value = "#ea0084")
	@Styles.Color(path = { ContentRow.class, GSValueComponents.class, Header.class }, value = "white")
	@GenericValueBackgroundColor(path = { ContentRow.class, GSValueComponents.class, Header.class }, value = "#3393ff")
	@AlignItems(path = { ContentRow.class, GSValueComponents.class, Header.class }, value = "flex-start")
	@ReactorDependencies({ HeaderRow.class, ContentRow.class })
	@ReactorDependencies(path = HeaderRow.class, value = { GSValueComponents.class, Content.class, ButtonDiv.class })
	@ReactorDependencies(path = ContentRow.class, value = { GSValueComponents.class, GSHolders.class, ButtonDiv.class })
	@ReactorDependencies(path = { ContentRow.class, ButtonDiv.class }, value = RemoveButton.class)
	@ReactorDependencies(path = { HeaderRow.class, Content.class }, value = GSValueComponents.class)
	@ForEach(path = { HeaderRow.class, Content.class }, value = ObservableListExtractor.ATTRIBUTES_OF_TYPE.class)
	@ForEach(path = ContentRow.class, value = ObservableListExtractor.SUBINSTANCES.class)
	@ForEach(path = { ContentRow.class, GSHolders.class }, value = ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
	@ForEach(path = { ContentRow.class, GSValueComponents.class, Content.class }, value = ObservableListExtractor.OTHER_COMPONENTS_2.class)
	public static class InstancesTable extends Table {

	}

	@GenericValueBackgroundColor(path = { GSValueComponents.class, Content.class }, value = "#e5ed00")
	@ReactorDependencies(value = GSValueComponents.class)
	@ReactorDependencies(path = { GSValueComponents.class, Header.class }, value = { HeaderLabel.class, GSCheckBoxDisplayer.class })
	@ForEach(path = GSValueComponents.class, value = ObservableListExtractor.HOLDERS.class)
	@ForEach(path = { GSValueComponents.class, Content.class }, value = ObservableListExtractor.OTHER_COMPONENTS_2.class)
	@Select(path = { GSValueComponents.class, Header.class, HeaderLabel.class }, value = ObservableValueSelector.LABEL_DISPLAYER.class)
	@Select(path = { GSValueComponents.class, Header.class, GSCheckBoxDisplayer.class }, value = ObservableValueSelector.CHECK_BOX_DISPLAYER.class)
	public static class GSHolders extends GSComposite {

	}

	@BackgroundColor(path = Content.class, value = "#e5ed00")
	@BackgroundColor(path = Header.class, value = "#e5ed00")
	@JustifyContent(path = Header.class, value = "center")
	@AlignItems(path = Header.class, value = "center")
	@JustifyContent(path = Content.class, value = "center")
	@AlignItems(path = Content.class, value = "center")
	@FlexDirectionStyle(FlexDirection.ROW)
	@ForEach(path = Content.class, value = ObservableListExtractor.OTHER_COMPONENTS_1.class)
	@Select(path = Header.class, value = ObservableValueSelector.STRICT_ATTRIBUTE_SELECTOR.class)
	@ReactorDependencies({ Header.class, Content.class })
	@MarginRight(path = Header.class, value = "1px")
	@MarginBottom(path = Header.class, value = "1px")
	@MarginRight(path = Content.class, value = "1px")
	@MarginBottom(path = Content.class, value = "1px")
	public static class GSValueComponents extends GSComposite {

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

	@Flex("1")
	@Height("100%")
	@Width("100%")
	public static class RemoveButton extends HtmlButton {

		@Override
		public void init() {
			setText("Remove");
			bindAction(Context::remove);
		}
	}
}