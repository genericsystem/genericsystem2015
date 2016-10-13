package org.genericsystem.reactor.gscomponents3;

import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import org.genericsystem.reactor.htmltag.HtmlButton;
import org.genericsystem.reactor.htmltag.HtmlHyperLink;
import org.genericsystem.reactor.htmltag.HtmlLabel.GSLabelDisplayer;

import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindSelection;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Style.GenericValueBackgroundColor;
import org.genericsystem.reactor.annotations.Style.KeepFlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.GSCheckBoxWithValue.GSCheckBoxDisplayer;
import org.genericsystem.reactor.gscomponents.GSCheckBoxWithValue.GSCheckBoxEditor;
import org.genericsystem.reactor.gscomponents.GSDiv;
import org.genericsystem.reactor.gscomponents3.GSComposite.Content;
import org.genericsystem.reactor.gscomponents3.GSComposite.Header;
import org.genericsystem.reactor.gscomponents3.InstancesTable.ButtonDiv;
import org.genericsystem.reactor.gscomponents3.InstancesTable.GSHolders;
import org.genericsystem.reactor.gscomponents3.InstancesTable.GSValueComponents;
import org.genericsystem.reactor.gscomponents3.InstancesTable.InstanceNameLink;
import org.genericsystem.reactor.gscomponents3.InstancesTable.RemoveButton;
import org.genericsystem.reactor.gscomponents3.Table.ContentRow;
import org.genericsystem.reactor.gscomponents3.Table.HeaderRow;
import org.genericsystem.reactor.model.ContextAction.REMOVE;
import org.genericsystem.reactor.model.ContextAction.SET_SELECTION;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.ObservableValueSelector;

@BindSelection(ContentRow.class)
@GenericValueBackgroundColor(path = { HeaderRow.class, GSValueComponents.class, GSDiv.class }, value = "#ea0084")
@GenericValueBackgroundColor(path = { ContentRow.class, GSValueComponents.class, Header.class }, value = "#3393ff")
@Style(path = HeaderRow.class, name = "color", value = "white")
@Style(path = { ContentRow.class, GSValueComponents.class, Header.class, InstanceNameLink.class }, name = "color", value = "white")
@Style(path = { ContentRow.class, GSValueComponents.class, Header.class }, name = "align-items", value = "flex-start")
@Children({ HeaderRow.class, InstanceBuilder.class, ContentRow.class })
@Children(path = HeaderRow.class, value = { GSValueComponents.class, GSValueComponents.class, ButtonDiv.class })
@Children(path = ContentRow.class, value = { GSValueComponents.class, GSHolders.class, ButtonDiv.class })
@Children(path = { ContentRow.class, GSValueComponents.class, Header.class }, value = InstanceNameLink.class)
@Children(path = { ContentRow.class, ButtonDiv.class }, value = RemoveButton.class)
@ForEach(path = { HeaderRow.class, GSValueComponents.class }, pos = { 0, 1 }, value = ObservableListExtractor.ATTRIBUTES_OF_TYPE.class)
@ForEach(path = ContentRow.class, value = ObservableListExtractor.SUBINSTANCES.class)
@ForEach(path = { ContentRow.class, GSHolders.class }, value = ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
@ForEach(path = { ContentRow.class, GSValueComponents.class, Content.class }, value = ObservableListExtractor.OTHER_COMPONENTS_2.class)
public class InstancesTable extends Table implements SelectionDefaults {

	@FlexDirectionStyle(FlexDirection.ROW)
	public static class HorizontalInstancesTable extends InstancesTable {
	}

	@GenericValueBackgroundColor(path = { GSValueComponents.class, Content.class }, value = "#e5ed00")
	@Children(value = GSValueComponents.class)
	@Children(path = { GSValueComponents.class, Header.class }, value = { GSLabelDisplayer.class, GSCheckBoxDisplayer.class })
	@ForEach(path = GSValueComponents.class, value = ObservableListExtractor.HOLDERS.class)
	@ForEach(path = { GSValueComponents.class, Content.class }, value = ObservableListExtractor.OTHER_COMPONENTS_2.class)
	@Select(path = { GSValueComponents.class, Header.class, GSLabelDisplayer.class }, value = ObservableValueSelector.LABEL_DISPLAYER.class)
	@Select(path = { GSValueComponents.class, Header.class, GSCheckBoxEditor.class }, value = ObservableValueSelector.CHECK_BOX_DISPLAYER.class)
	public static class GSHolders extends GSComposite {
	}

	@FlexDirectionStyle(FlexDirection.ROW)
	@Style(path = GSDiv.class, name = "background-color", value = "#e5ed00")
	@Style(path = GSDiv.class, name = "justify-content", value = "center")
	@Style(path = GSDiv.class, name = "align-items", value = "center")
	@Style(path = GSDiv.class, name = "margin-right", value = "1px")
	@Style(path = GSDiv.class, name = "margin-bottom", value = "1px")
	@ForEach(path = Content.class, value = ObservableListExtractor.OTHER_COMPONENTS_1.class)
	@Select(path = Header.class, value = ObservableValueSelector.STRICT_ATTRIBUTE_SELECTOR.class)
	@Children({ Header.class, Content.class })
	public static class GSValueComponents extends GSComposite {
	}

	@BindAction(SET_SELECTION.class)
	@BindText
	public static class InstanceNameLink extends HtmlHyperLink {
	}

	@KeepFlexDirection
	@Style(name = "background-color", value = "#ea0084")
	@Style(name = "margin-right", value = "1px")
	@Style(name = "margin-bottom", value = "1px")
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

	@Style(name = "flex", value = "1")
	@Style(name = "height", value = "100%")
	@Style(name = "width", value = "100%")
	@SetText("Remove")
	@BindAction(REMOVE.class)
	public static class RemoveButton extends HtmlButton {
	}
}