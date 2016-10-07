package org.genericsystem.reactor.gscomponents2;

import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import org.genericsystem.reactor.htmltag.HtmlButton;
import org.genericsystem.reactor.htmltag.HtmlHyperLink;

import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindSelection;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Styles.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Styles.GenericValueBackgroundColor;
import org.genericsystem.reactor.annotations.Styles.KeepFlexDirection;
import org.genericsystem.reactor.annotations.Styles.Style;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.GSCheckBoxWithValue.GSCheckBoxDisplayer;
import org.genericsystem.reactor.gscomponents.GSCheckBoxWithValue.GSCheckBoxEditor;
import org.genericsystem.reactor.gscomponents.GSDiv;
import org.genericsystem.reactor.gscomponents2.GSComposite.Content;
import org.genericsystem.reactor.gscomponents2.GSComposite.Header;
import org.genericsystem.reactor.gscomponents2.GSComposite.Header.HeaderLabel;
import org.genericsystem.reactor.gscomponents2.InstancesTable.ButtonDiv;
import org.genericsystem.reactor.gscomponents2.InstancesTable.GSHolders;
import org.genericsystem.reactor.gscomponents2.InstancesTable.GSValueComponents;
import org.genericsystem.reactor.gscomponents2.InstancesTable.InstanceNameLink;
import org.genericsystem.reactor.gscomponents2.InstancesTable.RemoveButton;
import org.genericsystem.reactor.gscomponents2.Table.ContentRow;
import org.genericsystem.reactor.gscomponents2.Table.HeaderRow;
import org.genericsystem.reactor.model.ContextAction.REMOVE;
import org.genericsystem.reactor.model.ContextAction.SET_SELECTION;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.ObservableValueSelector;

@BindSelection(ContentRow.class)
@Style(path = HeaderRow.class, name = "color", value = "white")
@GenericValueBackgroundColor(path = { HeaderRow.class, GSValueComponents.class, Content.class }, value = "#ea0084")
@GenericValueBackgroundColor(path = { HeaderRow.class, GSValueComponents.class, Header.class }, value = "#ea0084")
@GenericValueBackgroundColor(path = { HeaderRow.class, Content.class, GSValueComponents.class, Content.class }, value = "#ea0084")
@GenericValueBackgroundColor(path = { HeaderRow.class, Content.class, GSValueComponents.class, Header.class }, value = "#ea0084")
@Style(path = { ContentRow.class, GSValueComponents.class, Header.class, InstanceNameLink.class }, name = "color", value = "white")
@GenericValueBackgroundColor(path = { ContentRow.class, GSValueComponents.class, Header.class }, value = "#3393ff")
@Style(path = { ContentRow.class, GSValueComponents.class, Header.class }, name = "align-items", value = "flex-start")
@ReactorDependencies({ HeaderRow.class, InstanceBuilder.class, ContentRow.class })
@ReactorDependencies(path = HeaderRow.class, value = { GSValueComponents.class, Content.class, ButtonDiv.class })
@ReactorDependencies(path = { HeaderRow.class, Content.class }, value = GSValueComponents.class)
@ReactorDependencies(path = ContentRow.class, value = { GSValueComponents.class, GSHolders.class, ButtonDiv.class })
@ReactorDependencies(path = { ContentRow.class, GSValueComponents.class, Header.class }, value = InstanceNameLink.class)
@ReactorDependencies(path = { ContentRow.class, ButtonDiv.class }, value = RemoveButton.class)
@ForEach(path = { HeaderRow.class, Content.class }, value = ObservableListExtractor.ATTRIBUTES_OF_TYPE.class)
@ForEach(path = ContentRow.class, value = ObservableListExtractor.SUBINSTANCES.class)
@ForEach(path = { ContentRow.class, GSHolders.class }, value = ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
@ForEach(path = { ContentRow.class, GSValueComponents.class, Content.class }, value = ObservableListExtractor.OTHER_COMPONENTS_2.class)
public class InstancesTable extends Table implements SelectionDefaults {

	@GenericValueBackgroundColor(path = { GSValueComponents.class, Content.class }, value = "#e5ed00")
	@ReactorDependencies(value = GSValueComponents.class)
	@ReactorDependencies(path = { GSValueComponents.class, Header.class }, value = { HeaderLabel.class, GSCheckBoxDisplayer.class })
	@ForEach(path = GSValueComponents.class, value = ObservableListExtractor.HOLDERS.class)
	@ForEach(path = { GSValueComponents.class, Content.class }, value = ObservableListExtractor.OTHER_COMPONENTS_2.class)
	@Select(path = { GSValueComponents.class, Header.class, HeaderLabel.class }, value = ObservableValueSelector.LABEL_DISPLAYER.class)
	@Select(path = { GSValueComponents.class, Header.class, GSCheckBoxEditor.class }, value = ObservableValueSelector.CHECK_BOX_DISPLAYER.class)
	public static class GSHolders extends GSComposite {

	}

	@FlexDirectionStyle(FlexDirection.ROW)
	@Style(path = Content.class, name = "background-color", value = "#e5ed00")
	@Style(path = Header.class, name = "background-color", value = "#e5ed00")
	@Style(path = Header.class, name = "justify-content", value = "center")
	@Style(path = Header.class, name = "align-items", value = "center")
	@Style(path = Content.class, name = "justify-content", value = "center")
	@Style(path = Content.class, name = "align-items", value = "center")
	@ForEach(path = Content.class, value = ObservableListExtractor.OTHER_COMPONENTS_1.class)
	@Select(path = Header.class, value = ObservableValueSelector.STRICT_ATTRIBUTE_SELECTOR.class)
	@ReactorDependencies({ Header.class, Content.class })
	@Style(path = Header.class, name = "margin-right", value = "1px")
	@Style(path = Header.class, name = "margin-bottom", value = "1px")
	@Style(path = Content.class, name = "margin-right", value = "1px")
	@Style(path = Content.class, name = "margin-bottom", value = "1px")
	public static class GSValueComponents extends GSComposite {

	}

	@BindAction(SET_SELECTION.class)
	public static class InstanceNameLink extends HtmlHyperLink {

		@Override
		public void init() {
			bindText();
		}
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