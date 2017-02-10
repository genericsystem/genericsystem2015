package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindSelection;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Style.GenericValueBackgroundColor;
import org.genericsystem.reactor.annotations.Style.ReverseFlexDirection;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.context.ContextAction.MOUNT;
import org.genericsystem.reactor.context.ContextAction.REMOVE;
import org.genericsystem.reactor.context.ContextAction.SET_SELECTION;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.context.ObservableValueSelector;
import org.genericsystem.reactor.context.ObservableValueSelector.GENERIC_VALUE_DISPLAYER;
import org.genericsystem.reactor.context.TagSwitcher;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.gscomponents.CheckBoxWithValue.CheckBoxDisplayer;
import org.genericsystem.reactor.gscomponents.Composite.Content;
import org.genericsystem.reactor.gscomponents.Composite.Header;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlImg;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel.GSLabelDisplayer;
import org.genericsystem.reactor.gscomponents.InstancesTable.ButtonDiv;
import org.genericsystem.reactor.gscomponents.InstancesTable.ContentRow;
import org.genericsystem.reactor.gscomponents.InstancesTable.HeaderRow;
import org.genericsystem.reactor.gscomponents.InstancesTable.Holders;
import org.genericsystem.reactor.gscomponents.InstancesTable.LinksDiv;
import org.genericsystem.reactor.gscomponents.InstancesTable.ValueComponents;
import org.genericsystem.reactor.gscomponents.instancebuilder.InstanceBuilder;

@Switch(path = { FlexDiv.class, ButtonDiv.class }, pos = { -1, 0 }, value = TagSwitcher.ADMIN_MODE_ONLY.class)
@Style(name = "overflow", value = "hidden")
@ReverseFlexDirection(path = Composite.class)
@BindSelection(value = Composite.class, valuePos = 2)
@GenericValueBackgroundColor(path = { ContentRow.class, ValueComponents.class, Header.class }, value = "#3393ff")
@Style(path = Composite.class, name = "flex", value = "0 1 auto")
@Style(path = { ContentRow.class, ValueComponents.class, Header.class, GSLabelDisplayer.class }, name = "color", value = "white")
@Style(path = { ContentRow.class, ValueComponents.class, Header.class, GSLabelDisplayer.class }, name = "padding-left", value = "2px")
@Style(path = { ContentRow.class, ValueComponents.class, Header.class, GSLabelDisplayer.class }, name = "align-items", value = "flex-start")
@Style(path = { HeaderRow.class, ButtonDiv.class }, name = "background-color", value = "#ea0084")
@Style(name = "background-color", value = "lightgrey")
@Style(name = "padding-left", value = "1px")
@Style(name = "padding-top", value = "1px")
@Children({ HeaderRow.class, InstanceBuilder.class, ContentRow.class })
@Children(path = HeaderRow.class, value = { ValueComponents.class, ValueComponents.class, ButtonDiv.class })
@Children(path = ContentRow.class, value = { ValueComponents.class, Holders.class, LinksDiv.class })
@ForEach(path = { HeaderRow.class, ValueComponents.class }, pos = { 0, 1 }, value = ObservableListExtractor.ATTRIBUTES_OF_TYPE.class)
@ForEach(path = ContentRow.class, value = ObservableListExtractor.SUBINSTANCES.class)
@ForEach(path = { ContentRow.class, Holders.class }, value = ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
@ForEach(path = { ContentRow.class, ValueComponents.class, Content.class }, value = ObservableListExtractor.OTHER_COMPONENTS_2.class)
public class InstancesTable extends FlexDiv implements SelectionDefaults {

	@GenericValueBackgroundColor(path = { ValueComponents.class, FlexDiv.class }, value = "#ea0084")
	@Style(name = "color", value = "white")
	@Children(path = { ValueComponents.class, Header.class }, value = GSLabelDisplayer.class)
	@Select(path = { ValueComponents.class, Header.class }, value = GENERIC_VALUE_DISPLAYER.class)
	public static class HeaderRow extends Composite {
	}

	public static class ContentRow extends Composite {
	}

	@FlexDirectionStyle(FlexDirection.ROW)
	public static class HorizontalInstancesTable extends InstancesTable {
	}

	@GenericValueBackgroundColor(path = { ValueComponents.class, Content.class }, value = "#e5ed00")
	@Children(value = ValueComponents.class)
	@ForEach(path = ValueComponents.class, value = ObservableListExtractor.HOLDERS.class)
	@ForEach(path = { ValueComponents.class, Content.class }, value = ObservableListExtractor.OTHER_COMPONENTS_2.class)
	public static class Holders extends Composite {
	}

	@FlexDirectionStyle(FlexDirection.ROW)
	@Style(path = FlexDiv.class, name = "background-color", value = "#e5ed00")
	@Style(path = FlexDiv.class, name = "justify-content", value = "center")
	@Style(path = FlexDiv.class, name = "align-items", value = "center")
	@Style(path = FlexDiv.class, name = "margin-right", value = "1px")
	@Style(path = FlexDiv.class, name = "margin-bottom", value = "1px")
	@ForEach(path = Content.class, value = ObservableListExtractor.OTHER_COMPONENTS_1.class)
	@Select(path = Header.class, value = ObservableValueSelector.GENERIC_INSTANCE_VALUE_DISPLAYER.class)
	@Select(path = { Header.class, FlexDiv.class, HtmlLabel.class }, pos = { -1, 0, 0 }, value = ObservableValueSelector.NON_PASSWORD_INSTANCE_SELECTOR.class)
	@Select(path = { Header.class, FlexDiv.class, HtmlLabel.class }, pos = { -1, 0, 1 }, value = ObservableValueSelector.PASSWORD_INSTANCE_SELECTOR.class)
	@Select(path = { Header.class, CheckBoxDisplayer.class }, value = ObservableValueSelector.INSTANCE_CHECK_BOX_DISPLAYER.class)
	@Select(path = { Header.class, FlexDiv.class }, value = ObservableValueSelector.INSTANCE_LABEL_DISPLAYER.class)
	@Children({ Content.class, Header.class })
	@Children(path = Header.class, value = { CheckBoxDisplayer.class, FlexDiv.class })
	@Children(path = { Header.class, FlexDiv.class }, value = { GSLabelDisplayer.class, HtmlLabel.class })
	@SetText(path = { Header.class, FlexDiv.class, HtmlLabel.class }, pos = { -1, 0, 1 }, value = "******")
	public static class ValueComponents extends Composite {
	}

	@Style(name = "background-color", value = "white")
	@Style(name = "justify-content", value = "center")
	@FlexDirectionStyle(FlexDirection.ROW)
	@Style(name = "margin-right", value = "1px")
	@Style(name = "margin-bottom", value = "1px")
	public static class ButtonDiv extends FlexDiv {
		@Override
		public void init() {
			addPrefixBinding(context -> {
				if (FlexDirection.ROW.equals(getDirectionProperty(context).getValue())) {
					addStyle(context, "flex", "0");
					addStyleClass(context, "buttonDiv");
				} else {
					addStyle(context, "flex", "1");
				}
				getDirectionProperty(context).addListener((o, v, nv) -> {
					if (FlexDirection.ROW.equals(nv)) {
						addStyle(context, "flex", "0");
						addStyleClass(context, "buttonDiv");
					} else {
						addStyle(context, "flex", "1");
					}
				});
			});
		}
	}

	@Style(name = "background-color", value = "white")
	@StyleClass(path = { HtmlHyperLink.class, HtmlImg.class }, pos = { -1, 0 }, value = "img")
	@Children({ HtmlHyperLink.class, HtmlHyperLink.class })
	@Children(path = HtmlHyperLink.class, value = HtmlImg.class)
	@Attribute(path = { HtmlHyperLink.class, HtmlImg.class }, pos = { 1, 0 }, name = "src", value = "delete.png")
	@Attribute(path = { HtmlHyperLink.class, HtmlImg.class }, pos = { 0, 0 }, name = "src", value = "edit.png")
	@BindAction(path = HtmlHyperLink.class, pos = 1, value = REMOVE.class)
	@BindAction(path = HtmlHyperLink.class, pos = 0, value = { MOUNT.class, SET_SELECTION.class })
	public static class LinksDiv extends ButtonDiv {
	}
}