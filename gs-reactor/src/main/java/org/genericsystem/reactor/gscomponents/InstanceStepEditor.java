package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.modelproperties.SelectionDefaults;
import org.genericsystem.reactor.modelproperties.StepperDefaults;

import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Stepper;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Style.GenericValueBackgroundColor;
import org.genericsystem.reactor.annotations.Style.ReverseFlexDirection;
import org.genericsystem.reactor.gscomponents.Composite.Content;
import org.genericsystem.reactor.gscomponents.Composite.Header;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;
import org.genericsystem.reactor.gscomponents.InstanceEditor.AttributeContent;
import org.genericsystem.reactor.gscomponents.InstanceEditor.HoldersEditor;
import org.genericsystem.reactor.gscomponents.InstanceEditor.MultiCheckbox;
import org.genericsystem.reactor.gscomponents.InstanceEditor.ValueComponentsEditor;
import org.genericsystem.reactor.gscomponents.InstanceStepEditor.AttributeEdition;
import org.genericsystem.reactor.gscomponents.InstanceStepEditor.InstanceName;
import org.genericsystem.reactor.gscomponents.InstanceStepEditor.StepNavigator;
import org.genericsystem.reactor.gscomponents.InstancesTable.ValueComponents;
import org.genericsystem.reactor.model.ContextAction.NEXT;
import org.genericsystem.reactor.model.ContextAction.PREVIOUS;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.ObservableValueSelector;
import org.genericsystem.reactor.model.ObservableValueSelector.TYPE_SELECTOR;

@Style(name = "flex", value = "1 1 0%")
@Style(name = "overflow", value = "hidden")
@ReverseFlexDirection(path = Composite.class)
@Style(path = { Composite.class, ValueComponents.class }, pos = { -1, 0 }, name = "flex", value = "0.3")
@Style(path = { Composite.class, ValueComponents.class }, name = "color", value = "white")
@Style(path = { Composite.class, StepNavigator.class }, name = "flex", value = "")
@GenericValueBackgroundColor(path = { Composite.class, ValueComponents.class, FlexDiv.class }, value = "#ea0084")
@Children({ InstanceName.class, AttributeEdition.class })
@Children(path = InstanceName.class, value = { ValueComponents.class, ValueComponentsEditor.class,/* StepNavigator.class*/ })
@Children(path = AttributeEdition.class, value = { ValueComponents.class, AttributeContent.class, /*StepNavigator.class*/ })
@Children(path = { AttributeEdition.class, Content.class }, value = { HoldersEditor.class, MultiCheckbox.class })
@Children(path = { InstanceName.class, ValueComponentsEditor.class }, value = { Header.class, Content.class })
@ForEach(path = AttributeEdition.class, value = ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
@ForEach(path = { AttributeEdition.class, AttributeContent.class }, value = ObservableListExtractor.NO_FOR_EACH.class)
@ForEach(path = { AttributeEdition.class, ValueComponents.class, Content.class }, value = ObservableListExtractor.OTHER_COMPONENTS_2.class)
@Select(path = { InstanceName.class, ValueComponents.class }, pos = { 0, 0 }, value = TYPE_SELECTOR.class)
@Select(path = { AttributeEdition.class, ValueComponents.class, Header.class }, value = ObservableValueSelector.STRICT_ATTRIBUTE_SELECTOR_OR_CHECK_BOX_DISPLAYER_ATTRIBUTE.class)
//@Stepper(switchClass = Composite.class, switchClassPos = 1, headerClass = Composite.class, headerClassPos = 0)
public class InstanceStepEditor extends FlexDiv implements SelectionDefaults, StepperDefaults {

	public static class InstanceName extends Composite {
	}

	public static class AttributeEdition extends Composite {
	}

	@FlexDirectionStyle(FlexDirection.ROW)
	public static class HorizontalInstanceStepEditor extends InstanceStepEditor {

	}

	@Children({ PrevLink.class, NextLink.class })
	@Style(name = "justify-content", value = "space-between")
	@ReverseFlexDirection
	public static class StepNavigator extends FlexDiv {
	}

	@SetText("<")
	@BindAction(PREVIOUS.class)
	public static class PrevLink extends HtmlHyperLink {
	}

	@SetText(">")
	@BindAction(NEXT.class)
	public static class NextLink extends HtmlHyperLink {
	}
}
