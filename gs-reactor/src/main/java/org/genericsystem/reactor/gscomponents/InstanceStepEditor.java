package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.modelproperties.SelectionDefaults;
import org.genericsystem.reactor.modelproperties.StepperDefaults;

import org.genericsystem.reactor.htmltag.HtmlHyperLink;

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
import org.genericsystem.reactor.gscomponents.GSComposite.Content;
import org.genericsystem.reactor.gscomponents.GSComposite.Header;
import org.genericsystem.reactor.gscomponents.InstanceEditor.AttributeContent;
import org.genericsystem.reactor.gscomponents.InstanceEditor.GSHoldersEditor;
import org.genericsystem.reactor.gscomponents.InstanceEditor.GSMultiCheckbox;
import org.genericsystem.reactor.gscomponents.InstanceEditor.GSValueComponentsEditor;
import org.genericsystem.reactor.gscomponents.InstanceStepEditor.AttributeEdition;
import org.genericsystem.reactor.gscomponents.InstanceStepEditor.InstanceName;
import org.genericsystem.reactor.gscomponents.InstanceStepEditor.StepNavigator;
import org.genericsystem.reactor.gscomponents.InstancesTable.GSValueComponents;
import org.genericsystem.reactor.model.ContextAction.NEXT;
import org.genericsystem.reactor.model.ContextAction.PREVIOUS;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.ObservableValueSelector.TYPE_SELECTOR;

@Style(name = "flex", value = "1 1 0%")
@Style(name = "overflow", value = "hidden")
@ReverseFlexDirection(path = GSComposite.class)
@Style(path = { GSComposite.class, GSValueComponents.class }, pos = { -1, 0 }, name = "flex", value = "0.3")
@Style(path = { GSComposite.class, GSValueComponents.class }, name = "color", value = "white")
@Style(path = { GSComposite.class, StepNavigator.class }, name = "flex", value = "")
@GenericValueBackgroundColor(path = { GSComposite.class, GSValueComponents.class, GSDiv.class }, value = "#ea0084")
@Children({ InstanceName.class, AttributeEdition.class })
@Children(path = InstanceName.class, value = { GSValueComponents.class, GSValueComponentsEditor.class, StepNavigator.class })
@Children(path = AttributeEdition.class, value = { GSValueComponents.class, AttributeContent.class, StepNavigator.class })
@Children(path = { AttributeEdition.class, Content.class }, value = { GSHoldersEditor.class, GSMultiCheckbox.class })
@Children(path = { InstanceName.class, GSValueComponentsEditor.class }, value = { Header.class, Content.class })
@ForEach(path = AttributeEdition.class, value = ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
@ForEach(path = { AttributeEdition.class, AttributeContent.class }, value = ObservableListExtractor.NO_FOR_EACH.class)
@ForEach(path = { AttributeEdition.class, GSValueComponents.class, Content.class }, value = ObservableListExtractor.OTHER_COMPONENTS_2.class)
@Select(path = { InstanceName.class, GSValueComponents.class }, pos = { 0, 0 }, value = TYPE_SELECTOR.class)
@Stepper(switchClass = GSComposite.class, switchClassPos = 1, headerClass = GSComposite.class, headerClassPos = 0)
public class InstanceStepEditor extends GSDiv implements SelectionDefaults, StepperDefaults {

	public static class InstanceName extends GSComposite {
	}

	public static class AttributeEdition extends GSComposite {
	}

	@FlexDirectionStyle(FlexDirection.ROW)
	public static class HorizontalInstanceStepEditor extends InstanceStepEditor {

	}

	@Children({ PrevLink.class, NextLink.class })
	@Style(name = "justify-content", value = "space-between")
	@ReverseFlexDirection
	public static class StepNavigator extends GSDiv {
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
