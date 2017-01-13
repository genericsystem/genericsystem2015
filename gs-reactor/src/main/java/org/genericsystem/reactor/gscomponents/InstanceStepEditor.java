package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.Step;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Style.ReverseFlexDirection;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.annotations.Switcher;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;
import org.genericsystem.reactor.gscomponents.InstanceEditor.AttributeContent;
import org.genericsystem.reactor.gscomponents.InstanceEditor.AttributeEdition;
import org.genericsystem.reactor.gscomponents.InstanceEditor.InstanceName;
import org.genericsystem.reactor.gscomponents.InstanceEditor.ValueComponentsEditor;
import org.genericsystem.reactor.gscomponents.InstanceStepEditor.StepNavigator;
import org.genericsystem.reactor.gscomponents.InstancesTable.ValueComponents;

@Style(path = { Composite.class, StepNavigator.class }, name = "flex", value = "")
@Children(path = InstanceName.class, value = { ValueComponents.class, ValueComponentsEditor.class, StepNavigator.class })
@Children(path = AttributeEdition.class, value = { ValueComponents.class, AttributeContent.class, StepNavigator.class })
@Switcher(InstanceName.class)
@Step(path = InstanceName.class, next = AttributeEdition.class)
@Step(path = AttributeEdition.class, next = AttributeEdition.class)
public class InstanceStepEditor extends InstanceEditor {

	@FlexDirectionStyle(FlexDirection.ROW)
	public static class HorizontalInstanceStepEditor extends InstanceStepEditor {

	}

	@Children({ PrevLink.class, NextLink.class })
	@Style(name = "justify-content", value = "space-between")
	@ReverseFlexDirection
	public static class StepNavigator extends FlexDiv {
	}

	@BindAction(Controller.PrevAction.class)
	@BindText(Controller.PrevTextBinding.class)
	@Switch(Controller.PrevSwitcher.class)
	public static class PrevLink extends HtmlHyperLink {
	}

	@BindAction(Controller.NextAction.class)
	@BindText(Controller.NextTextBinding.class)
	@Switch(Controller.NextSwitcher.class)
	@Style(name = "margin-top", value = "auto")
	public static class NextLink extends HtmlHyperLink {
	}
}
