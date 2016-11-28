package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Stepper;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Style.ReverseFlexDirection;
import org.genericsystem.reactor.context.ContextAction.NEXT;
import org.genericsystem.reactor.context.ContextAction.PREVIOUS;
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
@Stepper(switchClass = Composite.class, switchClassPos = 1, headerClass = Composite.class, headerClassPos = 0)
public class InstanceStepEditor extends InstanceEditor {

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
