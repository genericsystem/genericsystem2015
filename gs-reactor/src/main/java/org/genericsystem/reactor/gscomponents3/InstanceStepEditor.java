package org.genericsystem.reactor.gscomponents3;

import org.genericsystem.reactor.modelproperties.SelectionDefaults;
import org.genericsystem.reactor.modelproperties.StepperDefaults;

import org.genericsystem.reactor.htmltag.HtmlHyperLink;

import org.genericsystem.reactor.gscomponents3.GSComposite.Content;
import org.genericsystem.reactor.gscomponents3.GSComposite.Header;
import org.genericsystem.reactor.gscomponents3.InstanceEditor.AttributeContent;
import org.genericsystem.reactor.gscomponents3.InstanceEditor.GSHoldersEditor;
import org.genericsystem.reactor.gscomponents3.InstanceEditor.GSMultiCheckbox;
import org.genericsystem.reactor.gscomponents3.InstanceEditor.GSValueComponentsEditor;
import org.genericsystem.reactor.gscomponents3.InstanceStepEditor.StepNavigator;
import org.genericsystem.reactor.gscomponents3.InstancesTable.GSValueComponents;
import org.genericsystem.reactor.gscomponents3.Table.ContentRow;
import org.genericsystem.reactor.gscomponents3.Table.HeaderRow;

import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Stepper;
import org.genericsystem.reactor.annotations.Styles.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Styles.GenericValueBackgroundColor;
import org.genericsystem.reactor.annotations.Styles.ReverseFlexDirection;
import org.genericsystem.reactor.annotations.Styles.Style;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.GSDiv;
import org.genericsystem.reactor.model.ContextAction.NEXT;
import org.genericsystem.reactor.model.ContextAction.PREVIOUS;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.ObservableValueSelector.TYPE_SELECTOR;

@Style(path = { HeaderRow.class, GSValueComponents.class }, name = "flex", value = "0.3")
@Style(path = { ContentRow.class, GSValueComponents.class }, name = "flex", value = "0.3")
@Style(path = ContentRow.class, name = "flex", value = "1")
@Style(path = { HeaderRow.class, GSValueComponents.class }, name = "color", value = "white")
@Style(path = { ContentRow.class, GSValueComponents.class }, name = "color", value = "white")
@GenericValueBackgroundColor(path = { HeaderRow.class, GSValueComponents.class, Content.class }, value = "#ea0084")
@GenericValueBackgroundColor(path = { HeaderRow.class, GSValueComponents.class, Header.class }, value = "#ea0084")
@GenericValueBackgroundColor(path = { ContentRow.class, GSValueComponents.class, Content.class }, value = "#ea0084")
@GenericValueBackgroundColor(path = { ContentRow.class, GSValueComponents.class, Header.class }, value = "#ea0084")
@ReactorDependencies({ HeaderRow.class, ContentRow.class })
@ReactorDependencies(path = HeaderRow.class, value = { GSValueComponents.class, Content.class, StepNavigator.class })
@ReactorDependencies(path = { HeaderRow.class, Content.class }, value = GSValueComponentsEditor.class)
@ReactorDependencies(path = ContentRow.class, value = { GSValueComponents.class, AttributeContent.class, StepNavigator.class })
@ReactorDependencies(path = { ContentRow.class, Content.class }, value = { GSHoldersEditor.class, GSMultiCheckbox.class })
@ReactorDependencies(path = { HeaderRow.class, Content.class, GSValueComponentsEditor.class }, value = { Header.class, Content.class })
@ForEach(path = ContentRow.class, value = ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
@ForEach(path = { ContentRow.class, AttributeContent.class }, value = ObservableListExtractor.NO_FOR_EACH.class)
@ForEach(path = { ContentRow.class, GSValueComponents.class, Content.class }, value = ObservableListExtractor.OTHER_COMPONENTS_2.class)
@Select(path = { HeaderRow.class, GSValueComponents.class }, value = TYPE_SELECTOR.class)
@Stepper(switchClass = ContentRow.class, headerClass = HeaderRow.class)
public class InstanceStepEditor extends Table implements SelectionDefaults, StepperDefaults {

	@FlexDirectionStyle(FlexDirection.ROW)
	public static class HorizontalInstanceStepEditor extends InstanceStepEditor {

	}

	@ReactorDependencies({ PrevLink.class, NextLink.class })
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
