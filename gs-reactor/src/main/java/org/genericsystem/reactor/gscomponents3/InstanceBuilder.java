package org.genericsystem.reactor.gscomponents3;

import org.genericsystem.reactor.modelproperties.ComponentsDefaults;
import org.genericsystem.reactor.modelproperties.GSBuilderDefaults;

import org.genericsystem.reactor.htmltag.HtmlButton;

import java.util.HashMap;

import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.gscomponents.GSInputTextWithConversion;
import org.genericsystem.reactor.gscomponents2.GSInstanceBuilder.BuilderCell.BooleanHolderBuilder.CheckboxContainerBuildDiv.BooleanHolderBuilderInput;
import org.genericsystem.reactor.gscomponents2.GSInstanceBuilder.BuilderCell.HolderBuilder.HolderBuilderInput;
import org.genericsystem.reactor.gscomponents3.GSComposite.Content;
import org.genericsystem.reactor.gscomponents3.InstanceBuilder.AddButton;
import org.genericsystem.reactor.gscomponents3.InstanceBuilder.GSHolderBuilder;
import org.genericsystem.reactor.gscomponents3.InstanceBuilder.GSMultiCheckboxBuilder;
import org.genericsystem.reactor.gscomponents3.InstanceEditor.Checkbox;
import org.genericsystem.reactor.gscomponents3.InstanceEditor.CheckboxLabel;
import org.genericsystem.reactor.gscomponents3.InstanceEditor.ComponentAdderSelect;
import org.genericsystem.reactor.gscomponents3.InstanceEditor.GSHolderAdder;
import org.genericsystem.reactor.gscomponents3.InstanceEditor.GSMultiCheckbox;
import org.genericsystem.reactor.gscomponents3.InstancesTable.ButtonDiv;
import org.genericsystem.reactor.model.ContextAction.CREATE_INSTANCE;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.ObservableListExtractor.SUBINSTANCES_OF_RELATION_COMPONENT;
import org.genericsystem.reactor.model.ObservableValueSelector;
import org.genericsystem.reactor.model.ObservableValueSelector.MULTICHECKBOX_SELECTOR_RELATION;
import org.genericsystem.reactor.model.ObservableValueSelector.NON_MULTICHECKBOX_SELECTOR_RELATION;

import javafx.beans.binding.Bindings;
import javafx.beans.value.ObservableValue;

@Style(path = GSInputTextWithConversion.class, name = "flex", value = "1")
@Style(path = GSInputTextWithConversion.class, name = "width", value = "100%")
@ReactorDependencies({ GSInputTextWithConversion.class, Content.class, ButtonDiv.class })
@ReactorDependencies(path = Content.class, value = { GSHolderBuilder.class, GSMultiCheckboxBuilder.class })
@ReactorDependencies(path = ButtonDiv.class, value = AddButton.class)
@ForEach(path = Content.class, value = ObservableListExtractor.ATTRIBUTES_OF_TYPE.class)
@Select(path = { Content.class, GSHolderBuilder.class }, value = NON_MULTICHECKBOX_SELECTOR_RELATION.class)
@Select(path = { Content.class, GSMultiCheckbox.class }, value = MULTICHECKBOX_SELECTOR_RELATION.class)
public class InstanceBuilder extends GSComposite implements GSBuilderDefaults {

	@Override
	public void init() {
		createHoldersMapProperty();
		createComponentsMapProperty();
		createInvalidListProperty();
		createMultipleRelationProperty();
	}

	@ReactorDependencies(path = CheckboxLabel.class, value = CheckboxBuilder.class)
	@ForEach(path = CheckboxLabel.class, value = SUBINSTANCES_OF_RELATION_COMPONENT.class)
	public static class GSMultiCheckboxBuilder extends GSMultiCheckbox implements GSBuilderDefaults {
		@Override
		public void init() {
			addPrefixBinding(context -> getMultipleRelationProperty(context).getValue().put(context.getGeneric(), new HashMap<>()));
		}
	}

	public static class CheckboxBuilder extends Checkbox implements GSBuilderDefaults {
		@Override
		public void init() {
			addConvertedValueChangeListener((context, nva) -> {
				if (Boolean.TRUE.equals(nva))
					getMultipleRelationProperty(context).getValue().get(context.getGenerics()[1]).put(context.getGeneric(), getConvertedValueProperty(context));
				if (Boolean.FALSE.equals(nva))
					getMultipleRelationProperty(context).getValue().get(context.getGenerics()[1]).remove(context.getGeneric());
			});
		}
	}

	@Style(name = "flex", value = "1")
	@ReactorDependencies({ Header.class, Content.class })
	@ReactorDependencies(path = Header.class, value = { HolderBuilderInput.class, BooleanHolderBuilderInput.class })
	@ReactorDependencies(path = Content.class, value = ComponentAdderSelect.class)
	@Select(path = { Header.class, HolderBuilderInput.class }, value = ObservableValueSelector.LABEL_DISPLAYER_ATTRIBUTE.class)
	@Select(path = { Header.class, BooleanHolderBuilderInput.class }, value = ObservableValueSelector.CHECK_BOX_DISPLAYER_ATTRIBUTE.class)
	public static class GSHolderBuilder extends GSHolderAdder implements GSBuilderDefaults, ComponentsDefaults {

		@Override
		public void init() {
			createComponentsListProperty();
			addPostfixBinding(model -> {
				if (getComponentsMapProperty(model) != null)
					getComponentsMapProperty(model).getValue().put(model.getGeneric(), getComponentsProperty(model).getValue());
			});
		}
	}

	@Style(name = "flex", value = "1")
	@Style(name = "height", value = "100%")
	@Style(name = "width", value = "100%")
	@SetText("Add")
	@BindAction(CREATE_INSTANCE.class)
	public static class AddButton extends HtmlButton {

		@Override
		public void init() {
			bindAttribute(ReactorStatics.DISABLED, ReactorStatics.DISABLED,
					model -> Bindings.createStringBinding(
							() -> Boolean.TRUE.equals(getInvalidListProperty(model).getValue().stream().map(input -> input.getValue()).filter(bool -> bool != null).reduce(false, (a, b) -> a || b)) ? ReactorStatics.DISABLED : "",
							getInvalidListProperty(model).getValue().stream().toArray(ObservableValue[]::new)));
		}
	}
}
