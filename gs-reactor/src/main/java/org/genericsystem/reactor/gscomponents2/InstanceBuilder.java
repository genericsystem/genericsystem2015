package org.genericsystem.reactor.gscomponents2;

import org.genericsystem.reactor.modelproperties.ComponentsDefaults;
import org.genericsystem.reactor.modelproperties.ConvertedValueDefaults;
import org.genericsystem.reactor.modelproperties.GSBuilderDefaults;

import org.genericsystem.reactor.htmltag.HtmlButton;

import java.io.Serializable;
import java.util.List;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.Styles.Flex;
import org.genericsystem.reactor.annotations.Styles.Height;
import org.genericsystem.reactor.annotations.Styles.ReverseFlexDirection;
import org.genericsystem.reactor.annotations.Styles.Style;
import org.genericsystem.reactor.annotations.Styles.Width;
import org.genericsystem.reactor.gscomponents.GSInputTextWithConversion;
import org.genericsystem.reactor.gscomponents2.GSInstanceBuilder.BuilderCell.BooleanHolderBuilder.CheckboxContainerBuildDiv.BooleanHolderBuilderInput;
import org.genericsystem.reactor.gscomponents2.GSInstanceBuilder.BuilderCell.HolderBuilder.HolderBuilderInput;
import org.genericsystem.reactor.gscomponents2.InstanceBuilder.AddButton;
import org.genericsystem.reactor.gscomponents2.InstanceBuilder.GSHolderBuilder;
import org.genericsystem.reactor.gscomponents2.InstanceEditor.ComponentAdderSelect;
import org.genericsystem.reactor.gscomponents2.InstanceEditor.GSHolderAdder;
import org.genericsystem.reactor.gscomponents2.InstancesTable.ButtonDiv;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.ObservableValueSelector;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

@ReverseFlexDirection
@Style(path = GSInputTextWithConversion.class, name = "flex", value = "1")
@Style(path = GSInputTextWithConversion.class, name = "width", value = "100%")
@ReactorDependencies({ GSInputTextWithConversion.class, GSHolderBuilder.class, ButtonDiv.class })
@ReactorDependencies(path = ButtonDiv.class, value = AddButton.class)
@ForEach(path = { GSHolderBuilder.class }, value = ObservableListExtractor.ATTRIBUTES_OF_TYPE.class)
public class InstanceBuilder extends GSComposite implements GSBuilderDefaults {

	@Override
	public void init() {
		createHoldersMapProperty();
		createComponentsMapProperty();
		createInvalidListProperty();
	}

	@Style(name = "flex", value = "1")
	@ReactorDependencies({ Header.class, Content.class })
	@ReactorDependencies(path = Header.class, value = { HolderBuilderInput.class, BooleanHolderBuilderInput.class })
	@ReactorDependencies(path = Content.class, value = ComponentAdderSelect.class)
	@Select(path = { Header.class, HolderBuilderInput.class }, value = ObservableValueSelector.LABEL_DISPLAYER_0.class)
	@Select(path = { Header.class, BooleanHolderBuilderInput.class }, value = ObservableValueSelector.CHECK_BOX_DISPLAYER_0.class)
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

	@Flex("1")
	@Height("100%")
	@Width("100%")
	public static class AddButton extends HtmlButton {

		@Override
		public void init() {
			setText("Add");
			bindAttribute(ReactorStatics.DISABLED, ReactorStatics.DISABLED,
					model -> Bindings.createStringBinding(
							() -> Boolean.TRUE.equals(getInvalidListProperty(model).getValue().stream().map(input -> input.getValue()).filter(bool -> bool != null).reduce(false, (a, b) -> a || b)) ? ReactorStatics.DISABLED : "",
							getInvalidListProperty(model).getValue().stream().toArray(ObservableValue[]::new)));
			bindAction(model -> {
				ConvertedValueDefaults input = getParent().getParent().find(GSInputTextWithConversion.class);
				Generic newInstance = model.getGeneric().setInstance(input.getConvertedValueProperty(model).getValue());
				for (Entry<Generic, Property<Serializable>> entry : getHoldersMapProperty(model).getValue().entrySet())
					if (entry.getValue().getValue() != null) {
						newInstance.setHolder(entry.getKey(), entry.getValue().getValue());
						entry.getValue().setValue(null);
					}
				for (Entry<Generic, List<Property<Context>>> entry : getComponentsMapProperty(model).getValue().entrySet()) {
					List<Generic> selectedGenerics = entry.getValue().stream().filter(obs -> obs.getValue() != null).map(obs -> obs.getValue().getGeneric()).filter(gen -> gen != null).collect(Collectors.toList());
					if (!selectedGenerics.isEmpty() && selectedGenerics.size() + 1 == entry.getKey().getComponents().size())
						newInstance.setHolder(entry.getKey(), null, selectedGenerics.stream().toArray(Generic[]::new));
					entry.getValue().stream().forEach(sel -> sel.setValue(null));
				}
				input.getConvertedValueProperty(model).setValue(null);
			});
		}
	}
}
