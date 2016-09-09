package org.genericsystem.reactor.gs3;

import java.io.Serializable;
import java.util.List;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.annotations.Parent;
import org.genericsystem.reactor.gs.GSCheckBoxWithValue;
import org.genericsystem.reactor.gs.GSDiv;
import org.genericsystem.reactor.gs.GSInputTextWithConversion;
import org.genericsystem.reactor.gs3.FlexStyle.ReversedFlexStyle;
import org.genericsystem.reactor.gs3.GSEditor.ComponentAdderSelect;
import org.genericsystem.reactor.gs3.GSEditor.LinkEditor;
import org.genericsystem.reactor.gstag.HtmlButton;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.modelproperties.ConvertedValueDefaults;
import org.genericsystem.reactor.modelproperties.GSBuilderDefaults;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

@Parent(Table.class)
public class GSInstanceBuilder extends GSDiv implements ReversedFlexStyle, GSBuilderDefaults {

	@Override
	public void init() {
		createHoldersMapProperty();
		createComponentsMapProperty();
		createInvalidListProperty();
	}

	// For the creation of the instance’s value.
	@Parent(GSInstanceBuilder.class)
	public static class InstanceNameBuilder extends GSDiv implements SubCellEditorStyle {
	}

	@Parent(InstanceNameBuilder.class)
	public static class InstanceNameBuilderInput extends GSInputTextWithConversion implements FullSizeStyle {
	}

	// Creation of holders/links.
	@Parent(GSInstanceBuilder.class)
	public static class BuilderCell extends GSDiv implements SubCellEditorStyle {

		@Override
		public void init() {
			forEach(ObservableListExtractor.ATTRIBUTES_OF_TYPE);
		}
	}

	// Creation of non-boolean holders.
	@Parent(BuilderCell.class)
	public static class HolderBuilder extends GSDiv implements SubCellEditorStyle {

		@Override
		public void init() {
			select(gs -> gs[0].getComponents().size() < 2 && !Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
		}
	}

	@Parent(HolderBuilder.class)
	public static class HolderBuilderInput extends GSInputTextWithConversion implements FullSizeStyle, GSBuilderDefaults {

		@Override
		public void init() {
			addPrefixBinding(model -> {
				if (getHoldersMapProperty(model) != null)
					getHoldersMapProperty(model).getValue().put(model.getGeneric(), getConvertedValueProperty(model));
				if (getInvalidListProperty(model) != null)
					getInvalidListProperty(model).getValue().add(getInvalidObservable(model));
			});
		}
	}

	// Creation of boolean holders.
	@Parent(BuilderCell.class)
	public static class BooleanHolderBuilder extends GSDiv implements SubCellEditorStyle {

		@Override
		public void init() {
			select(gs -> gs[0].getComponents().size() < 2 && Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
		}
	}

	@Parent(BooleanHolderBuilder.class)
	public static class CheckboxContainerBuildDiv extends GSDiv implements CenteredFlex {

	}

	@Parent(CheckboxContainerBuildDiv.class)
	public static class BooleanHolderBuilderInput extends GSCheckBoxWithValue implements GSBuilderDefaults {

		@Override
		public void init() {
			addPrefixBinding(model -> {
				if (getHoldersMapProperty(model) != null)
					getHoldersMapProperty(model).getValue().put(model.getGeneric(), getConvertedValueProperty(model));
			});
		}
	}

	// Creation of links.
	@Parent(BuilderCell.class)
	public static class LinkBuilder extends LinkEditor implements GSBuilderDefaults {

		@Override
		public void init() {
			super.init();
			addPostfixBinding(model -> {
				if (getComponentsMapProperty(model) != null)
					getComponentsMapProperty(model).getValue().put(model.getGeneric(), getComponentsProperty(model).getValue());
			});
		}
	}

	@Parent(LinkBuilder.class)
	public static class ComponentBuilder extends GSDiv implements ComponentEditorStyle {

		@Override
		public void init() {
			forEach((ObservableListExtractor) gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1])));
		}
	}

	@Parent(ComponentBuilder.class)
	public static class ComponentBuilderSelect extends ComponentAdderSelect {
	}

	// “Add” button.
	@Parent(GSInstanceBuilder.class)
	public static class AddButtonDiv extends GSDiv implements ButtonStyle {
	}

	@Parent(AddButtonDiv.class)
	public static class AddButton extends HtmlButton implements FullSizeStyle {

		@Override
		public void init() {
			setText("Add");
			bindAttribute(ReactorStatics.DISABLED, ReactorStatics.DISABLED,
					model -> Bindings.createStringBinding(
							() -> Boolean.TRUE.equals(getInvalidListProperty(model).getValue().stream().map(input -> input.getValue()).filter(bool -> bool != null).reduce(false, (a, b) -> a || b)) ? ReactorStatics.DISABLED : "",
							getInvalidListProperty(model).getValue().stream().toArray(ObservableValue[]::new)));
			bindAction(model -> {
				ConvertedValueDefaults input = (ConvertedValueDefaults) find(InstanceNameBuilderInput.class);
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
