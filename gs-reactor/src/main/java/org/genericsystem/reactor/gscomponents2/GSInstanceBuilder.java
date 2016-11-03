package org.genericsystem.reactor.gscomponents2;

import org.genericsystem.reactor.modelproperties.ConvertedValueDefaults;
import org.genericsystem.reactor.modelproperties.GSBuilderDefaults;

import org.genericsystem.reactor.htmltag.HtmlButton;

import java.util.List;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.ReverseFlexDirection;
import org.genericsystem.reactor.gscomponents.GSCheckBoxWithValue;
import org.genericsystem.reactor.gscomponents.GSDiv;
import org.genericsystem.reactor.gscomponents.GSInputTextWithConversion;
import org.genericsystem.reactor.gscomponents.InstancesTable;
import org.genericsystem.reactor.gscomponents2.GSCellDiv.CenteredFlexDiv;
import org.genericsystem.reactor.gscomponents2.GSCellDiv.GSComponentEditorDiv;
import org.genericsystem.reactor.gscomponents2.GSCellDiv.GSSubcellEditorDiv;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder.LinkAdder.ComponentAdder.ComponentAdderSelect;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellEditor.LinkEditor;
import org.genericsystem.reactor.gscomponents2.GSInstanceBuilder.AddButtonDiv;
import org.genericsystem.reactor.gscomponents2.GSInstanceBuilder.AddButtonDiv.AddButton;
import org.genericsystem.reactor.gscomponents2.GSInstanceBuilder.BuilderCell;
import org.genericsystem.reactor.gscomponents2.GSInstanceBuilder.BuilderCell.BooleanHolderBuilder;
import org.genericsystem.reactor.gscomponents2.GSInstanceBuilder.BuilderCell.BooleanHolderBuilder.CheckboxContainerBuildDiv;
import org.genericsystem.reactor.gscomponents2.GSInstanceBuilder.BuilderCell.BooleanHolderBuilder.CheckboxContainerBuildDiv.BooleanHolderBuilderInput;
import org.genericsystem.reactor.gscomponents2.GSInstanceBuilder.BuilderCell.HolderBuilder;
import org.genericsystem.reactor.gscomponents2.GSInstanceBuilder.BuilderCell.HolderBuilder.HolderBuilderInput;
import org.genericsystem.reactor.gscomponents2.GSInstanceBuilder.BuilderCell.LinkBuilder;
import org.genericsystem.reactor.gscomponents2.GSInstanceBuilder.BuilderCell.LinkBuilder.ComponentBuilder;
import org.genericsystem.reactor.gscomponents2.GSInstanceBuilder.InstanceNameBuilder;
import org.genericsystem.reactor.gscomponents2.GSInstanceBuilder.InstanceNameBuilder.InstanceNameBuilderInput;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.ObservableValueSelector;

import javafx.beans.binding.Bindings;
import javafx.beans.value.ObservableValue;

@Children({ InstanceNameBuilder.class, BuilderCell.class, AddButtonDiv.class })
@Style(name = "flex", value = "1")
@ReverseFlexDirection
public class GSInstanceBuilder extends GSDiv implements GSBuilderDefaults {

	@Override
	public void init() {
		createValueComponentsMapProperty();
		createInvalidListProperty();
	}

	// For the creation of the instance’s value.
	@Children(InstanceNameBuilderInput.class)
	public static class InstanceNameBuilder extends GSSubcellEditorDiv {
		@Style(name = "flex", value = "1")
		@Style(name = "height", value = "100%")
		@Style(name = "width", value = "100%")
		public static class InstanceNameBuilderInput extends GSInputTextWithConversion {
		}
	}

	// Creation of holders/links.
	@ForEach(ObservableListExtractor.ATTRIBUTES_OF_TYPE.class)
	@Children({ HolderBuilder.class, BooleanHolderBuilder.class, LinkBuilder.class })
	public static class BuilderCell extends GSSubcellEditorDiv {

		// Creation of non-boolean holders.
		@Select(ObservableValueSelector.LABEL_DISPLAYER_ATTRIBUTE.class)
		@Children(HolderBuilderInput.class)
		public static class HolderBuilder extends GSSubcellEditorDiv {

			@Style(name = "flex", value = "1")
			@Style(name = "height", value = "100%")
			@Style(name = "width", value = "100%")
			public static class HolderBuilderInput extends GSInputTextWithConversion implements GSBuilderDefaults {

				@Override
				public void init() {
					addPrefixBinding(context -> {
						if (getGenericValueComponents(context) != null)
							getGenericValueComponents(context).getValue().get(context.getGeneric()).setGenericValue(getConvertedValueProperty(context));
						if (getInvalidListProperty(context) != null)
							getInvalidListProperty(context).getValue().add(getInvalidObservable(context));
					});
				}
			}
		}

		// Creation of boolean holders.
		@Select(ObservableValueSelector.CHECK_BOX_DISPLAYER_ATTRIBUTE.class)
		@Children(CheckboxContainerBuildDiv.class)
		public static class BooleanHolderBuilder extends GSSubcellEditorDiv {
			@Children(BooleanHolderBuilderInput.class)
			public static class CheckboxContainerBuildDiv extends CenteredFlexDiv {
				public static class BooleanHolderBuilderInput extends GSCheckBoxWithValue implements GSBuilderDefaults {

					@Override
					public void init() {
						addPrefixBinding(context -> {
							if (getGenericValueComponents(context) != null)
								getGenericValueComponents(context).getValue().get(context.getGeneric()).setGenericValue(getConvertedValueProperty(context));
						});
					}
				}
			}
		}

		// Creation of links.
		@Children(ComponentBuilder.class)
		public static class LinkBuilder extends LinkEditor implements GSBuilderDefaults {

			@Override
			public void init() {
				super.init();
				addPostfixBinding(context -> {
					if (getGenericValueComponents(context) != null)
						getGenericValueComponents(context).getValue().get(context.getGeneric()).setComponents(getComponentsProperty(context).getValue());
				});
			}

			@ForEach(ObservableListExtractor.OTHER_COMPONENTS_1.class)
			@Children(ComponentAdderSelect.class)
			public static class ComponentBuilder extends GSComponentEditorDiv {
			}
		}
	}

	// “Add” button.
	@Children(AddButton.class)
	public static class AddButtonDiv extends InstancesTable.ButtonDiv {
		@Style(name = "flex", value = "1")
		@Style(name = "height", value = "100%")
		@Style(name = "width", value = "100%")
		public static class AddButton extends HtmlButton {

			@Override
			public void init() {
				setText("Add");
				bindAttribute(ReactorStatics.DISABLED, ReactorStatics.DISABLED,
						context -> Bindings.createStringBinding(
								() -> Boolean.TRUE.equals(getInvalidListProperty(context).getValue().stream().map(input -> input.getValue()).filter(bool -> bool != null).reduce(false, (a, b) -> a || b)) ? ReactorStatics.DISABLED : "",
								getInvalidListProperty(context).getValue().stream().toArray(ObservableValue[]::new)));
				bindAction(context -> {
					ConvertedValueDefaults input = getParent().getParent().find(InstanceNameBuilder.class).find(InstanceNameBuilderInput.class);
					Generic newInstance = context.getGeneric().setInstance(input.getConvertedValueProperty(context).getValue());
					for (Entry<Generic, GenericValueComponents> entry : getGenericValueComponents(context).getValue().entrySet()) {
						List<Generic> selectedGenerics = entry.getValue().getComponents().stream().filter(obs -> obs.getValue() != null).map(obs -> obs.getValue().getGeneric()).filter(gen -> gen != null).collect(Collectors.toList());
						if (!selectedGenerics.isEmpty() && selectedGenerics.size() + 1 == entry.getKey().getComponents().size())
							newInstance.setHolder(entry.getKey(), entry.getValue().getGenericValue().getValue(), selectedGenerics.stream().toArray(Generic[]::new));
						entry.getValue().getGenericValue().setValue(null);
						entry.getValue().getComponents().stream().forEach(sel -> sel.setValue(null));
					}
					input.getConvertedValueProperty(context).setValue(null);
				});
			}
		}
	}
}
