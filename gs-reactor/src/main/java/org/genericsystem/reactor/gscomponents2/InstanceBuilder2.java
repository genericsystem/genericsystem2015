package org.genericsystem.reactor.gscomponents2;

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
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.context.ObservableValueSelector;
import org.genericsystem.reactor.contextproperties.ConvertedValueDefaults;
import org.genericsystem.reactor.contextproperties.GSBuilderDefaults;
import org.genericsystem.reactor.gscomponents.CheckBoxWithValue;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.InputTextWithConversion;
import org.genericsystem.reactor.gscomponents.InstancesTable;
import org.genericsystem.reactor.gscomponents2.CellDiv.CenteredFlexDiv;
import org.genericsystem.reactor.gscomponents2.CellDiv.ComponentEditorDiv;
import org.genericsystem.reactor.gscomponents2.CellDiv.SubcellEditorDiv;
import org.genericsystem.reactor.gscomponents2.Editor2.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder.LinkAdder.ComponentAdder.ComponentAdderSelect;
import org.genericsystem.reactor.gscomponents2.Editor2.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellEditor.LinkEditor;
import org.genericsystem.reactor.gscomponents2.InstanceBuilder2.AddButtonDiv;
import org.genericsystem.reactor.gscomponents2.InstanceBuilder2.AddButtonDiv.AddButton2;
import org.genericsystem.reactor.gscomponents2.InstanceBuilder2.BuilderCell;
import org.genericsystem.reactor.gscomponents2.InstanceBuilder2.BuilderCell.BooleanHolderBuilder;
import org.genericsystem.reactor.gscomponents2.InstanceBuilder2.BuilderCell.BooleanHolderBuilder.CheckboxContainerBuildDiv;
import org.genericsystem.reactor.gscomponents2.InstanceBuilder2.BuilderCell.BooleanHolderBuilder.CheckboxContainerBuildDiv.BooleanHolderBuilderInput;
import org.genericsystem.reactor.gscomponents2.InstanceBuilder2.BuilderCell.HolderBuilder;
import org.genericsystem.reactor.gscomponents2.InstanceBuilder2.BuilderCell.HolderBuilder.HolderBuilderInput;
import org.genericsystem.reactor.gscomponents2.InstanceBuilder2.BuilderCell.LinkBuilder;
import org.genericsystem.reactor.gscomponents2.InstanceBuilder2.BuilderCell.LinkBuilder.ComponentBuilder;
import org.genericsystem.reactor.gscomponents2.InstanceBuilder2.InstanceNameBuilder;
import org.genericsystem.reactor.gscomponents2.InstanceBuilder2.InstanceNameBuilder.InstanceNameBuilderInput;

import javafx.beans.binding.Bindings;
import javafx.beans.value.ObservableValue;

@Children({ InstanceNameBuilder.class, BuilderCell.class, AddButtonDiv.class })
@Style(name = "flex", value = "1")
@ReverseFlexDirection
public class InstanceBuilder2 extends FlexDiv implements GSBuilderDefaults {

	@Override
	public void init() {
		createValueComponentsMapProperty();
		createInvalidListProperty();
	}

	// For the creation of the instance’s value.
	@Children(InstanceNameBuilderInput.class)
	public static class InstanceNameBuilder extends SubcellEditorDiv {
		@Style(name = "flex", value = "1")
		@Style(name = "height", value = "100%")
		@Style(name = "width", value = "100%")
		public static class InstanceNameBuilderInput extends InputTextWithConversion {
		}
	}

	// Creation of holders/links.
	@ForEach(ObservableListExtractor.ATTRIBUTES_OF_TYPE.class)
	@Children({ HolderBuilder.class, BooleanHolderBuilder.class, LinkBuilder.class })
	public static class BuilderCell extends SubcellEditorDiv {

		// Creation of non-boolean holders.
		@Select(ObservableValueSelector.LABEL_DISPLAYER_ATTRIBUTE.class)
		@Children(HolderBuilderInput.class)
		public static class HolderBuilder extends SubcellEditorDiv {

			@Style(name = "flex", value = "1")
			@Style(name = "height", value = "100%")
			@Style(name = "width", value = "100%")
			public static class HolderBuilderInput extends InputTextWithConversion implements GSBuilderDefaults {

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
		public static class BooleanHolderBuilder extends SubcellEditorDiv {
			@Children(BooleanHolderBuilderInput.class)
			public static class CheckboxContainerBuildDiv extends CenteredFlexDiv {
				public static class BooleanHolderBuilderInput extends CheckBoxWithValue implements GSBuilderDefaults {

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
			public static class ComponentBuilder extends ComponentEditorDiv {
			}
		}
	}

	// “Add” button.
	@Children(AddButton2.class)
	public static class AddButtonDiv extends InstancesTable.ButtonDiv {
		@Style(name = "flex", value = "1")
		@Style(name = "height", value = "100%")
		@Style(name = "width", value = "100%")
		public static class AddButton2 extends HtmlButton {

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
						Generic[] selectedGenerics = entry.getValue().getComponents().entrySet().stream().filter(entry_ -> entry_.getValue() != null && entry_.getValue().getValue() != null).map(entry_ -> entry_.getKey().setInstance(entry_.getValue().getValue())).filter(gen -> gen != null).toArray(Generic[]::new);
						if ((entry.getValue().getGenericValue().getValue() != null || selectedGenerics.length != 0) && selectedGenerics.length + 1 == entry.getKey().getComponents().size())
							newInstance.setHolder(entry.getKey(), entry.getValue().getGenericValue().getValue(), selectedGenerics);
						entry.getValue().getGenericValue().setValue(null);
						entry.getValue().getComponents().values().stream().forEach(sel -> sel.setValue(null));
					}
					input.getConvertedValueProperty(context).setValue(null);
				});
			}
		}
	}
}
