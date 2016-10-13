package org.genericsystem.reactor.gscomponents2;

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
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.ReverseFlexDirection;
import org.genericsystem.reactor.gscomponents.GSCheckBoxWithValue;
import org.genericsystem.reactor.gscomponents.GSDiv;
import org.genericsystem.reactor.gscomponents.GSInputTextWithConversion;
import org.genericsystem.reactor.gscomponents2.GSCellDiv.CenteredFlexDiv;
import org.genericsystem.reactor.gscomponents2.GSCellDiv.GSComponentEditorDiv;
import org.genericsystem.reactor.gscomponents2.GSCellDiv.GSSubcellEditorDiv;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder.LinkAdder.ComponentAdder.ComponentAdderSelect;
import org.genericsystem.reactor.gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellEditor.LinkEditor;
import org.genericsystem.reactor.gscomponents2.GSInstanceBuilder.AddButtonDiv.AddButton;
import org.genericsystem.reactor.gscomponents2.GSInstanceBuilder.BuilderCell.BooleanHolderBuilder.CheckboxContainerBuildDiv.BooleanHolderBuilderInput;
import org.genericsystem.reactor.gscomponents2.GSInstanceBuilder.BuilderCell.HolderBuilder.HolderBuilderInput;
import org.genericsystem.reactor.gscomponents2.GSInstanceBuilder.BuilderCell.LinkBuilder.ComponentBuilder.ComponentBuilderSelect;
import org.genericsystem.reactor.gscomponents2.GSInstanceBuilder.InstanceNameBuilder.InstanceNameBuilderInput;
import org.genericsystem.reactor.gscomponents3.InstancesTable;
import org.genericsystem.reactor.model.ObservableListExtractor;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

@ReactorDependencies({ InstanceNameBuilderInput.class, HolderBuilderInput.class, BooleanHolderBuilderInput.class, ComponentBuilderSelect.class, AddButton.class })
@Style(name = "flex", value = "1")
@ReverseFlexDirection
public class GSInstanceBuilder extends GSDiv implements GSBuilderDefaults {

	public GSInstanceBuilder() {
		super();
	}

	// public GSInstanceBuilder(Tag parent) {
	// super(parent, GSInstanceBuilder.class);
	// }

	@Override
	public void init() {
		createHoldersMapProperty();
		createComponentsMapProperty();
		createInvalidListProperty();
	}

	// For the creation of the instance’s value.
	public static class InstanceNameBuilder extends GSSubcellEditorDiv {
		@Style(name = "flex", value = "1")
		@Style(name = "height", value = "100%")
		@Style(name = "width", value = "100%")
		public static class InstanceNameBuilderInput extends GSInputTextWithConversion {
		}
	}

	// Creation of holders/links.
	public static class BuilderCell extends GSSubcellEditorDiv {

		@Override
		public void init() {
			forEach(ObservableListExtractor.ATTRIBUTES_OF_TYPE);
		}

		// Creation of non-boolean holders.
		public static class HolderBuilder extends GSSubcellEditorDiv {

			@Override
			public void init() {
				select(gs -> gs[0].getComponents().size() < 2 && !Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
			}

			@Style(name = "flex", value = "1")
			@Style(name = "height", value = "100%")
			@Style(name = "width", value = "100%")
			public static class HolderBuilderInput extends GSInputTextWithConversion implements GSBuilderDefaults {

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
		}

		// Creation of boolean holders.
		public static class BooleanHolderBuilder extends GSSubcellEditorDiv {

			@Override
			public void init() {
				select(gs -> gs[0].getComponents().size() < 2 && Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
			}

			public static class CheckboxContainerBuildDiv extends CenteredFlexDiv {
				public static class BooleanHolderBuilderInput extends GSCheckBoxWithValue implements GSBuilderDefaults {

					@Override
					public void init() {
						addPrefixBinding(model -> {
							if (getHoldersMapProperty(model) != null)
								getHoldersMapProperty(model).getValue().put(model.getGeneric(), getConvertedValueProperty(model));
						});
					}
				}
			}
		}

		// Creation of links.
		public static class LinkBuilder extends LinkEditor implements GSBuilderDefaults {

			@Override
			public void init() {
				super.init();
				addPostfixBinding(model -> {
					if (getComponentsMapProperty(model) != null)
						getComponentsMapProperty(model).getValue().put(model.getGeneric(), getComponentsProperty(model).getValue());
				});
			}

			public static class ComponentBuilder extends GSComponentEditorDiv {

				@Override
				public void init() {
					forEach((ObservableListExtractor) gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1])));
				}

				public static class ComponentBuilderSelect extends ComponentAdderSelect {
				}
			}
		}
	}

	// “Add” button.
	public static class AddButtonDiv extends InstancesTable.ButtonDiv {
		@Style(name = "flex", value = "1")
		@Style(name = "height", value = "100%")
		@Style(name = "width", value = "100%")
		public static class AddButton extends HtmlButton {

			@Override
			public void init() {
				setText("Add");
				bindAttribute(ReactorStatics.DISABLED, ReactorStatics.DISABLED,
						model -> Bindings.createStringBinding(
								() -> Boolean.TRUE.equals(getInvalidListProperty(model).getValue().stream().map(input -> input.getValue()).filter(bool -> bool != null).reduce(false, (a, b) -> a || b)) ? ReactorStatics.DISABLED : "",
								getInvalidListProperty(model).getValue().stream().toArray(ObservableValue[]::new)));
				bindAction(model -> {
					ConvertedValueDefaults input = find(InstanceNameBuilderInput.class);
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
}
