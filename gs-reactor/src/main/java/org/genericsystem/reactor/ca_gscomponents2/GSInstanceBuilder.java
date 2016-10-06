package org.genericsystem.reactor.ca_gscomponents2;

import java.io.Serializable;
import java.util.List;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.aa_modelproperties.ConvertedValueDefaults;
import org.genericsystem.reactor.aa_modelproperties.GSBuilderDefaults;
import org.genericsystem.reactor.annotations.Parent;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Styles.Flex;
import org.genericsystem.reactor.annotations.Styles.Height;
import org.genericsystem.reactor.annotations.Styles.ReverseFlexDirection;
import org.genericsystem.reactor.annotations.Styles.Width;
import org.genericsystem.reactor.ba_htmltag.HtmlButton;
import org.genericsystem.reactor.ca_gscomponents.GSCheckBoxWithValue;
import org.genericsystem.reactor.ca_gscomponents.GSDiv;
import org.genericsystem.reactor.ca_gscomponents.GSInputTextWithConversion;
import org.genericsystem.reactor.ca_gscomponents2.GSCellDiv.CenteredFlexDiv;
import org.genericsystem.reactor.ca_gscomponents2.GSCellDiv.GSComponentEditorDiv;
import org.genericsystem.reactor.ca_gscomponents2.GSCellDiv.GSSubcellEditorDiv;
import org.genericsystem.reactor.ca_gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellAdder.LinkAdder.ComponentAdder.ComponentAdderSelect;
import org.genericsystem.reactor.ca_gscomponents2.GSEditor.EditorContent.InstanceEdition.InstanceAttributeEditor.AttributeEditionColumn.SubcellEditor.LinkEditor;
import org.genericsystem.reactor.ca_gscomponents2.GSInstanceBuilder.AddButtonDiv.AddButton;
import org.genericsystem.reactor.ca_gscomponents2.GSInstanceBuilder.BuilderCell.BooleanHolderBuilder.CheckboxContainerBuildDiv.BooleanHolderBuilderInput;
import org.genericsystem.reactor.ca_gscomponents2.GSInstanceBuilder.BuilderCell.HolderBuilder.HolderBuilderInput;
import org.genericsystem.reactor.ca_gscomponents2.GSInstanceBuilder.BuilderCell.LinkBuilder.ComponentBuilder.ComponentBuilderSelect;
import org.genericsystem.reactor.ca_gscomponents2.GSInstanceBuilder.InstanceNameBuilder.InstanceNameBuilderInput;
import org.genericsystem.reactor.model.ObservableListExtractor;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

@Parent(GSTable.class)
@ReactorDependencies({ InstanceNameBuilderInput.class, HolderBuilderInput.class, BooleanHolderBuilderInput.class, ComponentBuilderSelect.class, AddButton.class })
@Flex("1")
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
	@Parent(GSInstanceBuilder.class)
	public static class InstanceNameBuilder extends GSSubcellEditorDiv {
		@Parent(InstanceNameBuilder.class)
		@Flex("1")
		@Height("100%")
		@Width("100%")
		public static class InstanceNameBuilderInput extends GSInputTextWithConversion {
		}
	}

	// Creation of holders/links.
	@Parent(GSInstanceBuilder.class)
	public static class BuilderCell extends GSSubcellEditorDiv {

		@Override
		public void init() {
			forEach(ObservableListExtractor.ATTRIBUTES_OF_TYPE);
		}

		// Creation of non-boolean holders.
		@Parent(BuilderCell.class)
		public static class HolderBuilder extends GSSubcellEditorDiv {

			@Override
			public void init() {
				select(gs -> gs[0].getComponents().size() < 2 && !Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
			}

			@Parent(HolderBuilder.class)
			@Flex("1")
			@Height("100%")
			@Width("100%")
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
		@Parent(BuilderCell.class)
		public static class BooleanHolderBuilder extends GSSubcellEditorDiv {

			@Override
			public void init() {
				select(gs -> gs[0].getComponents().size() < 2 && Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
			}

			@Parent(BooleanHolderBuilder.class)
			public static class CheckboxContainerBuildDiv extends CenteredFlexDiv {
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

			@Parent(LinkBuilder.class)
			public static class ComponentBuilder extends GSComponentEditorDiv {

				@Override
				public void init() {
					forEach((ObservableListExtractor) gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1])));
				}

				@Parent(ComponentBuilder.class)
				public static class ComponentBuilderSelect extends ComponentAdderSelect {
				}
			}
		}
	}

	// “Add” button.
	@Parent(GSInstanceBuilder.class)
	public static class AddButtonDiv extends InstancesTable.ButtonDiv {
		@Parent(AddButtonDiv.class)
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
