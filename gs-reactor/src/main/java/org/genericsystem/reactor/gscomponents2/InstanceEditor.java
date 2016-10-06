package org.genericsystem.reactor.gscomponents2;

import java.io.Serializable;
import java.util.List;
import java.util.stream.Collectors;

import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.Select.SelectModel;
import org.genericsystem.reactor.annotations.Styles.AlignItems;
import org.genericsystem.reactor.annotations.Styles.Flex;
import org.genericsystem.reactor.annotations.Styles.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Styles.FlexWrap;
import org.genericsystem.reactor.annotations.Styles.GenericValueBackgroundColor;
import org.genericsystem.reactor.annotations.Styles.JustifyContent;
import org.genericsystem.reactor.annotations.Styles.Overflow;
import org.genericsystem.reactor.annotations.Styles.Style;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.GSCheckBoxWithValue;
import org.genericsystem.reactor.gscomponents.GSDiv;
import org.genericsystem.reactor.gscomponents.GSInputTextWithConversion;
import org.genericsystem.reactor.gscomponents.GSCheckBoxWithValue.GSCheckBoxEditor;
import org.genericsystem.reactor.gscomponents.GSInputTextWithConversion.GSInputTextEditorWithConversion;
import org.genericsystem.reactor.gscomponents.GSSelect.CompositeSelectWithEmptyEntry;
import org.genericsystem.reactor.gscomponents.GSSelect.InstanceCompositeSelect;
import org.genericsystem.reactor.gscomponents2.GSCellDiv.GSActionLink;
import org.genericsystem.reactor.gscomponents2.GSComposite.Content;
import org.genericsystem.reactor.gscomponents2.GSComposite.Header;
import org.genericsystem.reactor.gscomponents2.InstanceEditor.GSHoldersEditor;
import org.genericsystem.reactor.gscomponents2.InstanceEditor.GSMultiCheckbox;
import org.genericsystem.reactor.gscomponents2.InstanceEditor.GSValueComponentsEditor;
import org.genericsystem.reactor.gscomponents2.InstancesTable.GSHolders;
import org.genericsystem.reactor.gscomponents2.InstancesTable.GSValueComponents;
import org.genericsystem.reactor.gscomponents2.Table.ContentRow;
import org.genericsystem.reactor.gscomponents2.Table.HeaderRow;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.ObservableListExtractor.NO_FOR_EACH;
import org.genericsystem.reactor.model.ObservableListExtractor.SUBINSTANCES_OF_COMPONENT;
import org.genericsystem.reactor.model.ObservableModelSelector.HOLDER_ADDITION_ENABLED_SELECTOR;
import org.genericsystem.reactor.model.ObservableModelSelector.REMOVABLE_HOLDER_SELECTOR;
import org.genericsystem.reactor.model.ObservableValueSelector;
import org.genericsystem.reactor.model.ObservableValueSelector.DIRECT_RELATION_SELECTOR;
import org.genericsystem.reactor.model.ObservableValueSelector.MULTICHECKBOX_SELECTOR;
import org.genericsystem.reactor.model.ObservableValueSelector.NON_MULTICHECKBOX_SELECTOR;
import org.genericsystem.reactor.model.ObservableValueSelector.REVERSED_RELATION_SELECTOR;
import org.genericsystem.reactor.model.ObservableValueSelector.STRICT_ATTRIBUTE_SELECTOR;

import org.genericsystem.reactor.modelproperties.ComponentsDefaults;
import org.genericsystem.reactor.modelproperties.ConvertedValueDefaults;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import org.genericsystem.reactor.htmltag.HtmlLabel.GSLabelDisplayer;

import org.genericsystem.reactor.model.StringExtractor;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;

@Style(path = HeaderRow.class, name = "flex", value = "0.3")
@Style(path = ContentRow.class, name = "flex", value = "1")
@Style(path = HeaderRow.class, name = "color", value = "white")
@GenericValueBackgroundColor(path = { HeaderRow.class, GSValueComponents.class, Content.class }, value = "#ea0084")
@GenericValueBackgroundColor(path = { HeaderRow.class, GSValueComponents.class, Header.class }, value = "#ea0084")
@GenericValueBackgroundColor(path = { HeaderRow.class, Content.class, GSValueComponents.class, Content.class }, value = "#ea0084")
@GenericValueBackgroundColor(path = { HeaderRow.class, Content.class, GSValueComponents.class, Header.class }, value = "#ea0084")
@ReactorDependencies({ HeaderRow.class, ContentRow.class })
@ReactorDependencies(path = HeaderRow.class, value = { GSValueComponents.class, Content.class })
@ReactorDependencies(path = { HeaderRow.class, Content.class }, value = GSValueComponents.class)
@ReactorDependencies(path = ContentRow.class, value = { GSValueComponentsEditor.class, Content.class })
@ReactorDependencies(path = { ContentRow.class, Content.class }, value = { GSHoldersEditor.class, GSMultiCheckbox.class })
@ReactorDependencies(path = { ContentRow.class, GSValueComponentsEditor.class }, value = { Header.class, Content.class })
@ForEach(path = { HeaderRow.class, Content.class }, value = ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
@ForEach(path = { HeaderRow.class, Content.class, GSValueComponents.class, Content.class }, value = ObservableListExtractor.OTHER_COMPONENTS_2.class)
@ForEach(path = { ContentRow.class, Content.class }, value = ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
@ForEach(path = { ContentRow.class, GSValueComponents.class, Content.class }, value = ObservableListExtractor.OTHER_COMPONENTS_2.class)
@Select(path = { ContentRow.class, Content.class, GSHoldersEditor.class }, value = NON_MULTICHECKBOX_SELECTOR.class)
@Select(path = { ContentRow.class, Content.class, GSMultiCheckbox.class }, value = MULTICHECKBOX_SELECTOR.class)
public class InstanceEditor extends Table implements SelectionDefaults {
	@ReactorDependencies(CheckboxLabel.class)
	@ForEach(path = CheckboxLabel.class, value = SUBINSTANCES_OF_COMPONENT.class)
	@FlexWrap("wrap")
	@Overflow("auto")
	public static class GSMultiCheckbox extends GSDiv {

	}

	@Flex("1 0 auto")
	@JustifyContent("center")
	@AlignItems("center")
	@Style(name = "text-align", value = "center")
	@ReactorDependencies(Checkbox.class)
	public static class CheckboxLabel extends org.genericsystem.reactor.htmltag.HtmlLabel {

		@Override
		public void init() {
			bindText();
			addPrefixBinding(model -> {
				if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric().getMeta())))
					addStyle(model, "background-color", getGenericStringProperty(model).getValue());
			});
		}
	}

	@Style(name = "float", value = "left")
	@Style(name = "vertical-align", value = "middle")
	@Style(name = "margin", value = "4px")
	public static class Checkbox extends GSCheckBoxWithValue {

		@Override
		public void init() {
			initValueProperty(context -> context.getGenerics()[2].getLink(context.getGenerics()[1], context.getGeneric()) != null ? true : false);
			storeProperty("exists", context -> {
				ObservableValue<Boolean> exists = Bindings.createBooleanBinding(() -> context.getGenerics()[2].getObservableLink(context.getGenerics()[1], context.getGeneric()).getValue() != null ? true : false,
						context.getGenerics()[2].getObservableLink(context.getGenerics()[1], context.getGeneric()));
				exists.addListener((o, v, nva) -> {
					if (!context.isDestroyed())
						getConvertedValueProperty(context).setValue(nva);
				});
				return exists;
			});
			addConvertedValueChangeListener((context, nva) -> {
				if (Boolean.TRUE.equals(nva))
					context.getGenerics()[2].setHolder(context.getGenerics()[1], null, context.getGeneric());
				if (Boolean.FALSE.equals(nva)) {
					Generic link = context.getGenerics()[2].getLink(context.getGenerics()[1], context.getGeneric());
					if (link != null)
						link.remove();
				}
			});
		}
	}

	@Style(path = { Header.class, GSInputTextEditorWithConversion.class }, name = "flex", value = "1")
	@Style(path = { Header.class, GSInputTextEditorWithConversion.class }, name = "width", value = "100%")
	@ReactorDependencies({ Header.class, Content.class, RemovalLink.class })
	@ReactorDependencies(path = Header.class, value = GSInputTextEditorWithConversion.class)
	@ReactorDependencies(path = Content.class, value = { DirectRelationComponentEditor.class, GSLabelDisplayer.class })
	@Select(path = { Content.class, DirectRelationComponentEditor.class }, value = DIRECT_RELATION_SELECTOR.class)
	@Select(path = { Content.class, GSLabelDisplayer.class }, value = REVERSED_RELATION_SELECTOR.class)
	@SelectModel(path = RemovalLink.class, value = REMOVABLE_HOLDER_SELECTOR.class)
	public static class GSValueComponentsEditor extends GSValueComponents implements ComponentsDefaults {
	}

	@Flex(path = GSValueComponentsEditor.class, value = "1 0 auto")
	@ReactorDependencies(value = { GSValueComponentsEditor.class, GSHolderAdder.class })
	@ReactorDependencies(path = { GSValueComponentsEditor.class, Header.class }, value = { GSInputTextEditorWithConversion.class, GSCheckBoxEditor.class })
	@ReactorDependencies(path = { GSHolderAdder.class, Header.class }, value = { HolderAdderInput.class, BooleanHolderAdderInput.class })
	@ForEach(path = GSHolderAdder.class, value = NO_FOR_EACH.class)
	@SelectModel(path = GSHolderAdder.class, value = HOLDER_ADDITION_ENABLED_SELECTOR.class)
	@Select(path = { GSValueComponents.class, Header.class, GSInputTextEditorWithConversion.class }, value = ObservableValueSelector.LABEL_DISPLAYER.class)
	@Select(path = { GSValueComponents.class, Header.class, GSCheckBoxEditor.class }, value = ObservableValueSelector.CHECK_BOX_DISPLAYER.class)
	@FlexDirectionStyle(FlexDirection.COLUMN)
	@Style(name = "flex-wrap", value = "wrap")
	@Style(name = "overflow", value = "auto")
	public static class GSHoldersEditor extends GSHolders {

	}

	@Style(name = "flex", value = "1")
	@Style(name = "width", value = "100%")
	public static class DirectRelationComponentEditor extends InstanceCompositeSelect {
		@Override
		public void init() {
			addPostfixBinding(model -> {
				Property<List<Property<Context>>> selectedComponents = getComponentsProperty(model);
				if (selectedComponents != null)
					selectedComponents.getValue().add(getSelectionProperty(model));
			});
		}
	}

	public static class RemovalLink extends GSActionLink {

		@Override
		public void init() {
			setText("×");
			bindAction(Context::remove);
		}
	}

	@Style(name = "flex", value = "1 0 auto")
	@Style(path = { Header.class, GSInputTextWithConversion.class }, name = "flex", value = "1")
	@Style(path = { Header.class, GSInputTextWithConversion.class }, name = "width", value = "100%")
	@ReactorDependencies({ Header.class, Content.class, AdditionLink.class })
	@ReactorDependencies(path = Header.class, value = { HolderAdderInput.class, BooleanHolderAdderInput.class })
	@ReactorDependencies(path = Content.class, value = ComponentAdderSelect.class)
	@Select(path = AdditionLink.class, value = STRICT_ATTRIBUTE_SELECTOR.class)
	@Select(path = { Header.class, HolderAdderInput.class }, value = ObservableValueSelector.LABEL_DISPLAYER_0.class)
	@Select(path = { Header.class, BooleanHolderAdderInput.class }, value = ObservableValueSelector.CHECK_BOX_DISPLAYER_0.class)
	public static class GSHolderAdder extends GSValueComponents implements ComponentsDefaults {
		@Override
		public void init() {
			createComponentsListProperty();
			addPostfixBinding(model -> {
				Property<List<Property<Context>>> selectedComponents = getComponentsProperty(model);
				ChangeListener<Context> listener = (o, v, nva) -> {
					List<Generic> selectedGenerics = selectedComponents.getValue().stream().filter(obs -> obs.getValue() != null).map(obs -> obs.getValue().getGeneric()).filter(gen -> gen != null).collect(Collectors.toList());
					if (selectedGenerics.size() + 1 == model.getGeneric().getComponents().size()) {
						selectedComponents.getValue().stream().forEach(sel -> sel.setValue(null));
						try {
							model.getGenerics()[1].setHolder(model.getGeneric(), null, selectedGenerics.stream().toArray(Generic[]::new));
						} catch (RollbackException e) {
							e.printStackTrace();
						}
					}
				};
				selectedComponents.getValue().forEach(component -> component.addListener(listener));
			});
		}
	}

	public static class HolderAdderInput extends GSInputTextWithConversion {
		@Override
		public void init() {
			addConvertedValueChangeListener((context, nva) -> {
				if (nva != null)
					context.getGenerics()[1].addHolder(context.getGeneric(), nva);
			});
		}
	}

	public static class BooleanHolderAdderInput extends GSCheckBoxWithValue {
		@Override
		public void init() {
			addConvertedValueChangeListener((context, nva) -> {
				if (nva != null)
					context.getGenerics()[1].addHolder(context.getGeneric(), nva);
			});
		}
	}

	public static void addHolder(Context context, ConvertedValueDefaults tag) {
		Property<Serializable> observable = tag.getConvertedValueProperty(context);
		if (observable.getValue() != null) {
			Serializable newValue = observable.getValue();
			observable.setValue(null);
			context.getGenerics()[1].addHolder(context.getGeneric(), newValue);
		}
	}

	@Style(name = "flex", value = "1")
	@Style(name = "width", value = "100%")
	public static class ComponentAdderSelect extends CompositeSelectWithEmptyEntry {

		@Override
		public void init() {
			addPostfixBinding(model -> {
				Property<List<Property<Context>>> selectedComponents = getComponentsProperty(model);
				if (selectedComponents != null)
					selectedComponents.getValue().add(getSelectionProperty(model));
			});
		}
	}

	public static class AdditionLink extends GSActionLink {

		@Override
		public void init() {
			setText("+");
			bindAction(context -> addHolder(context.getParent(), (ConvertedValueDefaults) this.getParent().find(Header.class).getObservableChildren().stream().filter(t -> t instanceof ConvertedValueDefaults).findFirst().get()));
		}

		private void addHolder(Context context, ConvertedValueDefaults tag) {
			Property<Serializable> observable = tag.getConvertedValueProperty(context);
			assert observable != null;
			if (observable.getValue() != null) {
				Serializable newValue = observable.getValue();
				observable.setValue(null);
				context.getGenerics()[1].addHolder(context.getGeneric(), newValue);
			}
		}
	}
}