package org.genericsystem.reactor.az3;

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
import org.genericsystem.reactor.annotations.Styles;
import org.genericsystem.reactor.annotations.Styles.BackgroundColor;
import org.genericsystem.reactor.annotations.Styles.Flex;
import org.genericsystem.reactor.annotations.Styles.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Styles.FlexWrap;
import org.genericsystem.reactor.annotations.Styles.Overflow;
import org.genericsystem.reactor.annotations.Styles.Width;
import org.genericsystem.reactor.az.FlexDirection;
import org.genericsystem.reactor.az.GSCheckBoxWithValue;
import org.genericsystem.reactor.az.GSCheckBoxWithValue.GSCheckBoxEditor;
import org.genericsystem.reactor.az.GSInputTextWithConversion;
import org.genericsystem.reactor.az.GSInputTextWithConversion.GSInputTextEditorWithConversion;
import org.genericsystem.reactor.az.GSSelect.CompositeSelectWithEmptyEntry;
import org.genericsystem.reactor.az.GSSelect.InstanceCompositeSelect;
import org.genericsystem.reactor.az3.GSCellDiv.GSActionLink;
import org.genericsystem.reactor.az3.GSComposite.Content;
import org.genericsystem.reactor.az3.GSComposite.Header;
import org.genericsystem.reactor.az3.InstanceEditor.GSHoldersEditor;
import org.genericsystem.reactor.az3.InstanceEditor.GSValueComponentsEditor;
import org.genericsystem.reactor.az3.InstancesTable.GSHolders;
import org.genericsystem.reactor.az3.InstancesTable.GSValueComponents;
import org.genericsystem.reactor.az3.Table.ContentRow;
import org.genericsystem.reactor.az3.Table.HeaderRow;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.ObservableListExtractor.NO_FOR_EACH;
import org.genericsystem.reactor.model.ObservableModelSelector.HOLDER_ADDITION_ENABLED_SELECTOR;
import org.genericsystem.reactor.model.ObservableModelSelector.REMOVABLE_HOLDER_SELECTOR;
import org.genericsystem.reactor.model.ObservableValueSelector;
import org.genericsystem.reactor.model.ObservableValueSelector.DIRECT_RELATION_SELECTOR;
import org.genericsystem.reactor.model.ObservableValueSelector.REVERSED_RELATION_SELECTOR;
import org.genericsystem.reactor.model.ObservableValueSelector.STRICT_ATTRIBUTE_SELECTOR;
import org.genericsystem.reactor.modelproperties.ComponentsDefaults;
import org.genericsystem.reactor.modelproperties.ConvertedValueDefaults;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import javafx.beans.property.Property;
import javafx.beans.value.ChangeListener;

@Flex(path = HeaderRow.class, value = "0.3")
@Styles.Color(path = HeaderRow.class, value = "white")
@BackgroundColor(path = { HeaderRow.class, GSValueComponents.class, Content.class }, value = "#ea0084")
@BackgroundColor(path = { HeaderRow.class, GSValueComponents.class, Header.class }, value = "#ea0084")
@BackgroundColor(path = { HeaderRow.class, Content.class, GSValueComponents.class, Content.class }, value = "#ea0084")
@BackgroundColor(path = { HeaderRow.class, Content.class, GSValueComponents.class, Header.class }, value = "#ea0084")
@ReactorDependencies({ HeaderRow.class, ContentRow.class })
@ReactorDependencies(path = HeaderRow.class, value = { GSValueComponents.class, Content.class })
@ReactorDependencies(path = { HeaderRow.class, Content.class }, value = GSValueComponents.class)
@ReactorDependencies(path = ContentRow.class, value = { GSValueComponentsEditor.class, GSHoldersEditor.class })
@ReactorDependencies(path = { ContentRow.class, GSValueComponentsEditor.class }, value = { Header.class, Content.class })
@ForEach(path = { HeaderRow.class, Content.class }, value = ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
@ForEach(path = { HeaderRow.class, Content.class, GSValueComponents.class, Content.class }, value = ObservableListExtractor.OTHER_COMPONENTS_2.class)
@ForEach(path = { ContentRow.class, GSHolders.class }, value = ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
@ForEach(path = { ContentRow.class, GSValueComponents.class, Content.class }, value = ObservableListExtractor.OTHER_COMPONENTS_2.class)
public class InstanceEditor extends Table implements SelectionDefaults {

	@Flex(path = { Header.class, GSInputTextEditorWithConversion.class }, value = "1")
	@Width(path = { Header.class, GSInputTextEditorWithConversion.class }, value = "100%")
	@ReactorDependencies({ Header.class, Content.class, RemovalLink.class })
	@ReactorDependencies(path = Header.class, value = GSInputTextEditorWithConversion.class)
	@ReactorDependencies(path = Content.class, value = { DirectRelationComponentEditor.class, GSLabelDisplayer.class })
	@Select(path = { Content.class, DirectRelationComponentEditor.class }, value = DIRECT_RELATION_SELECTOR.class)
	@Select(path = { Content.class, GSLabelDisplayer.class }, value = REVERSED_RELATION_SELECTOR.class)
	@SelectModel(path = RemovalLink.class, value = REMOVABLE_HOLDER_SELECTOR.class)
	public static class GSValueComponentsEditor extends GSValueComponents implements ComponentsDefaults {
	}

	@ReactorDependencies(value = { GSValueComponentsEditor.class, GSHolderAdder.class })
	@ReactorDependencies(path = { GSValueComponentsEditor.class, Header.class }, value = { GSInputTextEditorWithConversion.class, GSCheckBoxEditor.class })
	@ReactorDependencies(path = { GSHolderAdder.class, Header.class }, value = { HolderAdderInput.class, BooleanHolderAdderInput.class })
	@ForEach(path = GSHolderAdder.class, value = NO_FOR_EACH.class)
	@SelectModel(path = GSHolderAdder.class, value = HOLDER_ADDITION_ENABLED_SELECTOR.class)
	@Select(path = { GSValueComponents.class, Header.class, GSInputTextEditorWithConversion.class }, value = ObservableValueSelector.LABEL_DISPLAYER.class)
	@Select(path = { GSValueComponents.class, Header.class, GSCheckBoxEditor.class }, value = ObservableValueSelector.CHECK_BOX_DISPLAYER.class)
	@FlexDirectionStyle(FlexDirection.COLUMN)
	@FlexWrap("wrap")
	@Overflow("auto")
	public static class GSHoldersEditor extends GSHolders {

	}

	@Flex("1")
	@Width("100%")
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

	@Flex(path = { Header.class, GSInputTextWithConversion.class }, value = "1")
	@Width(path = { Header.class, GSInputTextWithConversion.class }, value = "100%")
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

	@Flex("1")
	@Width("100%")
	public static class ComponentAdderSelect extends CompositeSelectWithEmptyEntry {

		@Override
		public void init() {
			//			select(gs -> gs[1].isReferentialIntegrityEnabled(gs[1].getComponents().indexOf(gs[0])) ? gs[0] : null);
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
			System.out.println("addHolder, classe du tag : " + tag.getClass().getSimpleName());
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