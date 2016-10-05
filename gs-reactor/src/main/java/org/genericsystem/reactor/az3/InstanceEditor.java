package org.genericsystem.reactor.az3;

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
import org.genericsystem.reactor.annotations.Styles.Height;
import org.genericsystem.reactor.annotations.Styles.JustifyContent;
import org.genericsystem.reactor.annotations.Styles.Style;
import org.genericsystem.reactor.annotations.Styles.Width;
import org.genericsystem.reactor.az.GSCheckBoxWithValue;
import org.genericsystem.reactor.az.GSCheckBoxWithValue.GSCheckBoxEditor;
import org.genericsystem.reactor.az.GSInputTextWithConversion;
import org.genericsystem.reactor.az.GSInputTextWithConversion.GSInputTextEditorWithConversion;
import org.genericsystem.reactor.az.GSSelect.CompositeSelectWithEmptyEntry;
import org.genericsystem.reactor.az.GSSelect.InstanceCompositeSelect;
import org.genericsystem.reactor.az3.GSComposite.Content;
import org.genericsystem.reactor.az3.GSComposite.Header;
import org.genericsystem.reactor.az3.InstanceEditor.GSHoldersEditor;
import org.genericsystem.reactor.az3.InstanceEditor.GSValueComponentsEditor;
import org.genericsystem.reactor.az3.InstanceEditor.RemovalLink;
import org.genericsystem.reactor.az3.InstancesTable.GSHolders;
import org.genericsystem.reactor.az3.InstancesTable.GSValueComponents;
import org.genericsystem.reactor.az3.Table.ContentRow;
import org.genericsystem.reactor.az3.Table.HeaderRow;
import org.genericsystem.reactor.gstag.HtmlHyperLink;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.ObservableListExtractor.NO_FOR_EACH;
import org.genericsystem.reactor.model.ObservableModelSelector.REMOVABLE_HOLDER_SELECTOR;
import org.genericsystem.reactor.model.ObservableValueSelector;
import org.genericsystem.reactor.model.ObservableValueSelector.DIRECT_RELATION_SELECTOR;
import org.genericsystem.reactor.model.ObservableValueSelector.REVERSED_RELATION_SELECTOR;
import org.genericsystem.reactor.modelproperties.ComponentsDefaults;
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
@ReactorDependencies(path = { ContentRow.class, GSHoldersEditor.class, GSValueComponentsEditor.class }, value = { Header.class, Content.class, RemovalLink.class })
@ForEach(path = { HeaderRow.class, Content.class }, value = ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
@ForEach(path = { HeaderRow.class, Content.class, GSValueComponents.class, Content.class }, value = ObservableListExtractor.OTHER_COMPONENTS_2.class)
@ForEach(path = { ContentRow.class, GSHolders.class }, value = ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
@ForEach(path = { ContentRow.class, GSValueComponents.class, Content.class }, value = ObservableListExtractor.OTHER_COMPONENTS_2.class)
@SelectModel(path = { ContentRow.class, GSHoldersEditor.class, GSValueComponentsEditor.class, RemovalLink.class }, value = REMOVABLE_HOLDER_SELECTOR.class)
public class InstanceEditor extends Table implements SelectionDefaults {

	@Flex(path = { Header.class, GSInputTextEditorWithConversion.class }, value = "1")
	@Width(path = { Header.class, GSInputTextEditorWithConversion.class }, value = "100%")
	@ReactorDependencies(path = Header.class, value = GSInputTextEditorWithConversion.class)
	@ReactorDependencies(path = Content.class, value = { DirectRelationComponentEditor.class, GSLabelDisplayer.class })
	@Select(path = { Content.class, DirectRelationComponentEditor.class }, value = DIRECT_RELATION_SELECTOR.class)
	@Select(path = { Content.class, GSLabelDisplayer.class }, value = REVERSED_RELATION_SELECTOR.class)
	public static class GSValueComponentsEditor extends GSValueComponents implements ComponentsDefaults {
	}

	@ReactorDependencies(value = { GSValueComponentsEditor.class, GSHolderAdder.class })
	@ReactorDependencies(path = { GSValueComponentsEditor.class, Header.class }, value = { GSInputTextEditorWithConversion.class, GSCheckBoxEditor.class })
	@ReactorDependencies(path = { GSHolderAdder.class, Header.class }, value = { HolderAdderInput.class, BooleanHolderAdderInput.class })
	@ForEach(path = GSHolderAdder.class, value = NO_FOR_EACH.class)
	@Select(path = { GSValueComponents.class, Header.class, GSInputTextEditorWithConversion.class }, value = ObservableValueSelector.LABEL_DISPLAYER.class)
	@Select(path = { GSValueComponents.class, Header.class, GSCheckBoxEditor.class }, value = ObservableValueSelector.CHECK_BOX_DISPLAYER.class)
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

	@JustifyContent("center")
	@Height("100%")
	@Style(name = "text-decoration", value = "none")
	public static class RemovalLink extends HtmlHyperLink {

		@Override
		public void init() {
			setText("×");
			bindAction(Context::remove);
		}
	}

	@Flex(path = { Header.class, GSInputTextWithConversion.class }, value = "1")
	@Width(path = { Header.class, GSInputTextWithConversion.class }, value = "100%")
	@ReactorDependencies(path = Header.class, value = { HolderAdderInput.class, BooleanHolderAdderInput.class })
	@ReactorDependencies(path = Content.class, value = ComponentAdderSelect.class)
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
			addConvertedValueChangeListener((model, nva) -> {
				if (nva != null)
					model.getGenerics()[1].addHolder(model.getGeneric(), nva);
			});
		}
	}

	public static class BooleanHolderAdderInput extends GSCheckBoxWithValue {
		@Override
		public void init() {
			addConvertedValueChangeListener((model, nva) -> {
				if (nva != null)
					model.getGenerics()[1].addHolder(model.getGeneric(), nva);
			});
		}
	}

	@Flex("1")
	@Width("100%")
	public static class ComponentAdderSelect extends CompositeSelectWithEmptyEntry {

		@Override
		public void init() {
			// select(gs -> gs[1].isReferentialIntegrityEnabled(gs[1].getComponents().indexOf(gs[0])) ? gs[0] : null);
			addPostfixBinding(model -> {
				Property<List<Property<Context>>> selectedComponents = getComponentsProperty(model);
				if (selectedComponents != null)
					selectedComponents.getValue().add(getSelectionProperty(model));
			});
		}
	}
}