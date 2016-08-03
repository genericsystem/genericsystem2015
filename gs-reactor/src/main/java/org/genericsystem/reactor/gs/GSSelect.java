package org.genericsystem.reactor.gs;

import java.util.Map;

import org.genericsystem.reactor.TagProperty;
import org.genericsystem.reactor.TagProperty.SelectionIndexProperty;
import org.genericsystem.reactor.TagProperty.SelectionProperty;
import org.genericsystem.reactor.TagProperty.SelectionShiftProperty;
import org.genericsystem.reactor.TagProperty.SelectionStringProperty;
import org.genericsystem.reactor.TagProperty.TextProperty;
import org.genericsystem.reactor.gstag.GSOption;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

import javafx.beans.binding.Bindings;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;

public class GSSelect extends GSTag {

	public GSOption optionElement;
	protected final TagProperty<GenericModel> selectionProperty;
	protected final TagProperty<Number> selectionIndexProperty;
	protected final TagProperty<String> selectionStringProperty;
	protected TagProperty<Integer> selectionShiftProperty;

	private GSSelect(GSTag parent) {
		super(parent, "select");
		options();
		init();
		selectionProperty = createNewProperty(SelectionProperty::new);
		selectionIndexProperty = storeProperty(SelectionIndexProperty::new, model -> model.getSelectionIndex(this));
		bindBiDirectionalSelection(optionElement, selectionProperty, selectionIndexProperty, selectionShiftProperty);
		selectionStringProperty = storeProperty(SelectionStringProperty::new, model -> Bindings
				.createStringBinding(() -> getStringExtractor().apply(selectionProperty.getValue(model.getGeneric()) != null ? selectionProperty.getValue(model.getGeneric()).getGeneric() : null), selectionProperty.getProperty(model.getGeneric())));
	}

	@Override
	protected SelectableHtmlDomNode createNode(String parentId) {
		return new SelectableHtmlDomNode(parentId);
	}

	protected void options() {
		optionElement = new GSOption(this) {
			{
				bindText(GenericModel::getString);
				forEach(GSSelect.this);
			}
		};
	}

	protected void init() {

	}

	public static class CompositeSelectWithEmptyEntry extends GSSelect {

		public CompositeSelectWithEmptyEntry(GSTag parent) {
			super(parent);
			addPrefixBinding(model -> {
				if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric()))) {
					Map<String, String> map = model.getObservableStyles(this);
					ChangeListener<String> listener = (o, old, newValue) -> map.put("background-color", newValue);
					ObservableValue<String> observable = selectionStringProperty.getObservable(model.getGeneric());
					observable.addListener(listener);
					map.put("background-color", observable.getValue());
				}
			});
			optionElement.addPrefixBinding(model -> {
				if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric().getMeta())))
					model.getObservableStyles(optionElement).put("background-color", model.getString().getValue());
			});
		}

		@Override
		protected void options() {
			new GSOption(this);
			super.options();
		}

		@Override
		protected void init() {
			selectionShiftProperty = createNewProperty(SelectionShiftProperty::new);
			initProperty(selectionShiftProperty, 1);
		}
	}

	public static class ColorsSelect extends GSSelect {

		public ColorsSelect(GSTag parent) {
			super(parent);
			bindStyle("background-color", selectionStringProperty);
			optionElement.bindStyle("background-color", TextProperty::new, GenericModel::getString);
		}
	}

	public static class InstanceCompositeSelect extends GSSelect {

		public InstanceCompositeSelect(GSTag parent) {
			super(parent);
			addPostfixBinding(modelContext -> {
				int axe = pos(modelContext.getGenerics()[2], modelContext.getGenerics()[1]);
				selectionProperty.getProperty(modelContext.getGenerics()[1]).addListener((ov, ova, nva) -> modelContext.getGenerics()[2].updateComponent(nva.getGeneric(), axe));
			});
		}

		@Override
		public ObservableListExtractor getObservableListExtractor() {
			return ObservableListExtractor.SUBINSTANCES_OF_META;
		}
	}
}
