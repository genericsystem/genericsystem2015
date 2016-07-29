package org.genericsystem.reactor.gs;

import java.util.Map;

import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gstag.GSOption;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

import javafx.beans.binding.Bindings;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;

public class GSSelect extends GSTag {

	public GSOption optionElement;

	private GSSelect(GSTag parent) {
		super(parent, "select");
		options();
		init();
		createNewProperty(ReactorStatics.SELECTION);
		storeProperty(ReactorStatics.SELECTION_INDEX, model -> model.getSelectionIndex(this));
		bindBiDirectionalSelection(optionElement);
		storeProperty(ReactorStatics.SELECTION_STRING,
				model -> Bindings.createStringBinding(() -> getStringExtractor().apply(getProperty(ReactorStatics.SELECTION, model).getValue() != null ? ((GenericModel) getProperty(ReactorStatics.SELECTION, model).getValue()).getGeneric() : null),
						getProperty(ReactorStatics.SELECTION, model)));
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
					ObservableValue<String> observable = model.getObservableValue(this, ReactorStatics.SELECTION_STRING);
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
			createNewProperty(ReactorStatics.SELECTION_SHIFT);
			initProperty(ReactorStatics.SELECTION_SHIFT, 1);
		}
	}

	public static class ColorsSelect extends GSSelect {

		public ColorsSelect(GSTag parent) {
			super(parent);
			bindStyle("background-color", ReactorStatics.SELECTION_STRING);
			optionElement.bindStyle("background-color", ReactorStatics.TEXT, GenericModel::getString);
		}
	}

	public static class InstanceCompositeSelect extends GSSelect {

		public InstanceCompositeSelect(GSTag parent) {
			super(parent);
			addPostfixBinding(modelContext -> {
				int axe = pos(modelContext.getGenerics()[2], modelContext.getGenerics()[1]);
				getProperty(ReactorStatics.SELECTION, modelContext).addListener((ov, ova, nva) -> modelContext.getGenerics()[2].updateComponent(((GenericModel) nva).getGeneric(), axe));
			});
		}

		@Override
		public ObservableListExtractor getObservableListExtractor() {
			return ObservableListExtractor.SUBINSTANCES_OF_META;
		}
	}
}
