package org.genericsystem.reactor.gs;

import java.util.Map;

import org.genericsystem.reactor.HtmlDomNode.SelectableHtmlDomNode;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gstag.HtmlOption;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import javafx.beans.binding.Bindings;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;

public class GSSelect extends GSTag implements SelectionDefaults {

	public HtmlOption optionElement;

	private GSSelect(GSTag parent) {
		super(parent, "select");
		options();
		init();
		createSelectionProperty();
		storeProperty(ReactorStatics.SELECTION_INDEX, model -> model.getSelectionIndex(this));
		bindBiDirectionalSelection(optionElement);
		storeProperty(ReactorStatics.SELECTION_STRING,
				model -> Bindings.createStringBinding(() -> StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(getSelectionProperty(model).getValue() != null ? getSelectionProperty(model).getValue().getGeneric() : null), getSelectionProperty(model)));
	}

	@Override
	protected SelectableHtmlDomNode createNode(String parentId) {
		return new SelectableHtmlDomNode(parentId);
	}

	protected void options() {
		optionElement = new HtmlOption(this) {
			{
				bindText();
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
					ObservableValue<String> observable = getObservableValue(ReactorStatics.SELECTION_STRING, model);
					observable.addListener(listener);
					map.put("background-color", observable.getValue());
				}
			});
			optionElement.addPrefixBinding(model -> {
				if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric().getMeta())))
					model.getObservableStyles(optionElement).put("background-color", optionElement.getGenericStringProperty(model).getValue());
			});
		}

		@Override
		protected void options() {
			new HtmlOption(this);
			super.options();
		}

		@Override
		protected void init() {
			createNewInitializedProperty(ReactorStatics.SELECTION_SHIFT, model -> 1);
		}
	}

	public static class ColorsSelect extends GSSelect {

		public ColorsSelect(GSTag parent) {
			super(parent);
			bindStyle("background-color", ReactorStatics.SELECTION_STRING);
			optionElement.bindStyle("background-color", ReactorStatics.BACKGROUND, model -> optionElement.getGenericStringProperty(model));
		}
	}

	public static class InstanceCompositeSelect extends GSSelect {

		public InstanceCompositeSelect(GSTag parent) {
			super(parent);
			addPostfixBinding(model -> getSelectionProperty(model).addListener((ov, ova, nva) -> model.getGenerics()[1].updateComponent(nva.getGeneric(), model.getGenerics()[1].getComponents().indexOf(model.getGeneric()))));
		}

		@Override
		public ObservableListExtractor getObservableListExtractor() {
			return ObservableListExtractor.SUBINSTANCES_OF_META;
		}
	}
}
