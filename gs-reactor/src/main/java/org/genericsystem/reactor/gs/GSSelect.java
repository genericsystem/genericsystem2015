package org.genericsystem.reactor.gs;

import java.util.Map;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.TagImpl;
import org.genericsystem.reactor.gstag.HtmlOption;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;
import org.genericsystem.reactor.modelproperties.ComponentsDefaults;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import io.vertx.core.json.JsonObject;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;

public class GSSelect extends TagImpl implements SelectionDefaults, ComponentsDefaults {

	public HtmlOption optionElement;

	private GSSelect(Tag parent) {
		super(parent, "select");
		options();
		initSelect();
		createSelectionProperty();
		bindBiDirectionalSelection(optionElement);
		addPrefixBinding(model -> {
			if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric()))) {
				Map<String, String> map = getDomNodeStyles(model);
				ChangeListener<String> listener = (o, old, newValue) -> map.put("background-color", newValue);
				ObservableValue<String> observable = getSelectionString(model);
				observable.addListener(listener);
				map.put("background-color", observable.getValue());
			}
		});
		optionElement.addPrefixBinding(model -> {
			if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric().getMeta())))
				optionElement.addStyle(model, "background-color", optionElement.getGenericStringProperty(model).getValue());
		});
	}

	@Override
	public HtmlDomNode createNode(HtmlDomNode parent, Context modelContext) {
		return new HtmlDomNode(parent, modelContext, this) {

			@Override
			public void handleMessage(JsonObject json) {
				if (UPDATE.equals(json.getString(MSG_TYPE))) {
					((SelectionDefaults) getTag()).getSelectionIndex(getModelContext()).setValue(json.getInteger(SELECTED_INDEX));
				}
			}
		};
	}

	protected void options() {
		optionElement = new HtmlOption(this) {
			{
				bindText();
				forEach(GSSelect.this);
			}
		};
	}

	protected void initSelect() {

	}

	public static class CompositeSelectWithEmptyEntry extends GSSelect {

		public CompositeSelectWithEmptyEntry(Tag parent) {
			super(parent);
		}

		@Override
		protected void options() {
			new HtmlOption(this);
			super.options();
		}

		@Override
		protected void initSelect() {
			setSelectionShift(1);
		}
	}

	public static class InstanceCompositeSelect extends GSSelect {

		public InstanceCompositeSelect(Tag parent) {
			super(parent);
			addPostfixBinding(model -> {
				if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric().getMeta())))
					addStyle(model, "background-color", getSelectionString(model).getValue());
			});
			addPostfixBinding(model -> getSelectionProperty(model).addListener((ov, ova, nva) -> model.getGenerics()[1].updateComponent(nva.getGeneric(), model.getGenerics()[1].getComponents().indexOf(model.getGeneric()))));
		}

		@Override
		public ObservableListExtractor getObservableListExtractor() {
			return ObservableListExtractor.SUBINSTANCES_OF_META;
		}
	}
}
