package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.modelproperties.ComponentsDefaults;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import org.genericsystem.reactor.htmltag.HtmlOption;
import org.genericsystem.reactor.htmltag.HtmlSelect;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

import javafx.beans.value.ObservableValue;

public class GSSelect extends HtmlSelect implements SelectionDefaults, ComponentsDefaults {

	public HtmlOption optionElement;

	public GSSelect() {
		initSelect();
	}

	private GSSelect(Tag parent) {
		super(parent);
		initSelect();
	}

	protected void initSelect() {
		options();
		initProperties();
		createSelectionProperty();
		bindBiDirectionalSelection(optionElement);
		addPrefixBinding(model -> {
			if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric()))) {
				ObservableValue<String> observable = getSelectionString(model);
				observable.addListener((o, old, newValue) -> addStyle(model, "background-color", newValue));
				addStyle(model, "background-color", observable.getValue());
			}
		});
		optionElement.addPrefixBinding(model -> {
			if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric().getMeta())))
				optionElement.addStyle(model, "background-color", optionElement.getGenericStringProperty(model).getValue());
		});
	}

	protected void options() {
		optionElement = new HtmlOption(this) {
			{
				bindText();
				forEach(GSSelect.this);
			}
		};
	}

	protected void initProperties() {

	}

	public static class CompositeSelectWithEmptyEntry extends GSSelect {

		public CompositeSelectWithEmptyEntry() {
		}

		public CompositeSelectWithEmptyEntry(Tag parent) {
			super(parent);
		}

		@Override
		protected void options() {
			new HtmlOption(this);
			super.options();
		}

		@Override
		protected void initProperties() {
			setSelectionShift(1);
		}
	}

	public static class InstanceCompositeSelect extends GSSelect {

		public InstanceCompositeSelect() {
		}

		public InstanceCompositeSelect(Tag parent) {
			super(parent);
		}

		@Override
		protected void initSelect() {
			super.initSelect();
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
