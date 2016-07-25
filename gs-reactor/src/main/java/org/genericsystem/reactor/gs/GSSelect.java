package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gstag.GSOption;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;

import javafx.beans.binding.Bindings;

public class GSSelect extends GSTag {

	public GSOption optionElement;

	private GSSelect(GSTag parent) {
		super(parent, "select");
		options();
		init();
		createProperty(ReactorStatics.SELECTION);
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
		}

		@Override
		protected void options() {
			new GSOption(this);
			super.options();
		}

		@Override
		protected void init() {
			createProperty(ReactorStatics.SELECTION_SHIFT);
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
		}

		@Override
		public ObservableListExtractor getObservableListExtractor() {
			return ObservableListExtractor.SUBINSTANCES_OF_META;
		}
	}
}
