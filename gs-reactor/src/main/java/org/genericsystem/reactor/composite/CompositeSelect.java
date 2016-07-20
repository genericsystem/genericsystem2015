package org.genericsystem.reactor.composite;

import javafx.beans.binding.Bindings;

import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.html.HtmlOption;
import org.genericsystem.reactor.html.HtmlSelect;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;

public class CompositeSelect extends HtmlSelect implements CompositeTag<GenericModel> {

	protected HtmlOption<GenericModel> optionElement;

	private CompositeSelect(Tag<?> parent) {
		super(parent);
		options();
		init();
		createProperty(ReactorStatics.SELECTION);
		storeProperty(ReactorStatics.SELECTION_INDEX, model -> model.getSelectionIndex(this));
		bindBiDirectionalSelection(optionElement);
		storeProperty(ReactorStatics.SELECTION_STRING,
				model -> Bindings.createStringBinding(() -> getStringExtractor().apply(getProperty(ReactorStatics.SELECTION, model).getValue() != null ? ((GenericModel) getProperty(ReactorStatics.SELECTION, model).getValue()).getGeneric() : null),
						getProperty(ReactorStatics.SELECTION, model)));
	}

	protected void options() {
		optionElement = new HtmlOption<GenericModel>(this) {
			{
				bindText(GenericModel::getString);
				forEach(CompositeSelect.this);
			}
		};
	}

	protected void init() {

	}

	public static class CompositeSelectWithEmptyEntry extends CompositeSelect {

		public CompositeSelectWithEmptyEntry(Tag<?> parent) {
			super(parent);
		}

		@Override
		protected void options() {
			new HtmlOption<GenericModel>(this);
			super.options();
		}

		@Override
		protected void init() {
			initProperty(ReactorStatics.SELECTION_SHIFT, 1);
		}
	}

	public static class ColorsSelect extends CompositeSelect {

		public ColorsSelect(Tag<?> parent) {
			super(parent);
			bindStyle("background-color", ReactorStatics.SELECTION_STRING);
			optionElement.bindStyle("background-color", ReactorStatics.TEXT, GenericModel::getString);
		}
	}

	public static class InstanceCompositeSelect extends CompositeSelect implements CompositeTag<GenericModel> {

		public InstanceCompositeSelect(Tag<?> parent) {
			super(parent);
		}

		@Override
		public ObservableListExtractor getObservableListExtractor() {
			return ObservableListExtractor.SUBINSTANCES_OF_META;
		}
	}

}
