package org.genericsystem.reactor.composite;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.html.HtmlOption;
import org.genericsystem.reactor.html.HtmlSelect;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;

public class CompositeSelect extends HtmlSelect implements CompositeTag<GenericModel> {

	protected HtmlOption<GenericModel> optionElement;
	private final int shift;

	public CompositeSelect(Tag<?> parent) {
		this(parent, 0);
	}

	private CompositeSelect(Tag<?> parent, int shift) {
		super(parent);
		this.shift = shift;
		options();
		enableSelectorBehavior();
		bindBiDirectionalSelection(optionElement, shift);
	}

	protected void options() {
		optionElement = new HtmlOption<GenericModel>(this) {
			{
				bindText(GenericModel::getString);
				forEach(CompositeSelect.this);
			}
		};
	}

	public static class CompositeSelectWithEmptyEntry extends CompositeSelect {

		public CompositeSelectWithEmptyEntry(Tag<?> parent) {
			super(parent, 1);
		}

		@Override
		protected void options() {
			new HtmlOption<GenericModel>(this);
			super.options();
		}
	}

	public static class ColorsSelect extends CompositeSelect {

		public ColorsSelect(Tag<?> parent) {
			super(parent);
			bindStyle("background-color", GenericModel::getSelectionString);
			optionElement.bindStyle("background-color", GenericModel::getString);
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
