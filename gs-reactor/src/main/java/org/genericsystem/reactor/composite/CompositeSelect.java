package org.genericsystem.reactor.composite;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.html.HtmlOption;
import org.genericsystem.reactor.html.HtmlSelect;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;

public class CompositeSelect<M extends GenericModel> extends HtmlSelect<M> implements CompositeTag<M> {

	HtmlOption<GenericModel> optionElement;

	public CompositeSelect(Tag<?> parent) {
		super(parent);
		options();
		markSelector();
		bindOptionsToSelection();
		initSelection(optionElement);
	}

	protected void options() {
		optionElement = new HtmlOption<GenericModel>(this) {
			{
				bindText(GenericModel::getString);
				forEach(CompositeSelect.this);
			}
		};
	}

	protected void bindOptionsToSelection() {
		bindBiDirectionalSelection(optionElement);
	}

	public static class CompositeSelectWithEmptyEntry<M extends GenericModel> extends CompositeSelect<M> {

		public CompositeSelectWithEmptyEntry(Tag<?> parent) {
			super(parent);
		}

		@Override
		protected void options() {
			new HtmlOption<GenericModel>(this);
			optionElement = new HtmlOption<GenericModel>(this) {
				{
					bindText(GenericModel::getString);
					forEach(CompositeSelectWithEmptyEntry.this);
				}
			};
		}

		@Override
		protected void bindOptionsToSelection() {
			bindBiDirectionalSelection(optionElement, 1);
		}
	}

	public static class ColorsSelect<M extends GenericModel> extends CompositeSelect<M> {

		public ColorsSelect(Tag<?> parent) {
			super(parent);
			bindStyle("background-color", GenericModel::getSelectionString);
		}

		@Override
		protected void options() {
			optionElement = new HtmlOption<GenericModel>(this) {
				{
					bindText(GenericModel::getString);
					bindStyle("background-color", GenericModel::getString);
					forEach(ColorsSelect.this);
				}
			};
		}
	}

	public static class InstanceCompositeSelect<M extends GenericModel> extends CompositeSelect<M> implements CompositeTag<M> {

		public InstanceCompositeSelect(Tag<?> parent) {
			super(parent);
		}

		@Override
		public ObservableListExtractor getObservableListExtractor() {
			return ObservableListExtractor.SUBINSTANCES_OF_META;
		}
	}

}
