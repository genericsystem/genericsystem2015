package org.genericsystem.reactor.composite;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.html.HtmlOption;
import org.genericsystem.reactor.html.HtmlSelect;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.SelectorModel;

public class CompositeSelect<M extends SelectorModel> extends HtmlSelect<M> implements CompositeTag<M> {
	
	HtmlOption<GenericModel> optionElement;

	public CompositeSelect(Tag<?> parent) {
		super(parent);
		options();
		bindOptionElement();
	}

	protected void options() {
		optionElement = new HtmlOption<GenericModel>(this) {
			{
				bindText(GenericModel::getString);
				forEach(CompositeSelect.this);
			}
		};
	}
	
	protected void bindOptionElement() {
		bindBiDirectionalSelection(optionElement);
	}


	public static class CompositeSelectWithEmptyEntry<M extends SelectorModel> extends CompositeSelect<M> {

		public CompositeSelectWithEmptyEntry(Tag<?> parent) {
			super(parent);
		}

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
		protected void bindOptionElement() {
			bindBiDirectionalSelection(optionElement, 1);
		}
	}

	public static class ColorsSelect<M extends SelectorModel> extends CompositeSelect<M> {

		public ColorsSelect(Tag<?> parent) {
			super(parent);
			bindStyle("background-color", SelectorModel::getSelectionString);
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
}