package org.genericsystem.reactor.composite;

import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.html.HtmlOption;
import org.genericsystem.reactor.html.HtmlSelect;
import org.genericsystem.reactor.model.CompositeModel;
import org.genericsystem.reactor.model.SelectorModel;

public class CompositeSelect<M extends SelectorModel> extends HtmlSelect<M> implements CompositeElement<M> {
	
	HtmlOption<CompositeModel> optionElement;

	public CompositeSelect(Element<?> parent) {
		super(parent);
		options();
		bindOptionElement();
	}

	protected void options() {
		optionElement = new HtmlOption<CompositeModel>(this) {
			{
				bindText(CompositeModel::getString);
				forEach(CompositeSelect.this);
			}
		};
	}
	
	protected void bindOptionElement() {
		bindBiDirectionalSelection(optionElement);
	}


	public static class CompositeSelectWithEmptyEntry<M extends SelectorModel> extends CompositeSelect<M> {

		public CompositeSelectWithEmptyEntry(Element<?> parent) {
			super(parent);
		}

		protected void options() {
			new HtmlOption<CompositeModel>(this);
			optionElement = new HtmlOption<CompositeModel>(this) {
				{
					bindText(CompositeModel::getString);
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

		public ColorsSelect(Element<?> parent) {
			super(parent);
			bindStyle("background-color", SelectorModel::getSelectionString);
		}

		@Override
		protected void options() {
			optionElement = new HtmlOption<CompositeModel>(this) {
				{
					bindText(CompositeModel::getString);
					bindStyle("background-color", CompositeModel::getString);
					forEach(ColorsSelect.this);
				}
			};
		}
	}
}