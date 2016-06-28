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
		bindBiDirectionalSelection(optionElement);
	}

	protected void options() {
		optionElement = new HtmlOption<CompositeModel>(this) {
			{
				bindText(CompositeModel::getString);
				forEach(CompositeSelect.this);
			}
		};
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