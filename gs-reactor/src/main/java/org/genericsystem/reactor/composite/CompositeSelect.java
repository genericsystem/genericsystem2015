package org.genericsystem.reactor.composite;

import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.html.HtmlOption;
import org.genericsystem.reactor.html.HtmlSelect;

public class CompositeSelect<M extends CompositeModel> extends HtmlSelect<M> implements Composite<M> {

	public CompositeSelect(Element<?> parent) {
		super(parent);
		options();
	}

	protected void options() {
		new HtmlOption<CompositeModel>(this) {
			{
				bindText(CompositeModel::getString);
				bindAction(CompositeModel::select);
				forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs), (a, b) -> getModelConstructor().build(a, b));
			}
		};
	}

	public static class ColorsSelect<M extends CompositeModel> extends CompositeSelect<M> {

		public ColorsSelect(Element<?> parent) {
			super(parent);
			bindStyle("background-color", model -> model.getObservableStyle(ColorsSelect.this, "background-color", model.getString().getValue()));
		}

		@Override
		protected void options() {
			new HtmlOption<CompositeModel>(this) {
				{
					ColorsSelect<M> parent = ColorsSelect.this;
					bindStyle("background-color");
					HtmlOption<CompositeModel> row = this;
					bindText(CompositeModel::getString);
					bindAction(
							model -> ((CompositeModel) model.getParent()).getStyleProperty(parent, "background-color").setValue(model.getString().getValue()));
					forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs),
							(gs, extractor) -> new CompositeModel(gs, extractor) {
								{
									getStyleProperty(row, "background-color").setValue(getString().getValue());
								}
							});
				}

			};
		}
	}
}