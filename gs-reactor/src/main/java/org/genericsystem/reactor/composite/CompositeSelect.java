package org.genericsystem.reactor.composite;

import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.html.HtmlOption;
import org.genericsystem.reactor.html.HtmlSelect;

import javafx.collections.MapChangeListener;

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
			addStyle("background-color", "White");
		}

		@Override
		protected void options() {
			new HtmlOption<CompositeModel>(this) {
				{
					ColorsSelect<M> parent = ColorsSelect.this;

					HtmlOption<CompositeModel> row = this;
					bindText(CompositeModel::getString);
					bindAction(model -> {
						model.getParent().getObservableStyles(parent)
								.addListener((MapChangeListener<String, String>) change -> System.out.println("zzz" + change));
						System.out.println("put : background-color," + model.getString().getValue());
						model.getParent().getObservableStyles(parent).remove("background-color");
						model.getParent().getObservableStyles(parent).put("background-color", model.getString().getValue());
					});
					forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs),
							(gs, extractor) -> new CompositeModel(gs, extractor) {
								{
									getObservableStyles(row).put("background-color", getString().getValue());
								}
							});
				}

			};
		}
	}
}