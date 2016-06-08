package org.genericsystem.reactor.composite;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.composite.CompositeModel.ModelConstructor;
import org.genericsystem.reactor.composite.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.composite.CompositeModel.StringExtractor;
import org.genericsystem.reactor.flex.FlexColumn;
import org.genericsystem.reactor.flex.FlexRow;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlLabel;

/**
 * @author Nicolas Feybesse
 *
 */
public class CompositeSectionHtml<M extends CompositeModel> extends FlexColumn<M> {

	public CompositeSectionHtml(HtmlElement<?, ?> parent) {
		super(parent);
		header();
		body();
		footer();
	}

	protected void header() {
	}

	protected void body() {
		new FlexRow<CompositeModel>(this) {
			{
				forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs), (gs, extractor) -> getModelConstructor().build(gs, extractor));
				new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
			}
		};
	}

	protected void footer() {
	}

	public StringExtractor getStringExtractor() {
		return StringExtractor.SIMPLE_CLASS_EXTRACTOR;
	}

	public ObservableListExtractor getObservableListExtractor() {
		return ObservableListExtractor.INSTANCES;
	}

	@SuppressWarnings("unchecked")
	public ModelConstructor<M> getModelConstructor() {
		return (ModelConstructor) CompositeModel::new;
	}

	public static class LabelCompositeSectionHtml<M extends CompositeModel> extends CompositeSectionHtml<M> {

		@Override
		protected void header() {
			new FlexRow<CompositeModel>(this) {
				{
					addStyle("justify-content", "center");
					addStyle("background-color", "#ffa500");
					new HtmlH1<CompositeModel>(this) {
						{
							bindText(CompositeModel::getString);
						}
					};
				};
			};
		}

		public LabelCompositeSectionHtml(HtmlElement<?, ?> parent) {
			super(parent);
		}

	}

	public static class ColorCompositeSectionHtml<M extends CompositeModel> extends LabelCompositeSectionHtml<M> {

		public ColorCompositeSectionHtml(HtmlElement<?, ?> parent) {
			super(parent);
		}

		@Override
		protected void body() {
			new FlexRow<CompositeModel>(this) {
				{
					FlexRow<CompositeModel> row = this;
					bindStyle("background-color");
					forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs), (gs, extractor) -> new CompositeModel(gs, extractor) {
						{
							getStyleProperty(row, "background-color").setValue(getString().getValue());
						}
					});
					new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
				}
			};
		}
	}

}
