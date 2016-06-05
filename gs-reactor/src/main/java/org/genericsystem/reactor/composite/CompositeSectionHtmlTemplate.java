package org.genericsystem.reactor.composite;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.composite.CompositeModel.ModelConstructor;
import org.genericsystem.reactor.composite.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.composite.CompositeModel.StringExtractor;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.html.HtmlSection;

public abstract class CompositeSectionHtmlTemplate<M extends CompositeModel> extends HtmlSection<M> {

	private StringExtractor stringExtractor = StringExtractor.SIMPLE_CLASS_EXTRACTOR;
	private ObservableListExtractor observableListExtractor;
	private ModelConstructor<CompositeModel> modelConstructor = CompositeModel::new;

	public CompositeSectionHtmlTemplate(HtmlElement<?, ?> parent) {
		super(parent);
		// this.bindStyles(CompositeModel::getFlexStyles);
		addStyle("display", "flex");
		addStyle("flex-direction", "column");
		addStyle("flex-wrap", "nowrap");
		// addStyle("margin-bottom", "11px");
		setObservableListExtractor(ObservableListExtractor.INSTANCES);
	}

	protected void initSubChildren(HtmlSection<CompositeModel> subSection) {
		new HtmlLabel<CompositeModel>(subSection).bindText(CompositeModel::getString);
	}

	public StringExtractor getStringExtractor() {
		return stringExtractor;
	}

	public void setStringExtractor(StringExtractor stringExtractor) {
		this.stringExtractor = stringExtractor;
	}

	public ObservableListExtractor getObservableListExtractor() {
		return observableListExtractor;
	}

	public void setObservableListExtractor(ObservableListExtractor observableListExtractor) {
		this.observableListExtractor = observableListExtractor;
	}

	public ModelConstructor<CompositeModel> getModelConstructor() {
		return modelConstructor;
	}

	public void setModelConstructor(ModelConstructor<CompositeModel> modelConstructor) {
		this.modelConstructor = modelConstructor;
	}

	public static class CompositeSectionHtml<M extends CompositeModel> extends CompositeSectionHtmlTemplate<M> {
		public CompositeSectionHtml(HtmlElement<?, ?> parent) {
			super(parent);
			HtmlSection<CompositeModel> subSection = new HtmlSection<CompositeModel>(this) {
				{
					addStyle("flex", "1");
					forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs), (gs, stringExtractor) -> getModelConstructor().build(gs, stringExtractor));
					new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
				}
			};
			new HtmlLabel<CompositeModel>(subSection).bindText(CompositeModel::getString);
		}
	}

	public static abstract class TitleCompositeSectionHtmlTemplate<M extends CompositeModel, COMPONENT extends TitleCompositeSectionHtmlTemplate<M, COMPONENT>> extends CompositeSectionHtmlTemplate<M> {

		public TitleCompositeSectionHtmlTemplate(HtmlElement<?, ?> parent) {
			super(parent);
		}
	}

	public static class TitleCompositeSectionHtml<M extends CompositeModel> extends TitleCompositeSectionHtmlTemplate<M, TitleCompositeSectionHtml<M>> {
		public TitleCompositeSectionHtml(HtmlElement<?, ?> parent) {
			super(parent);
			new HtmlSection<CompositeModel>(this) {
				{
					new HtmlH1<M>(this);
				}
			};
			new HtmlSection<CompositeModel>(this) {
				{
					addStyle("flex", "1");
					forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs), (gs, stringExtractor) -> getModelConstructor().build(gs, stringExtractor));
					new HtmlLabel<CompositeModel>(this).bindText(CompositeModel::getString);
				}
			};
		}
	}
}
