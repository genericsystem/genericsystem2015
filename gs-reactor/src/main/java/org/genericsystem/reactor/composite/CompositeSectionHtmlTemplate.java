package org.genericsystem.reactor.composite;

import org.genericsystem.reactor.CompositeModel;
import org.genericsystem.reactor.CompositeModel.ModelConstructor;
import org.genericsystem.reactor.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.CompositeModel.StringExtractor;
import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.html.HtmlSectionTemplate;

public abstract class CompositeSectionHtmlTemplate<M extends CompositeModel, COMPONENT extends CompositeSectionHtmlTemplate<M, COMPONENT>>
		extends HtmlSectionTemplate<M, COMPONENT> {

	private StringExtractor stringExtractor = StringExtractor.SIMPLE_CLASS_EXTRACTOR;
	private ObservableListExtractor observableListExtractor;
	private ModelConstructor<CompositeModel> modelConstructor = CompositeModel::new;

	public CompositeSectionHtmlTemplate(HtmlElement<?, ?, ?> parent) {
		super(parent);
		addStyleClass("gstable");
		setObservableListExtractor(ObservableListExtractor.INSTANCES);
	}

	@Override
	protected void initChildren() {
		HtmlSection<CompositeModel> htmlSection = new HtmlSection<CompositeModel>(this).addStyleClass("gscell").addStyleClass("gstitlecell");
		htmlSection.forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs),
				(gs, constructor) -> getModelConstructor().build(gs, constructor)).addStyleClass("gscell");
		initSubChildren(htmlSection);
	}

	protected void initSubChildren(HtmlSection<CompositeModel> parentSection) {
		new HtmlLabel<CompositeModel>(parentSection).bindText(CompositeModel::getString);
	}

	public StringExtractor getStringExtractor() {
		return stringExtractor;
	}

	@SuppressWarnings("unchecked")
	public COMPONENT setStringExtractor(StringExtractor stringExtractor) {
		this.stringExtractor = stringExtractor;
		return (COMPONENT) this;
	}

	public ObservableListExtractor getObservableListExtractor() {
		return observableListExtractor;
	}

	@SuppressWarnings("unchecked")
	public COMPONENT setObservableListExtractor(ObservableListExtractor observableListExtractor) {
		this.observableListExtractor = observableListExtractor;
		return (COMPONENT) this;
	}

	public ModelConstructor<CompositeModel> getModelConstructor() {
		return modelConstructor;
	}

	@SuppressWarnings("unchecked")
	public COMPONENT setModelConstructor(ModelConstructor<CompositeModel> modelConstructor) {
		this.modelConstructor = modelConstructor;
		return (COMPONENT) this;
	}

	public static class CompositeSectionHtml<M extends CompositeModel> extends CompositeSectionHtmlTemplate<M, CompositeSectionHtml<M>> {
		public CompositeSectionHtml(HtmlElement<?, ?, ?> parent) {
			super(parent);
		}
	}

	public static abstract class TitleCompositeSectionHtmlTemplate<M extends CompositeModel, COMPONENT extends TitleCompositeSectionHtmlTemplate<M, COMPONENT>>
			extends CompositeSectionHtmlTemplate<M, COMPONENT> {

		public TitleCompositeSectionHtmlTemplate(HtmlElement<?, ?, ?> parent) {
			super(parent);
		}

		@Override
		protected void initChildren() {
			new HtmlH1<M>(new HtmlSection<M>(this).addStyleClass("gsrow").addStyleClass("gstitlerow")).bindText(CompositeModel::getString);
			super.initChildren();
		}
	}

	public static class TitleCompositeSectionHtml<M extends CompositeModel> extends TitleCompositeSectionHtmlTemplate<M, TitleCompositeSectionHtml<M>> {
		public TitleCompositeSectionHtml(HtmlElement<?, ?, ?> parent) {
			super(parent);
		}
	}
}
