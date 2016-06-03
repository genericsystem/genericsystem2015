package org.genericsystem.reactor.composite;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.composite.CompositeModel.ModelConstructor;
import org.genericsystem.reactor.composite.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.composite.CompositeModel.StringExtractor;
import org.genericsystem.reactor.html.HtmlOption;
import org.genericsystem.reactor.html.HtmlSelectTemplate;

public abstract class CompositeSelectHtmlTemplate<M extends CompositeModel, COMPONENT extends CompositeSelectHtmlTemplate<M, COMPONENT>>
		extends HtmlSelectTemplate<M, COMPONENT> {

	private StringExtractor stringExtractor = StringExtractor.SIMPLE_CLASS_EXTRACTOR;
	private ObservableListExtractor observableListExtractor = ObservableListExtractor.INSTANCES;
	private ModelConstructor<CompositeModel> modelConstructor = CompositeModel::new;

	public CompositeSelectHtmlTemplate(HtmlElement<?, ?, ?> parent) {
		super(parent);
	}

	@Override
	protected void initChildren() {
		new HtmlOption<CompositeModel>(this)
				.forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs), (a, b) -> getModelConstructor().build(a, b))
				.bindText(CompositeModel::getString).bindAction(CompositeModel::select);
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

	public static class CompositeSelectHtml<M extends CompositeModel> extends CompositeSelectHtmlTemplate<M, CompositeSelectHtml<M>> {

		public CompositeSelectHtml(HtmlElement<?, ?, ?> parent) {
			super(parent);
		}

	}
}
