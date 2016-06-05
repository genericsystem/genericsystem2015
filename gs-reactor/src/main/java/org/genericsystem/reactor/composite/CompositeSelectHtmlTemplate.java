package org.genericsystem.reactor.composite;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.composite.CompositeModel.ModelConstructor;
import org.genericsystem.reactor.composite.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.composite.CompositeModel.StringExtractor;
import org.genericsystem.reactor.html.HtmlOption;
import org.genericsystem.reactor.html.HtmlSelect;

public abstract class CompositeSelectHtmlTemplate<M extends CompositeModel> extends HtmlSelect<M> {

	private StringExtractor stringExtractor = StringExtractor.SIMPLE_CLASS_EXTRACTOR;
	private ObservableListExtractor observableListExtractor = ObservableListExtractor.INSTANCES;
	private ModelConstructor<CompositeModel> modelConstructor = CompositeModel::new;

	public CompositeSelectHtmlTemplate(HtmlElement<?, ?> parent) {
		super(parent);
	}

	public StringExtractor getStringExtractor() {
		return stringExtractor;
	}

	public CompositeSelectHtmlTemplate<M> setStringExtractor(StringExtractor stringExtractor) {
		this.stringExtractor = stringExtractor;
		return this;
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

	public static class CompositeSelectHtml<M extends CompositeModel> extends CompositeSelectHtmlTemplate<M> {
		public CompositeSelectHtml(HtmlElement<?, ?> parent) {
			super(parent);
			new HtmlOption<CompositeModel>(this) {
				{
					bindText(CompositeModel::getString);
					bindAction(CompositeModel::select);
					forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs), (a, b) -> getModelConstructor().build(a, b));
				}
			};
		}
	}
}
