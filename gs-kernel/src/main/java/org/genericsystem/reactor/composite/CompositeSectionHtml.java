package org.genericsystem.reactor.composite;

import org.genericsystem.reactor.CompositeModel;
import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.CompositeModel.ModelConstructor;
import org.genericsystem.reactor.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.CompositeModel.StringExtractor;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.html.HtmlSection;

public class CompositeSectionHtml<M extends CompositeModel> extends HtmlSection<M> {

	private StringExtractor stringExtractor = StringExtractor.SIMPLE_CLASS_EXTRACTOR;
	private ObservableListExtractor observableListExtractor;
	private ModelConstructor<CompositeModel> modelConstructor = CompositeModel::new;

	public CompositeSectionHtml(HtmlElement<?, ?, ?> parent) {
		super(parent);
		addStyleClass("gstable");
		setObservableListExtractor(ObservableListExtractor.INSTANCES);
	}

	@Override
	protected void initChildren() {
		HtmlSection<CompositeModel> htmlSection = new HtmlSection<CompositeModel>(this).addStyleClass("gscell").addStyleClass("gstitlecell");
		htmlSection.forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs), (a, b) -> getModelConstructor().build(a, b)).addStyleClass("gscell");
		initSubChildren(htmlSection);
	}

	protected void initSubChildren(HtmlSection<CompositeModel> parentSection) {
		new HtmlLabel<CompositeModel>(parentSection).bindText(CompositeModel::getString);
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

	public static class TitleCompositeSectionHtml<M extends CompositeModel> extends CompositeSectionHtml<M> {

		public TitleCompositeSectionHtml(HtmlElement<?, ?, ?> parent) {
			super(parent);
		}

		@Override
		protected void initChildren() {
			new HtmlH1<M>(new HtmlSection<M>(this).addStyleClass("gsrow").addStyleClass("gstitlerow")).bindText(CompositeModel::getString);
			super.initChildren();
		}
	}
}
