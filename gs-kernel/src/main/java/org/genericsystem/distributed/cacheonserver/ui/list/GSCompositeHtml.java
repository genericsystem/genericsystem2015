package org.genericsystem.distributed.cacheonserver.ui.list;

import org.genericsystem.distributed.ui.CompositeModel;
import org.genericsystem.distributed.ui.CompositeModel.ModelConstructor;
import org.genericsystem.distributed.ui.CompositeModel.ObservableListExtractor;
import org.genericsystem.distributed.ui.CompositeModel.StringExtractor;
import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.components.HtmlH1;
import org.genericsystem.distributed.ui.components.HtmlLabel;
import org.genericsystem.distributed.ui.components.HtmlSection;

public class GSCompositeHtml<M extends CompositeModel> extends HtmlSection<M> {

	private StringExtractor stringExtractor = StringExtractor.SIMPLE_CLASS_EXTRACTOR;
	private ObservableListExtractor observableListExtractor;
	private ModelConstructor<CompositeModel> modelConstructor = CompositeModel::new;

	public GSCompositeHtml(HtmlElement<?, ?, ?> parent) {
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

	public static class GSTitleCompositeHtml<M extends CompositeModel> extends GSCompositeHtml<M> {

		public GSTitleCompositeHtml(HtmlElement<?, ?, ?> parent) {
			super(parent);
		}

		@Override
		protected void initChildren() {
			new HtmlH1<M>(new HtmlSection<M>(this).addStyleClass("gsrow").addStyleClass("gstitlerow")).bindText(CompositeModel::getString);
			super.initChildren();
		}
	}
}
