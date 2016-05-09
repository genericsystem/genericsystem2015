package org.genericsystem.distributed.cacheonserver.ui.list;

import org.genericsystem.distributed.ui.CompositeModel;
import org.genericsystem.distributed.ui.CompositeModel.ModelConstructor;
import org.genericsystem.distributed.ui.CompositeModel.ObservableListExtractor;
import org.genericsystem.distributed.ui.CompositeModel.StringExtractor;
import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.components.HtmlOption;
import org.genericsystem.distributed.ui.components.HtmlSelect;

public class GSSelectHtml<M extends CompositeModel> extends HtmlSelect<M> {

	private StringExtractor stringExtractor = StringExtractor.SIMPLE_CLASS_EXTRACTOR;
	private ObservableListExtractor observableListExtractor = ObservableListExtractor.INSTANCES;
	private ModelConstructor<CompositeModel> modelConstructor = CompositeModel::new;

	public GSSelectHtml(HtmlElement<?, ?, ?> parent) {
		super(parent);
	}

	@Override
	protected void initChildren() {
		new HtmlOption<CompositeModel>(this).forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs), (a, b) -> getModelConstructor().build(a, b)).bindText(CompositeModel::getString).bindAction(CompositeModel::select);
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
}
