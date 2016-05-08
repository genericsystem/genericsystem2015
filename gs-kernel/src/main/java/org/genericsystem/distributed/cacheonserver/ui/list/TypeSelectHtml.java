package org.genericsystem.distributed.cacheonserver.ui.list;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.components.HtmlOption;
import org.genericsystem.distributed.ui.components.HtmlSelect;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.CompositeModel.ModelConstructor;
import org.genericsystem.distributed.ui.models.CompositeModel.ObservableListExtractor;
import org.genericsystem.distributed.ui.models.CompositeModel.StringExtractor;

public class TypeSelectHtml<M extends CompositeModel> extends HtmlSelect<M> {

	private StringExtractor stringExtractor = StringExtractor.SIMPLE_CLASS_EXTRACTOR;
	private ObservableListExtractor observableListExtractor = generics -> generics[0].getObservableSubInstances();
	private ModelConstructor<?> modelConstructor = CompositeModel::new;

	public TypeSelectHtml(HtmlElement<?, ?, ?> parent) {
		super(parent);
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

	public ModelConstructor<?> getModelConstructor() {
		return modelConstructor;
	}

	public void setModelConstructor(ModelConstructor<?> modelConstructor) {
		this.modelConstructor = modelConstructor;
	}

	@Override
	protected void initChildren() {
		new HtmlOption<CompositeModel>(this).forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs), (a, b) -> getModelConstructor().build(a, b)).bindText(CompositeModel::getString);
	}
}
