package org.genericsystem.distributed.cacheonserver.ui.list;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.components.HtmlH1;
import org.genericsystem.distributed.ui.components.HtmlLabel;
import org.genericsystem.distributed.ui.components.HtmlSection;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.CompositeModel.ModelConstructor;
import org.genericsystem.distributed.ui.models.CompositeModel.ObservableListExtractor;
import org.genericsystem.distributed.ui.models.CompositeModel.StringExtractor;

public class TypeSectionHtml<M extends CompositeModel> extends HtmlSection<M> {

	private StringExtractor stringExtractor = StringExtractor.SIMPLE_CLASS_EXTRACTOR;
	private ObservableListExtractor observableListExtractor = generics -> generics[0].getObservableSubInstances();
	private ModelConstructor<?> modelConstructor = CompositeModel::new;

	public TypeSectionHtml(HtmlElement<?, ?, ?> parent) {
		super(parent);
		addStyleClass("gstable");
	}

	@Override
	protected void initChildren() {
		HtmlSection<?> htmlSection = new HtmlSection<>(this).addStyleClass("gscell").addStyleClass("gstitlecell");
		htmlSection.forEach(g -> getStringExtractor().apply(g), gs -> getObservableListExtractor().apply(gs), (a, b) -> getModelConstructor().build(a, b));
		initSubChildren(htmlSection);
	}

	protected void initSubChildren(HtmlSection<?> parentSection) {
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

	public ModelConstructor<?> getModelConstructor() {
		return modelConstructor;
	}

	public void setModelConstructor(ModelConstructor<?> modelConstructor) {
		this.modelConstructor = modelConstructor;
	}

	public static class TitleTypeListHtml<M extends CompositeModel> extends TypeSectionHtml<M> {

		public TitleTypeListHtml(HtmlElement<?, ?, ?> parent) {
			super(parent);
		}

		@Override
		protected void initChildren() {
			new HtmlH1<M>(new HtmlSection<M>(this).addStyleClass("gsrow").addStyleClass("gstitlerow")).bindText(CompositeModel::getString);
			super.initChildren();
		}
	}
}
