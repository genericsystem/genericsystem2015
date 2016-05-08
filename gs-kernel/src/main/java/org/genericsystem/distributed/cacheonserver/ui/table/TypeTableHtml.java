package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.components.HtmlH1;
import org.genericsystem.distributed.ui.components.HtmlSection;
import org.genericsystem.distributed.ui.models.CompositeHtmlSection;
import org.genericsystem.distributed.ui.models.CompositeModel.ObservableListExtractor;
import org.genericsystem.distributed.ui.models.CompositeModel.StringExtractor;

public class TypeTableHtml<M extends TypeTableModel> extends CompositeHtmlSection<M> {

	private ObservableListExtractor attributesExtractor = gs -> gs[0].getObservableAttributes();

	public TypeTableHtml(HtmlElement<?, ?, ?> parent, StringExtractor extractor) {
		super(parent, extractor);
		addStyleClass("gstable");
	}

	public ObservableListExtractor getAttributesExtractor() {
		return attributesExtractor;
	}

	public TypeTableHtml<M> setAttributesExtractor(ObservableListExtractor attributesExtractor) {
		this.attributesExtractor = attributesExtractor;
		return this;
	}

	@Override
	protected void initChildren() {
		new HtmlH1<M>(new HtmlSection<>(this).addStyleClass("gsrow").addStyleClass("gstitlerow")).bindText(TypeTableModel::getString);
		new InstanceRowHtml<InstanceRowModel>(this, StringExtractor.SIMPLE_CLASS_EXTRACTOR).forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, generics -> generics[0].getObservableSubInstances(), InstanceRowModel::new);
	}

}
