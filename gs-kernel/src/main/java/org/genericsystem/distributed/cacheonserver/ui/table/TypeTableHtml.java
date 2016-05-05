package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.components.HtmlH1;
import org.genericsystem.distributed.ui.components.HtmlSection;
import org.genericsystem.distributed.ui.models.CompositeHtmlSection;
import org.genericsystem.distributed.ui.models.CompositeModel.CompositeGenericConstructor;
import org.genericsystem.distributed.ui.models.CompositeModel.StringExtractor;

public class TypeTableHtml<M extends TypeTableModel> extends CompositeHtmlSection<M> {

	public TypeTableHtml(HtmlElement<?, ?, ?> parent, StringExtractor extractor, CompositeGenericConstructor<M, InstanceRowModel> constructor) {
		super(parent, extractor, constructor);
		addStyleClass("gstable");
	}

	@Override
	protected void initChildren() {
		new HtmlH1<M>(new HtmlSection<>(this).addStyleClass("gsrow").addStyleClass("gstitlerow")).bindText(TypeTableModel::getString);
		new InstanceRowHtml<>(this).forEach(TypeTableModel::getSubModels);
	}
}
