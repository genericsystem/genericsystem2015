package org.genericsystem.distributed.cacheonserver.ui.table;

import org.genericsystem.distributed.cacheonserver.ui.list.GSCompositeHtml.GSTitleCompositeHtml;
import org.genericsystem.distributed.ui.CompositeModel;
import org.genericsystem.distributed.ui.CompositeModel.ObservableListExtractor;
import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.components.HtmlSection;

public class TypeTableHtml<M extends CompositeModel> extends GSTitleCompositeHtml<M> {

	private ObservableListExtractor attributesExtractor = ObservableListExtractor.ATTRIBUTES;

	public TypeTableHtml(HtmlElement<?, ?, ?> parent) {
		super(parent);
		addStyleClass("gstable");
		setObservableListExtractor(ObservableListExtractor.INSTANCES);
	}

	public ObservableListExtractor getAttributesExtractor() {
		return attributesExtractor;
	}

	public TypeTableHtml<M> setAttributesExtractor(ObservableListExtractor attributesExtractor) {
		this.attributesExtractor = attributesExtractor;
		return this;
	}

	@Override
	protected void initSubChildren(HtmlSection<CompositeModel> parentSection) {
		new InstanceRowHtml<>(parentSection);
	}

}
