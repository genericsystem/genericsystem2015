package org.genericsystem.reactor.table;

import org.genericsystem.reactor.CompositeModel;
import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.components.HtmlSection;
import org.genericsystem.reactor.list.GSCompositeHtml.GSTitleCompositeHtml;

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
