package org.genericsystem.distributed.ui.models;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.models.CompositeModel.StringExtractor;

public class DynamicCompositeHtmlSection<M extends CompositeModel<?>> extends CompositeHtmlSection<M> {
	public DynamicCompositeHtmlSection(HtmlElement<?, ?, ?> parent, StringExtractor stringExtractor) {
		super(parent, stringExtractor);
	}
}