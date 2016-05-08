package org.genericsystem.distributed.ui.models;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.models.CompositeModel.StringExtractor;

public class CompositeHtmlSection<M extends CompositeModel<?>> extends GenericHtmlSection<M> {

	public CompositeHtmlSection(HtmlElement<?, ?, ?> parent, StringExtractor stringExtractor) {
		super(parent, stringExtractor);
	}
}