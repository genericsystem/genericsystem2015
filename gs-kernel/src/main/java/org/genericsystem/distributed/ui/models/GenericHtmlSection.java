package org.genericsystem.distributed.ui.models;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.components.HtmlSection;
import org.genericsystem.distributed.ui.models.CompositeModel.StringExtractor;

public class GenericHtmlSection<M extends CompositeModel<?>> extends HtmlSection<M> {
	private final StringExtractor stringExtractor;

	public GenericHtmlSection(HtmlElement<?, ?, ?> parent, StringExtractor stringExtractor) {
		super(parent);
		this.stringExtractor = stringExtractor;
	}

	public StringExtractor getStringExtractor() {
		return stringExtractor;
	}
}
