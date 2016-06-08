package org.genericsystem.reactor.flex;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.HtmlElement.HtmlDomNode;
import org.genericsystem.reactor.html.HtmlSection;

public class FlexColumn<M extends Model> extends HtmlSection<M> {

	public FlexColumn(HtmlElement<?, ?> parent) {
		super(parent);
		addStyle("display", "flex");
		addStyle("flex-direction", "column");
		addStyle("flex-wrap", "nowrap");
	}

	@Override
	protected HtmlDomNode createNode(Object parent) {
		return new HtmlDomNode("section");
	}
}