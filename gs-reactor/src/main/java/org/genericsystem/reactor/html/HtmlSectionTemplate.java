package org.genericsystem.reactor.html;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.HtmlElement.HtmlDomNode;
import org.genericsystem.reactor.Model;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlSectionTemplate<M extends Model, COMPONENT extends HtmlSectionTemplate<M, COMPONENT>> extends HtmlElement<M, COMPONENT, HtmlDomNode> {

	public HtmlSectionTemplate(HtmlElement<?, ?, ?> parent) {
		super(parent, HtmlDomNode.class);
	}

	@Override
	protected HtmlDomNode createNode(Object parent) {
		return new HtmlDomNode("section");
	}

	public static class HtmlSection<M extends Model> extends HtmlSectionTemplate<M, HtmlSection<M>> {

		public HtmlSection(HtmlElement<?, ?, ?> parent) {
			super(parent);
		}
	}
}
