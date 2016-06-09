package org.genericsystem.reactor.html;

import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.Model;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlFooter<M extends Model> extends Element<M> {

	public HtmlFooter(Element<?> parent) {
		super(parent, "footer");
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}

}
