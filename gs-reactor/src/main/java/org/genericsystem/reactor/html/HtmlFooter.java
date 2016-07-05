package org.genericsystem.reactor.html;

import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlFooter<M extends Model> extends Tag<M> {

	public HtmlFooter(Tag<?> parent) {
		super(parent, "footer");
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}

}
