package org.genericsystem.reactor.html;

import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlH1<M extends Model> extends Tag<M> {

	public HtmlH1(Tag<?> parent) {
		super(parent, "h1");
	}

	public HtmlH1(Tag<?> parent, String text) {
		super(parent, "h1");
		setText(this, text);
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}

}
