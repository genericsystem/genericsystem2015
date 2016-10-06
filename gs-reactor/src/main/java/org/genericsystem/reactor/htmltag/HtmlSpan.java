package org.genericsystem.reactor.htmltag;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gscomponents.GSTagImpl;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlSpan extends GSTagImpl {

	public HtmlSpan() {

	}

	public HtmlSpan(Tag parent) {
		super(parent);
	}

	@Override
	public String getTag() {
		return "span";
	}
}
