package org.genericsystem.reactor.htmltag;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gscomponents.GSTagImpl;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlDiv extends GSTagImpl {

	public HtmlDiv() {

	}

	public HtmlDiv(Tag parent) {
		super(parent);
	}

	@Override
	public String getTag() {
		return "div";
	}
}
