package org.genericsystem.reactor.htmltag;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gscomponents.GSTagImpl;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlP extends GSTagImpl {

	public HtmlP() {

	}

	public HtmlP(Tag parent) {
		super(parent);
	}

	@Override
	public String getTag() {
		return "p";
	}
}