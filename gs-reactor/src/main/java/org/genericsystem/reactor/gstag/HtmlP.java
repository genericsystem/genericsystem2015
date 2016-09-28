package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.az.GSTagImpl;

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