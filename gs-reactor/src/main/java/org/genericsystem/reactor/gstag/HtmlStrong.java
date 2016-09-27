package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.az.GSTagImpl;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlStrong extends GSTagImpl {

	public HtmlStrong() {
	}

	public HtmlStrong(Tag parent) {
		super(parent);
	}

	@Override
	public String getTag() {
		return "strong";
	}
}
