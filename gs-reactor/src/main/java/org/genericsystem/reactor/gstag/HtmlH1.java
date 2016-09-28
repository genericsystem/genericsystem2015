package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.az.GSTagImpl;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlH1 extends GSTagImpl {

	public HtmlH1(Tag parent) {
		super(parent);
	}

	public HtmlH1(Tag parent, String text) {
		super(parent);
		setText(text);
	}

	@Override
	public String getTag() {
		return "h1";
	}
}
