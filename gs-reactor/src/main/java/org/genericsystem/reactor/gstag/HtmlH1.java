package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gs.GSTagImpl;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlH1 extends GSTagImpl {

	public HtmlH1(Tag parent) {
		super(parent, "h1");
	}

	public HtmlH1(Tag parent, String text) {
		super(parent, "h1");
		setText(text);
	}
}
