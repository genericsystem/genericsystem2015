package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.Tag;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlH2 extends Tag {

	public HtmlH2(Tag parent) {
		super(parent, "h2");
	}

	public HtmlH2(Tag parent, String text) {
		super(parent, "h2");
		setText(text);
	}
}
