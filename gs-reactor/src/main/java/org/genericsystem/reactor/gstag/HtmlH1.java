package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.Tag;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlH1 extends Tag {

	public HtmlH1(Tag parent) {
		super(parent, "h1");
	}

	public HtmlH1(Tag parent, String text) {
		super(parent, "h1");
		setText(text);
	}
}
