package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.TagImpl;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlH2 extends TagImpl {

	public HtmlH2() {
		super("h2");
	}

	public HtmlH2(Tag parent) {
		super(parent, "h2");
	}

	public HtmlH2(Tag parent, String text) {
		super(parent, "h2");
		setText(text);
	}
}
