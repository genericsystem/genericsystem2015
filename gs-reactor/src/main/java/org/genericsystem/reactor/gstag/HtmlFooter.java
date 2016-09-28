package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.az.GSTagImpl;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlFooter extends GSTagImpl {

	public HtmlFooter() {
	}

	public HtmlFooter(Tag parent) {
		super(parent);
	}

	@Override
	public String getTag() {
		return "footer";
	}
}
