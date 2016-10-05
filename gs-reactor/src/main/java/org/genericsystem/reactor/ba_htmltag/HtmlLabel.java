package org.genericsystem.reactor.ba_htmltag;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.ca_gscomponents.GSTagImpl;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlLabel extends GSTagImpl {

	public HtmlLabel() {

	}

	public HtmlLabel(Tag parent) {
		super(parent);
	}

	@Override
	public String getTag() {
		return "label";
	}

	// @Style(propertyName = "width", propertyValue = "100%")
	public static class GSLabelDisplayer extends HtmlLabel {

		public GSLabelDisplayer() {
			bindText();
		}

		public GSLabelDisplayer(Tag parent) {
			super(parent);
			bindText();
		}
	}
}
