package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.az.GSTagImpl;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlLabel extends GSTagImpl {

	public HtmlLabel() {
		super("label");
	}

	public HtmlLabel(Tag parent) {
		super(parent, "label");
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
