package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.TagImpl;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlLabel extends TagImpl {

	public HtmlLabel() {
		super("label");
	}

	public HtmlLabel(Tag parent) {
		super(parent, "label");
	}

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
