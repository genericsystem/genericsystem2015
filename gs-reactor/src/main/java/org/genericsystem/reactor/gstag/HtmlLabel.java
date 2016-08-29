package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.Tag;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlLabel extends Tag {

	public HtmlLabel(Tag parent) {
		super(parent, "label");
	}

	public static class GSLabelDisplayer extends HtmlLabel {

		public GSLabelDisplayer(Tag parent) {
			super(parent);
			bindText();
		}
	}
}
