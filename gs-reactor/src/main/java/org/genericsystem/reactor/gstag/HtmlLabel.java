package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.gs.GSTag;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlLabel extends GSTag {

	public HtmlLabel(GSTag parent) {
		super(parent, "label");
	}

	public static class GSLabelDisplayer extends HtmlLabel {

		public GSLabelDisplayer(GSTag parent) {
			super(parent);
			bindText();
		}
	}
}
