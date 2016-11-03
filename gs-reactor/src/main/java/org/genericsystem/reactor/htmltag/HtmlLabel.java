package org.genericsystem.reactor.htmltag;

import org.genericsystem.reactor.gscomponents.GSTagImpl;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlLabel extends GSTagImpl {

	@Override
	public String getTag() {
		return "label";
	}

	// @Style(propertyName = "width", propertyValue = "100%")
	public static class GSLabelDisplayer extends HtmlLabel {

		public GSLabelDisplayer() {
			bindText();
		}
	}
}
