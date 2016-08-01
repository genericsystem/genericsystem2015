package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.gs.GSTag;
import org.genericsystem.reactor.model.GenericModel;

/**
 * @author Nicolas Feybesse
 *
 */
public class GSLabel extends GSTag {

	public GSLabel(GSTag parent) {
		super(parent, "label");
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}

	public static class GSLabelDisplayer extends GSLabel {

		public GSLabelDisplayer(GSTag parent) {
			super(parent);
			bindText(GenericModel::getString);
		}
	}
}
