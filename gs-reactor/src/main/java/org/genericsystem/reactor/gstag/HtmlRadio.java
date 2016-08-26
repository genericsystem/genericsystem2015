package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gs.GSTag;

import io.vertx.core.json.JsonObject;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlRadio extends GSTag {

	public HtmlRadio(GSTag parent) {
		super(parent, "input");
	}

	@Override
	protected HtmlDomNode createNode(String parentId, HtmlDomNode parent, Model modelContext, Tag tag) {
		return new HtmlDomNode(parentId, parent, modelContext, tag) {

			@Override
			public JsonObject fillJson(JsonObject jsonObj) {
				super.fillJson(jsonObj);
				return jsonObj.put("type", "radio");
			}
		};
	}
}
