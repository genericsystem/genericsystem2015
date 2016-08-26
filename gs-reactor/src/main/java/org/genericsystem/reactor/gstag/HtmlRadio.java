package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.HtmlDomNode;
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
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId) {

			@Override
			public JsonObject fillJson(JsonObject jsonObj) {
				super.fillJson(jsonObj);
				return jsonObj.put("type", "radio");
			}
		};
	}
}
