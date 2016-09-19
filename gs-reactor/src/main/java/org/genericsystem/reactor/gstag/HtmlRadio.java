package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gs.GSTagImpl;

import io.vertx.core.json.JsonObject;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlRadio extends GSTagImpl {

	public HtmlRadio(Tag parent) {
		super(parent, "input");
	}

	@Override
	public HtmlDomNode createNode(HtmlDomNode parent, Context modelContext) {
		return new HtmlDomNode(parent, modelContext, this) {

			@Override
			public JsonObject fillJson(JsonObject jsonObj) {
				super.fillJson(jsonObj);
				return jsonObj.put("type", "radio");
			}
		};
	}
}
