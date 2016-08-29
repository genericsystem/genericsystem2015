package org.genericsystem.reactor.gstag;

import io.vertx.core.json.JsonObject;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.Tag;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlCheckBox extends Tag {

	public HtmlCheckBox(Tag parent) {
		super(parent, "input");
	}

	@Override
	protected HtmlDomNode createNode(HtmlDomNode parent, Context modelContext) {
		return new HtmlDomNode(parent, modelContext, this) {

			@Override
			public JsonObject fillJson(JsonObject jsonObj) {
				super.fillJson(jsonObj);
				return jsonObj.put("type", "checkbox");
			}

			@Override
			public void handleMessage(JsonObject json) {
				getTag().getDomNodeAttributes(getModelContext()).put(ReactorStatics.CHECKED, json.getBoolean(ReactorStatics.CHECKED) ? ReactorStatics.CHECKED : "");
			}
		};
	}
}
