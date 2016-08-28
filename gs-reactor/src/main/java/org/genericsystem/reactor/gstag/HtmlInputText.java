package org.genericsystem.reactor.gstag;

import io.vertx.core.json.JsonObject;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.modelproperties.ActionDefaults;

public class HtmlInputText extends Tag implements ActionDefaults {

	public HtmlInputText(Tag parent) {
		super(parent, "input");
	}

	@Override
	protected HtmlDomNode createNode(HtmlDomNode parent, Context modelContext, Tag tag) {
		return new HtmlDomNode(parent, modelContext, tag) {

			@Override
			public JsonObject fillJson(JsonObject jsonObj) {
				super.fillJson(jsonObj);
				return jsonObj.put("type", "text");
			}

			@Override
			public void handleMessage(JsonObject json) {
				if (ADD.equals(json.getString(MSG_TYPE)))
					((ActionDefaults) getTag()).getAction(getModelContext()).accept(new Object());
				if (UPDATE.equals(json.getString(MSG_TYPE)))
					getTag().getDomNodeAttributes(getModelContext()).put("value", json.getString(TEXT_CONTENT));
			}
		};
	}
}