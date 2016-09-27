package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.az.GSTagImpl;
import org.genericsystem.reactor.modelproperties.ActionDefaults;

import io.vertx.core.json.JsonObject;

public class HtmlInputText extends GSTagImpl implements ActionDefaults {

	public HtmlInputText() {
		super("input");
	}

	public HtmlInputText(Tag parent) {
		super(parent, "input");
	}

	@Override
	public HtmlDomNode createNode(HtmlDomNode parent, Context modelContext) {
		return new HtmlDomNode(parent, modelContext, this) {

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