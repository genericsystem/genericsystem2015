package org.genericsystem.reactor.ba_htmltag;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.aa_modelproperties.ActionDefaults;
import org.genericsystem.reactor.ca_gscomponents.GSTagImpl;

import io.vertx.core.json.JsonObject;

public class HtmlInputText extends GSTagImpl implements ActionDefaults {

	public HtmlInputText() {

	}

	public HtmlInputText(Tag parent) {
		super(parent);
	}

	@Override
	public String getTag() {
		return "input";
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