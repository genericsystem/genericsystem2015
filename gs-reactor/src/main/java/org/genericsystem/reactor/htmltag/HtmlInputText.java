package org.genericsystem.reactor.htmltag;

import org.genericsystem.reactor.modelproperties.ActionDefaults;

import java.util.function.Consumer;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.gscomponents.TagImpl;

import io.vertx.core.json.JsonObject;
import javafx.beans.property.Property;

public class HtmlInputText extends TagImpl implements ActionDefaults {

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
				if (ADD.equals(json.getString(MSG_TYPE))) {
					Property<Consumer<Object>> action = ((ActionDefaults) getTag()).getActionProperty(getModelContext());
					if (action != null)
						action.getValue().accept(new Object());
				}
				if (UPDATE.equals(json.getString(MSG_TYPE)))
					getTag().getDomNodeAttributes(getModelContext()).put("value", json.getString(TEXT_CONTENT));
			}
		};
	}
}