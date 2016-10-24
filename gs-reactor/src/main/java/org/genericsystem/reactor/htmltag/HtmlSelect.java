package org.genericsystem.reactor.htmltag;

import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gscomponents.GSTagImpl;

import io.vertx.core.json.JsonObject;

public class HtmlSelect extends GSTagImpl {

	public HtmlSelect() {
	}

	public HtmlSelect(Tag parent) {
		super(parent);
	}

	@Override
	public String getTag() {
		return "select";
	}

	@Override
	public HtmlDomNode createNode(HtmlDomNode parent, Context modelContext) {
		return new HtmlDomNode(parent, modelContext, this) {

			@Override
			public void handleMessage(JsonObject json) {
				if (UPDATE.equals(json.getString(MSG_TYPE))) {
					((SelectionDefaults) getTag()).getSelectionIndex(getModelContext()).setValue(json.getInteger(SELECTED_INDEX));
				}
			}
		};
	}
}
