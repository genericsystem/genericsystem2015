package org.genericsystem.newgui.metacontext;

import java.util.ArrayList;
import java.util.List;

public abstract class AbstractMetaContext implements IMetaContext {

	private List<IMetaContext> children = new ArrayList<>();

	public AbstractMetaContext(IMetaContext parent) {
		if (parent != null)
			parent.getChildren().add(this);
	}

	@Override
	public List<IMetaContext> getChildren() {
		return this.children;
	}
}
