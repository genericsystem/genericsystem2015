package org.genericsystem.newgui.metacontext;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;

import org.genericsystem.newgui.context.IContext;

public abstract class AbstractMetaContext implements IMetaContext {

	private List<IMetaContext> children = new ArrayList<>();

	@Override
	public IContext buildContext(IContext parent) {
		try {
			return getClazz().getConstructor(IContext.class).newInstance(parent);
		} catch (InstantiationException | IllegalAccessException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
			throw new IllegalStateException();
		}
	}

	public AbstractMetaContext(IMetaContext parent) {
		if (parent != null)
			parent.getChildren().add(this);
	}

	@Override
	public List<IMetaContext> getChildren() {
		return this.children;
	}
}
