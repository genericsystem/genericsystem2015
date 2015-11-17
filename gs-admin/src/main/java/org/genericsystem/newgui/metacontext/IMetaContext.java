package org.genericsystem.newgui.metacontext;

import java.util.List;

import org.genericsystem.newgui.context.IContext;

public interface IMetaContext {
	public IContext apply();

	public List<IMetaContext> getChildren();

	public static class TableViewMetaContext extends AbstractMetaContext {
		public TableViewMetaContext(IMetaContext parent) {
			super(parent);
		}

		@Override
		public IContext apply() {
			return null;
		}
	}

	public static class RootMetaContext extends AbstractMetaContext {
		public RootMetaContext(IMetaContext parent) {
			super(parent);
		}

		@Override
		public IContext apply() {
			return null;
		}
	}
}
