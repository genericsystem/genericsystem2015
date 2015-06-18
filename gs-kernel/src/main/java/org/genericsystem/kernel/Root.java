package org.genericsystem.kernel;

import java.io.Serializable;
import java.util.List;

import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.Vertex;

public class Root extends AbstractRoot<Generic> implements Generic {

	private final Archiver archiver;
	private final GarbageCollector garbageCollector = new GarbageCollector(this);

	@Override
	public Root getRoot() {
		return this;
	}

	public Root(Class<?>... userClasses) {
		this(Statics.ENGINE_VALUE, userClasses);
	}

	public Root(Serializable value, Class<?>... userClasses) {
		this(value, null, userClasses);
	}

	public Root(Serializable value, String persistentDirectoryPath, Class<?>... userClasses) {
		super(value, persistentDirectoryPath, userClasses);
		archiver = new Archiver(this, persistentDirectoryPath);
		isInitialized = true;
	}

	@Override
	public Transaction newCache() {
		return new Transaction(this, pickNewTs());
	}

	@Override
	public Transaction getCurrentCache() {
		return (Transaction) super.getCurrentCache();
	}

	@Override
	public void close() {
		archiver.close();
		garbageCollector.stopsScheduler();
	}

	@Override
	public final Generic[] newTArray(int dim) {
		return new Generic[dim];
	}

	GarbageCollector getGarbageCollector() {
		return garbageCollector;
	}

	@Override
	protected Class<Generic> getTClass() {
		return Generic.class;
	}

	protected Generic init(Vertex vertex) {
		return super.init(null, vertex);
	}

	@Override
	protected RootWrapper buildHandler(Generic generic, Vertex vertex) {
		return new RootWrapper(generic, vertex);
	}

	@Override
	protected RootWrapper buildHandler(Generic meta, List<Generic> supers, Serializable value, List<Generic> components, long ts, long[] otherTs) {
		return new RootWrapper(meta, supers, value, components, ts, otherTs);
	}

	class RootWrapper extends AbstractRootWrapper {

		private final LifeManager lifeManager;
		private final AbstractTsDependencies dependencies;

		private RootWrapper(Generic generic, Vertex vertex) {
			super(generic, vertex);
			this.lifeManager = new LifeManager(vertex.getOtherTs());
			this.dependencies = new AbstractTsDependencies() {
				@Override
				public LifeManager getLifeManager() {
					return lifeManager;
				}
			};
		}

		private RootWrapper(Generic meta, List<Generic> supers, Serializable value, List<Generic> components, long ts, long[] otherTs) {
			super(meta, supers, value, components, ts, otherTs);
			this.lifeManager = new LifeManager(getOtherTs());
			this.dependencies = new AbstractTsDependencies() {
				@Override
				public LifeManager getLifeManager() {
					return lifeManager;
				}
			};
		}

		LifeManager getLifeManager() {
			return lifeManager;
		}

		AbstractTsDependencies getDependencies() {
			return dependencies;
		}

		@Override
		protected Root getRoot() {
			return Root.this;
		}
	}
}
