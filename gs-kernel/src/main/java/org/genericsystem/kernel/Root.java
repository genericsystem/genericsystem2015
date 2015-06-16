package org.genericsystem.kernel;

import java.io.Serializable;

import javassist.util.proxy.MethodHandler;

import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.TsDependencies;
import org.genericsystem.common.Vertex;

public class Root extends AbstractRoot<Generic> implements Generic {

	private final Archiver archiver;
	private final GarbageCollector garbageCollector = new GarbageCollector(this);
	private TsDependencies<Generic> dependencies;
	private LifeManager lifeManager;

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
	protected void initSubRoot(Serializable value, String persistentDirectoryPath, Class<?>... userClasses) {
		lifeManager = new LifeManager(vertex.getOtherTs());
		dependencies = new AbstractTsDependencies() {
			@Override
			public LifeManager getLifeManager() {
				return lifeManager;
			}
		};
	};

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
	protected MethodHandler buildHandler(Vertex vertex) {
		return new RootWrapper(vertex);
	}

	class RootWrapper extends AbstractRootWrapper {

		private final LifeManager lifeManager;
		private final AbstractTsDependencies dependencies;

		private RootWrapper(Vertex vertex) {
			super(vertex);
			this.lifeManager = new LifeManager(vertex.getOtherTs());
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

	@Override
	public LifeManager getLifeManager() {
		return lifeManager;
	}

	@Override
	public TsDependencies<Generic> getDependencies() {
		return dependencies;
	}
}
