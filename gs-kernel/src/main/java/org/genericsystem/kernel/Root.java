package org.genericsystem.kernel;

import java.io.Serializable;

import org.genericsystem.defaults.DefaultRoot;

public class Root extends AbstractRoot<Generic> implements Generic, DefaultRoot<Generic> {

	private final Archiver archiver;
	private boolean initialized = false;
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
		initialized = true;
	}

	@Override
	public Transaction newCache() {
		return new Transaction(this, pickNewTs());
	}

	@Override
	protected boolean isInitialized() {
		return initialized;
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

	TsDependencies<Generic> getDependencies(Generic generic) {
		TsDependencies<Generic> dependencies = dependenciesMap.get(generic);
		if (dependencies == null) {
			Vertex vertex = getVertex(generic);
			TsDependencies<Generic> already = dependenciesMap.putIfAbsent(generic, dependencies = new AbstractTsDependencies() {
				@Override
				public LifeManager getLifeManager() {
					return vertex.getLifeManager();
				}
			});
			if (already != null)
				dependencies = already;
		}
		return dependencies;
	}

	@Override
	protected Class<Generic> getTClass() {
		return Generic.class;
	}

}
