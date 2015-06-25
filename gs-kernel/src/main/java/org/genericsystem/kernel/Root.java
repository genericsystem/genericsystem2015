package org.genericsystem.kernel;

import io.vertx.example.util.ExampleRunner;
import java.io.Serializable;
import java.util.List;
import org.genericsystem.common.AbstractContext;
import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.Vertex;

public class Root extends AbstractRoot<Generic> implements Generic {

	private final Archiver archiver;
	private final GarbageCollector garbageCollector = new GarbageCollector(this);

	@Override
	public Root getRoot() {
		return this;
	}

	public Root() {
		this(new Class[] {});
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
	public AbstractContext<Generic> newCache() {
		return new Transaction(this, pickNewTs());
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
	protected RootWrapped buildHandler(Generic meta, List<Generic> supers, Serializable value, List<Generic> components, long ts, long[] otherTs) {
		return new RootWrapped(meta, supers, value, components, ts, otherTs);
	}

	class RootWrapped extends Wrapped {

		private final LifeManager lifeManager;
		private final AbstractTsDependencies dependencies;

		private RootWrapped(Generic meta, List<Generic> supers, Serializable value, List<Generic> components, long ts, long[] otherTs) {
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

	public static void main(String[] args) {
		ExampleRunner.runJavaExample("gs-kernel/src/main/java/", Root.class, true);
	}

}
