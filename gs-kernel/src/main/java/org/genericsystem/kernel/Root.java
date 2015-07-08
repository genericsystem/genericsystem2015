package org.genericsystem.kernel;

import java.io.Serializable;
import java.util.List;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.AbstractContext;
import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.Vertex;

public class Root extends AbstractRoot<Generic> implements Generic, Server {

	private final Archiver archiver;
	private final GarbageCollector garbageCollector = new GarbageCollector(this);
	private TsGenerator generator;

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

	@Override
	protected void initSubRoot(Serializable value, String persistentDirectoryPath, java.lang.Class<?>... userClasses) {
		generator = new TsGenerator();
	};

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

	@Override
	protected Generic build(Vertex vertex) {
		return super.build(vertex);
	}

	@Override
	protected RootWrapped buildHandler(Class<?> clazz, Generic meta, List<Generic> supers, Serializable value, List<Generic> components, long ts, long[] otherTs) {
		return new RootWrapped(clazz, meta, supers, value, components, ts, otherTs);
	}

	class RootWrapped extends Wrapped {

		private final LifeManager lifeManager;
		private final AbstractTsDependencies dependencies;

		private RootWrapped(Class<?> clazz, Generic meta, List<Generic> supers, Serializable value, List<Generic> components, long ts, long[] otherTs) {
			super(clazz, meta, supers, value, components, ts, otherTs);
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

	@Override
	public long pickNewTs() {
		return generator.pickNewTs();
	}

	@Override
	public Vertex getVertex(long id) {
		return this.getGenericById(id).getVertex();
	}

	@Override
	public long[] getDependencies(long ts, long id) {
		Generic genericById = this.getGenericById(id);
		return genericById != null ? genericById.getProxyHandler().getDependencies().stream(ts).mapToLong(generic -> generic.getTs()).toArray() : EMPTY;
	}

	@Override
	public void apply(long ts, long[] removes, Vertex[] adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		new Transaction(this, ts).remoteApply(removes, adds);
	}

}
