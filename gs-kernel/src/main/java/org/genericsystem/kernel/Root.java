package org.genericsystem.kernel;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.Cache;
import org.genericsystem.common.Container;
import org.genericsystem.common.IDifferential;
import org.genericsystem.common.Vertex;

public class Root extends AbstractRoot implements Generic, Server {

	protected final Archiver archiver;
	private final GarbageCollector garbageCollector = new GarbageCollector(this);
	private TsGenerator generator = new TsGenerator();;

	// @Override
	// protected void finalize() throws Throwable {
	// System.out.println("FINALIZE ROOT !!!!!!!!");
	// super.finalize();
	// }

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

	public Root(String value, Class<?>... userClasses) {
		this(value, null, userClasses);
	}

	public Root(String value, String persistentDirectoryPath, Class<?>... userClasses) {
		init(this, buildHandler(getClass(), (Generic) this, Collections.emptyList(), value, Collections.emptyList(), ApiStatics.TS_SYSTEM, ApiStatics.SYSTEM_TS));
		startSystemCache(userClasses);
		archiver = new Archiver(this, persistentDirectoryPath);
		isInitialized = true;
	}

	@Override
	public Cache newCache() {
		return new Cache(this) {

			@Override
			protected IDifferential<Generic> buildTransaction() {
				return new Transaction((Root) getRoot());
			}

			@Override
			protected Generic plug(Generic generic) {
				return ((Transaction) getTransaction()).plug(generic);
			}

			@Override
			protected void unplug(Generic generic) {
				((Transaction) getTransaction()).unplug(generic);
			}

		};
	}

	@Override
	public void close() {
		super.close();
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
	protected Generic build(Long ts, Class<?> clazz, Generic meta, List<Generic> supers, Serializable value, List<Generic> components, long birhTs) {
		return build(ts, clazz, meta, supers, value, components, isInitialized() ? new long[] { birhTs, 0L, Long.MAX_VALUE } : ApiStatics.SYSTEM_TS);
	}

	protected Generic build(Long ts, Class<?> clazz, Generic meta, List<Generic> supers, Serializable value, List<Generic> components, long[] otherTs) {
		return init(newT(adaptClass(clazz, meta)), buildHandler(clazz, meta, supers, value, components, ts == null ? pickNewTs() : ts, otherTs));
	}

	protected RootServerHandler buildHandler(Class<?> clazz, Generic meta, List<Generic> supers, Serializable value, List<Generic> components, long ts, long[] otherTs) {
		return new RootServerHandler(clazz, meta, supers, value, components, ts, otherTs);
	}

	class RootServerHandler extends DefaultHandler {

		private final LifeManager lifeManager;
		private final AbstractTsDependencies dependencies;

		private RootServerHandler(Class<?> clazz, Generic meta, List<Generic> supers, Serializable value, List<Generic> components, long ts, long[] otherTs) {
			super(clazz, meta, supers, value, components, ts);
			this.lifeManager = new LifeManager(otherTs);
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

		@Override
		public long getBirthTs() {
			return lifeManager.getBirthTs();
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
		Generic ancestor = this.getGenericById(id);
		return ancestor != null ? ((RootServerHandler) ancestor.getProxyHandler()).getDependencies().stream(ts).mapToLong(generic -> generic.getTs()).toArray() : EMPTY;
	}

	@Override
	public void apply(long ts, long[] removeIds, Vertex[] addVertices) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		// System.out.println("APPLY RECEIVED FOR TS : " + ts);
		Arrays.stream(removeIds).forEach(removeId -> System.out.println("Remove : " + removeId));
		Arrays.stream(addVertices).forEach(addVertex -> System.out.println("Add : " + addVertex.getTs()));
		new Transaction(this, ts).apply(new Container(Arrays.stream(removeIds).mapToObj(removeId -> getRoot().getGenericById(removeId))), new Container(Arrays.stream(addVertices).map(addVertex -> {
			// System.out.println("Commit " + addVertex.getTs() + " " + addVertex.getValue());
				assert getRoot().getGenericById(addVertex.getTs()) == null : "" + addVertex.getTs() + addVertex.getValue() + " " + getGenericById(addVertex.getTs()).info();
				assert addVertex.getBirthTs() == Long.MAX_VALUE;
				return getRoot().build(addVertex);
			})));
	}

	private static class TsGenerator {
		private final long startTime = System.currentTimeMillis() * Statics.MILLI_TO_NANOSECONDS - System.nanoTime();
		private final AtomicLong lastTime = new AtomicLong(0L);

		public long pickNewTs() {
			long nanoTs;
			long current;
			for (;;) {
				nanoTs = startTime + System.nanoTime();
				current = lastTime.get();
				if (nanoTs - current > 0)
					if (lastTime.compareAndSet(current, nanoTs))
						return nanoTs;
			}
		}
	}

}
