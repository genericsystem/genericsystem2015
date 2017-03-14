package org.genericsystem.kernel;

import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.Container;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Protocol;
import org.genericsystem.common.Root;
import org.genericsystem.common.Statics;
import org.genericsystem.common.Vertex;

/**
 * @author Nicolas Feybesse
 *
 */
public abstract class AbstractServer extends Root implements Generic, Protocol {

	protected Archiver archiver;
	private final GarbageCollector garbageCollector = new GarbageCollector(this);
	private TsGenerator generator = new TsGenerator();

	@Override
	public AbstractServer getRoot() {
		return this;
	}

	@Override
	public abstract AbstractCache newCache();

	@Override
	public void close() {
		super.close();
		archiver.close();
		garbageCollector.stopsScheduler();
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
		protected AbstractServer getRoot() {
			return AbstractServer.this;
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
	public Vertex[] getDependencies(long ts, long id) {
		Generic ancestor = this.getGenericById(id);
		return ancestor != null ? ((RootServerHandler) ancestor.getProxyHandler()).getDependencies().stream(ts).map(generic -> generic.getVertex()).toArray(Vertex[]::new) : Statics.EMPTY;
	}

	@Override
	public void apply(long ts, long[] removeIds, Vertex[] addVertices) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		assert Arrays.stream(addVertices).allMatch(addVertex -> getRoot().getGenericById(addVertex.getTs()) == null);
		assert Arrays.stream(addVertices).allMatch(addVertex -> addVertex.getBirthTs() == Long.MAX_VALUE);
		Snapshot<Generic> removes = new Container(Arrays.stream(removeIds).mapToObj(removeId -> getRoot().getGenericById(removeId)));
		Snapshot<Generic> adds = new Container(Arrays.stream(addVertices).map(addVertex -> getRoot().build(addVertex)));
		try {
			new Transaction(this, ts).apply(removes, adds);
		} catch (ConcurrencyControlException | OptimisticLockConstraintViolationException e) {
			adds.forEach(add -> getRoot().release(add.getTs()));
			throw e;
		}
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
