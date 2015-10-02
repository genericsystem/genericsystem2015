package org.genericsystem.kernel;

import java.io.Serializable;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;
import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.common.AbstractEngine;
import org.genericsystem.common.Generic;
import org.genericsystem.common.HeavyCache;
import org.genericsystem.common.IDifferential;
import org.genericsystem.common.Protocole;
import org.genericsystem.common.Vertex;

public abstract class AbstractServer extends AbstractEngine implements Generic, Protocole {

	protected Archiver archiver;
	private final GarbageCollector garbageCollector = new GarbageCollector(this);
	private TsGenerator generator = new TsGenerator();;

	@Override
	public AbstractServer getRoot() {
		return this;
	}

	@Override
	public HeavyCache newCache() {
		return new HeavyCache(this) {

			@Override
			protected IDifferential<Generic> buildTransaction() {
				return new Transaction((LightServerEngine) getRoot());
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
