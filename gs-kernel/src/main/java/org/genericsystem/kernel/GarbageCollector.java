package org.genericsystem.kernel;

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.genericsystem.common.Generic;
import org.genericsystem.kernel.AbstractServer.RootServerHandler;

/**
 * @author Nicolas Feybesse
 *
 */
public class GarbageCollector extends LinkedHashSet<Generic> {

	private static final long serialVersionUID = -2021341943811568201L;
	private final ScheduledExecutorService scheduler = Executors.newSingleThreadScheduledExecutor();
	private final AbstractServer root;

	public GarbageCollector(AbstractServer root) {
		this.root = root;
	}

	public void startScheduler() {
		scheduler.scheduleAtFixedRate(() -> runGarbage(Statics.LIFE_TIMEOUT), Statics.GARBAGE_INITIAL_DELAY, Statics.GARBAGE_PERIOD, TimeUnit.MILLISECONDS);
	}

	public void runGarbage(long timeOut) {
		long ts = root.pickNewTs();
		synchronized (root) {
			Iterator<Generic> iterator = GarbageCollector.this.iterator();
			while (iterator.hasNext()) {
				Generic generic = iterator.next();
				if (ts - ((RootServerHandler) generic.getProxyHandler()).getLifeManager().getDeathTs() >= timeOut) {
					generic.remove();
					iterator.remove();
				}
			}
		}
	}

	public void stopsScheduler() {
		scheduler.shutdown();
	}
}
