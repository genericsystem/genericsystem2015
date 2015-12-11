package org.genericsystem.distributed.cacheonclient;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.HeavyCache.ContextEventListener;
import org.genericsystem.common.Vertex;
import org.genericsystem.kernel.Statics;

public class CocClientEngine extends AbstractRoot implements Generic {

	protected final CocClient server;

	public CocClientEngine(Class<?>... userClasses) {
		this(Statics.ENGINE_VALUE, null, Statics.DEFAULT_PORT, userClasses);
	}

	public CocClientEngine(String engineValue, Class<?>... userClasses) {
		this(engineValue, null, Statics.DEFAULT_PORT, userClasses);
	}

	public CocClientEngine(String engineValue, String host, int port, Class<?>... userClasses) {
		this(engineValue, host, port, null, userClasses);
	}

	public CocClientEngine(String engineValue, String host, int port, String persistentDirectoryPath, Class<?>... userClasses) {
		init(this, buildHandler(getClass(), (Generic) this, Collections.emptyList(), engineValue, Collections.emptyList(), ApiStatics.TS_SYSTEM, ApiStatics.TS_SYSTEM));
		server = new CocClient(host, port, "/" + engineValue);
		startSystemCache(userClasses);
		isInitialized = true;
	}

	@Override
	public CocClientEngine getRoot() {
		return this;
	}

	@Override
	public CocCache newCache() {
		return new CocCache(this);
	}

	public CocCache newCache(ContextEventListener<Generic> listener) {
		return new CocCache(this, listener);
	}

	@Override
	public CocCache getCurrentCache() {
		return (CocCache) super.getCurrentCache();
	}

	public Generic getGenericByVertex(Vertex vertex) {
		Generic generic = super.getGenericById(vertex.getTs());
		if (generic == null) {
			generic = build(vertex);
		}
		return generic;
	}

	@Override
	public Generic getGenericById(long ts) {
		Generic generic = super.getGenericById(ts);
		if (generic == null) {
			Vertex vertex = server.getVertex(ts);
			generic = build(vertex);
		}
		return generic;
	}

	@Override
	protected void finalize() throws Throwable {
		server.close();
		super.finalize();
	}

	@Override
	public void close() {
		server.close();
		super.close();
	}

	@Override
	protected Class<Generic> getTClass() {
		return Generic.class;
	}

	public CocClient getServer() {
		return server;
	}

	@Override
	protected Generic build(Long ts, Class<?> clazz, Generic meta, List<Generic> supers, Serializable value, List<Generic> components, long birthTs) {
		return init(newT(adaptClass(clazz, meta)), buildHandler(clazz, meta, supers, value, components, ts == null ? pickNewTs() : ts, birthTs));
	}

	ClientEngineHandler buildHandler(Class<?> clazz, Generic meta, List<Generic> supers, Serializable value, List<Generic> components, long ts, long birthTs) {
		return new ClientEngineHandler(clazz, meta, supers, value, components, ts, birthTs);
	}

	class ClientEngineHandler extends DefaultHandler {

		long birthTs;

		public ClientEngineHandler(Class<?> clazz, Generic meta, List<Generic> supers, Serializable value, List<Generic> components, long ts, long birthTs) {
			super(clazz, meta, supers, value, components, ts);
			this.birthTs = birthTs;
		}

		@Override
		protected CocClientEngine getRoot() {
			return CocClientEngine.this;
		}

		@Override
		public long getBirthTs() {
			return birthTs;
		}
	}

	@Override
	public long pickNewTs() {
		return server.pickNewTs();
	}

	// ____/Use async gs-default methods\_____________

	@Override
	public boolean isAlive() {
		try {
			return isAsyncAlive().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	@Override
	public Generic getInstance(Serializable value, Generic... components) {
		try {
			return getAsyncInstance(value, components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	@Override
	public Generic getInstance(Generic... components) {
		try {
			return getAsyncInstance(components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	// // use computeAsyncAndCheckOverridesAreReached
	// @Override
	// public Generic getInstance(Generic override, Serializable value, Generic... components) {
	// try {
	// System.out.println("test-----------");
	// return getAsyncInstance(override, value, components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	// } catch (InterruptedException | ExecutionException | TimeoutException e) {
	// e.printStackTrace();
	// throw new RuntimeException();
	// }
	// }
	//
	// // use computeAsyncAndCheckOverridesAreReached
	// @Override
	// public Generic getInstance(List<Generic> overrides, Serializable value, Generic... components) {
	// try {
	// return getAsyncInstance(overrides, value, components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	// } catch (InterruptedException | ExecutionException | TimeoutException e) {
	// e.printStackTrace();
	// throw new RuntimeException();
	// }
	// }

	@Override
	public Snapshot<Generic> getInstances(Serializable value, Generic... components) {
		try {
			return getAsyncInstances(value, components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	@Override
	public Snapshot<Generic> getInstances() {
		try {
			return getAsyncInstances().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	@Override
	public Snapshot<Generic> getInstances(Generic... components) {
		try {
			return getAsyncInstances(components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	// @Override
	// // use computeAsyncAndCheckOverridesAreReached : unimplemented
	// public Snapshot<Generic> getInstances(Generic override, Serializable value, Generic... components) {
	// try {
	// return getAsyncInstances(override, value, components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	// } catch (InterruptedException | ExecutionException | TimeoutException e) {
	// e.printStackTrace();
	// throw new RuntimeException();
	// }
	// }
	//
	// @Override
	// // use computeAsyncAndCheckOverridesAreReached : unimplemented
	// public Snapshot<Generic> getInstances(List<Generic> overrides, Serializable value, Generic... components) {
	// try {
	// return getAsyncInstances(overrides, value, components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	// } catch (InterruptedException | ExecutionException | TimeoutException e) {
	// e.printStackTrace();
	// throw new RuntimeException();
	// }
	// }

	@Override
	public Generic getSubInstance(Serializable value, Generic... components) { // TODO never used
		try {
			System.out.println("_4_________________________________________");
			return getAsyncSubInstance(value, components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	@Override
	public Snapshot<Generic> getSubInstances(Serializable value, Generic... components) {
		try {
			return getAsyncSubInstances(value, components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	@Override
	public Snapshot<Generic> getSubInstances() {
		try {
			return getAsyncSubInstances().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	@Override
	public Generic getSubInstance(Generic... components) { // TODO never used
		try {
			System.out.println("_7_________________________________________");
			return getAsyncSubInstance(components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	@Override
	public Snapshot<Generic> getSubInstances(Generic... components) {
		try {
			return getAsyncSubInstances(components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	// @Override
	// // use computeAsyncAndCheckOverridesAreReached : unimplemented
	// public Generic getSubInstance(Generic override, Serializable value, Generic... components) {
	// try {
	// System.out.println("_9_________________________________________");
	// return getAsyncSubInstance(override, value, components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	// } catch (InterruptedException | ExecutionException | TimeoutException e) {
	// e.printStackTrace();
	// throw new RuntimeException();
	// }
	// }
	//
	// @Override
	// // use computeAsyncAndCheckOverridesAreReached : unimplemented
	// public Snapshot<Generic> getSubInstances(Generic override, Serializable value, Generic... components) {
	// try {
	// System.out.println("_10_________________________________________");
	// return getAsyncSubInstances(override, value, components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	// } catch (InterruptedException | ExecutionException | TimeoutException e) {
	// e.printStackTrace();
	// throw new RuntimeException();
	// }
	// }

	@Override
	public Generic getSubInstance(List<Generic> overrides, Serializable value, Generic... components) { // TODO never used
		try {
			System.out.println("_11_________________________________________");
			return getAsyncSubInstance(overrides, value, components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	// @Override
	// // use computeAsyncAndCheckOverridesAreReached : unimplemented
	// public Snapshot<Generic> getSubInstances(List<Generic> overrides, Serializable value, Generic... components) {
	// try {
	// System.out.println("_12_________________________________________");
	// return getAsyncSubInstances(overrides, value, components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	// } catch (InterruptedException | ExecutionException | TimeoutException e) {
	// e.printStackTrace();
	// throw new RuntimeException();
	// }
	// }

	@Override
	public Generic getInheriting(Serializable value, Generic... components) {
		try {
			return getAsyncInheriting(value, components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	@Override
	public Generic getInheriting(Generic... components) {
		try {
			return getAsyncInheriting(components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	@Override
	public Snapshot<Generic> getInheritings(Serializable value, Generic... components) {
		try {
			return getAsyncInheritings(value, components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	@Override
	public Snapshot<Generic> getInheritings() {
		try {
			return getAsyncInheritings().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	@Override
	public Snapshot<Generic> getInheritings(Generic... components) {
		try {
			return getAsyncInheritings(components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	@Override
	public Generic getSubInheriting(Serializable value, Generic... components) { // TODO never used
		try {
			System.out.println("_18_________________________________________");
			return getAsyncSubInheriting(value, components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	@Override
	public Snapshot<Generic> getSubInheritings(Serializable value, Generic... components) {// TODO ERROR_____________________________________
		try {
			System.out.println("_19_________________________________________");
			return getAsyncSubInheritings(value, components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	@Override
	public Generic getSubInheriting(Generic... components) { // TODO never used
		try {
			System.out.println("_20_________________________________________");
			return getAsyncSubInheriting(components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	@Override
	public Snapshot<Generic> getSubInheritings(Generic... components) {
		try {
			return getAsyncSubInheritings(components).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	@Override
	public Snapshot<Generic> getSubInheritings() {
		try {
			return getAsyncSubInheritings().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	@Override
	public Generic getComposite(Serializable value) {
		try {
			return getAsyncComposite(value).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	@Override
	public Snapshot<Generic> getComposites(Serializable value) {
		try {
			return getAsyncComposites(value).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	@Override
	public Snapshot<Generic> getComposites() {
		try {
			return getAsyncComposites().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new RuntimeException();
		}
	}

	// Override DefaultCompositesInheritance

	// @SuppressWarnings("unchecked")
	// @Override
	// public Snapshot<Generic> getAttributes(Generic attribute) {
	// try {
	// return getAsyncAttributes(attribute).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	// } catch (InterruptedException | ExecutionException | TimeoutException e) {
	// e.printStackTrace();
	// throw new IllegalStateException(e);
	// }
	// }

	// @Override
	// public Snapshot<Generic> getHolders(Generic attribute) {
	// try {
	// return getAsyncHolders(attribute).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	// } catch (InterruptedException | ExecutionException | TimeoutException e) {
	// e.printStackTrace();
	// throw new IllegalStateException(e);
	// }
	// }

	@Override
	public Generic getKey(Class<? extends SystemProperty> propertyClass, int pos) {
		try {
			return getAsyncKey(propertyClass, pos).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		} catch (InterruptedException | ExecutionException | TimeoutException e) {
			e.printStackTrace();
			throw new IllegalStateException(e);
		}
	}

	// @Override
	// public Snapshot<Generic> getAttributes(Generic attribute) {
	// try {
	// System.out.println("'''''''");
	// return getAsyncAttributes(attribute).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	// } catch (InterruptedException | ExecutionException | TimeoutException e) {
	// e.printStackTrace();
	// throw new IllegalStateException(e);
	// }
	// }
}
