package org.genericsystem.common;

import java.io.Serializable;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import javassist.util.proxy.MethodFilter;
import javassist.util.proxy.MethodHandler;
import javassist.util.proxy.ProxyFactory;
import javassist.util.proxy.ProxyObject;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.ISignature;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.defaults.DefaultCache;
import org.genericsystem.defaults.DefaultConfig.MetaAttribute;
import org.genericsystem.defaults.DefaultConfig.MetaRelation;
import org.genericsystem.defaults.DefaultConfig.Sequence;
import org.genericsystem.defaults.DefaultConfig.SystemMap;
import org.genericsystem.defaults.DefaultRoot;
import org.genericsystem.defaults.DefaultVertex;

/**
 * @author Nicolas Feybesse
 *
 */
public abstract class AbstractRoot implements DefaultRoot<Generic>, ProxyObject, Generic {

	protected final Map<Long, Generic> genericsById = new ConcurrentHashMap<>();
	private final SystemCache systemCache = new SystemCache(this);
	protected boolean isInitialized = false;
	protected volatile AbstractCache context;

	public Iterator<Map.Entry<Long, Generic>> genericsByIdIterator() {
		return genericsById.entrySet().iterator();
	}

	@Override
	public AbstractRoot getRoot() {
		return this;
	}

	protected void startSystemCache(Class<?>... userClasses) {
		newCache().start();
		systemCache.mount(Arrays.asList(MetaAttribute.class, MetaRelation.class, SystemMap.class, Sequence.class), userClasses);
		getCurrentCache().flush();
		// shiftContext();
	}

	private MethodHandler handler;

	@Override
	public void setHandler(MethodHandler handler) {
		this.handler = handler;
	}

	@Override
	public MethodHandler getHandler() {
		return handler;
	}

	@Override
	public abstract AbstractCache newCache();

	public static interface Wrapper {

		AbstractCache get();

		void set(AbstractCache context);
	}

	// public class ContextWrapper implements Wrapper {
	//
	//
	//
	// @Override
	// public Cache get() {
	// return context;
	// }
	//
	// @Override
	// public void set(Cache context) {
	// this.context = context;
	//
	// }
	// }

	//
	// protected final Wrapper buildContextWrapper() {
	// return new ContextWrapper();
	// }

	@Override
	public AbstractCache getCurrentCache() {
		// Cache context = contextWrapper.get();
		if (context == null)
			throw new IllegalStateException("Unable to find the current cache. Did you miss to call start() method on it ?");
		return context;
	}

	@SuppressWarnings("unchecked")
	@Override
	public <Custom extends Generic> Custom find(Class<?> clazz) {
		return (Custom) systemCache.find(clazz);
	}

	@SuppressWarnings("unchecked")
	@Override
	public <Custom extends Generic> Custom bind(Class<?> clazz) {
		return (Custom) systemCache.bind(clazz);
	}

	@Override
	public Class<?> findAnnotedClass(Generic generic) {
		return systemCache.getClassByVertex(generic);
	}

	public Generic getGenericById(long ts) {
		return genericsById.get(ts);
	}

	protected void release(long ts) {
		genericsById.remove(ts);
	}

	protected final Generic newT(Class<?> clazz) {
		try {
			return newInstance(clazz);
		} catch (IllegalArgumentException e) {
			getCurrentCache().discardWithException(e);
		}
		return null; // Not reached
	}

	@Override
	public final Generic[] newTArray(int dim) {
		return new Generic[dim];
	}

	protected Generic build(Vertex vertex) {
		return build(vertex.getTs(), vertex.getClazz(), vertex.getMeta() == vertex.getTs() ? null : getGenericById(vertex.getMeta()), vertex.getSupers().stream().map(this::getGenericById).collect(Collectors.toList()), vertex.getValue(), vertex
				.getComponents().stream().map(this::getGenericById).collect(Collectors.toList()), vertex.getBirthTs());
	}

	protected Class<?> adaptClass(Class<?> clazz, Generic meta) {
		InstanceClass metaAnnotation = meta == null ? null : getAnnotedClass(meta).getAnnotation(InstanceClass.class);
		if (metaAnnotation != null)
			if (clazz == null || clazz.isAssignableFrom(metaAnnotation.value()))
				clazz = metaAnnotation.value();
			else if (!metaAnnotation.value().isAssignableFrom(clazz))
				getCurrentCache().discardWithException(new InstantiationException(clazz + " must extends " + metaAnnotation.value()));
		if (clazz == null || !getTClass().isAssignableFrom(clazz))
			clazz = getTClass();
		return clazz;
	}

	public abstract long pickNewTs();

	Generic build(Long ts, Class<?> clazz, Generic meta, List<Generic> supers, Serializable value, List<Generic> components) {
		return build(ts, clazz, meta, supers, value, components, isInitialized() ? Long.MAX_VALUE : ApiStatics.TS_SYSTEM);
	}

	protected abstract Generic build(Long ts, Class<?> clazz, Generic meta, List<Generic> supers, Serializable value, List<Generic> components, long birhTs);

	protected Generic init(Generic generic, DefaultHandler handler) {
		((ProxyObject) generic).setHandler(handler);
		assert ((ProxyObject) generic).getHandler() instanceof AbstractRoot.DefaultHandler;
		Generic gresult = genericsById.putIfAbsent(handler.getTs(), generic);
		assert gresult == null : gresult.info();
		return generic;
	}

	protected abstract Class<Generic> getTClass();

	private final static ProxyFactory PROXY_FACTORY = new ProxyFactory();
	private final static MethodFilter METHOD_FILTER = method -> method.getName().equals("toString");

	private Generic newInstance(Class<?> clazz) {
		PROXY_FACTORY.setSuperclass(clazz.isInterface() ? Object.class : clazz);
		PROXY_FACTORY.setInterfaces(clazz.isInterface() ? getTClass().isAssignableFrom(clazz) ? new Class[] { clazz } : new Class[] { clazz, getTClass() } : getTClass().isAssignableFrom(clazz) ? new Class[] {} : new Class[] { getTClass() });
		try {
			return (Generic) PROXY_FACTORY.createClass(METHOD_FILTER).newInstance();
		} catch (InstantiationException | IllegalAccessException e) {
			throw new IllegalStateException(e);
		}
	}

	public Class<?> getAnnotedClass(Generic generic) {
		if (generic.isSystem()) {
			Class<?> annotedClass = findAnnotedClass(generic);
			if (annotedClass != null)
				return annotedClass;
		}
		if (!generic.isRoot() && generic instanceof ProxyObject)
			return generic.getClass().getSuperclass();
		return generic.getClass();
	}

	public boolean isInitialized() {
		return isInitialized;
	}

	public abstract class DefaultHandler implements MethodHandler, ISignature<Generic> {

		private final Class<?> clazz;
		private final Generic meta;
		private final List<Generic> supers;
		private final Serializable value;
		private final List<Generic> components;
		private final long ts;

		protected DefaultHandler(Class<?> clazz, Generic meta, List<Generic> supers, Serializable value, List<Generic> components, long ts) {
			this.clazz = clazz;
			this.meta = meta;
			this.supers = supers;
			this.value = value;
			this.components = components;
			this.ts = ts;
		}

		@Override
		public Object invoke(Object self, Method m, Method proceed, Object[] args) throws Throwable {
			return ((DefaultVertex<?>) self).defaultToString();
		}

		abstract protected AbstractRoot getRoot();

		public Vertex getVertex() {
			return new Vertex(getClazz(), getTs(), getMeta() != null ? getMeta().getTs() : getTs(), getSupers().stream().map(Generic::getTs).collect(Collectors.toList()), getValue(), getComponents().stream().map(Generic::getTs)
					.collect(Collectors.toList()), getBirthTs());
		}

		public Class<?> getClazz() {
			return clazz;
		}

		@Override
		public Generic getMeta() {
			return meta;
		}

		@Override
		public List<Generic> getSupers() {
			return supers;
		}

		@Override
		public Serializable getValue() {
			return value;
		}

		@Override
		public List<Generic> getComponents() {
			return components;
		}

		@Override
		public long getTs() {
			return ts;
		}

	};

	protected AbstractCache start(AbstractCache context) {
		this.context = context;
		return context;
	}

	protected void stop(DefaultCache<Generic> context) {
		assert this.context == context;
		context = null;
	}

	@Override
	public void close() {
		context = null;
	}

	@Override
	public String toString() {
		return defaultToString();
	}

}
