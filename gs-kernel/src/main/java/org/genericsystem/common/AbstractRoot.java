package org.genericsystem.common;

import java.io.Serializable;
import java.lang.reflect.Method;
import java.util.Arrays;
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
import org.genericsystem.defaults.DefaultConfig.MetaAttribute;
import org.genericsystem.defaults.DefaultConfig.MetaRelation;
import org.genericsystem.defaults.DefaultConfig.Sequence;
import org.genericsystem.defaults.DefaultConfig.SystemMap;
import org.genericsystem.defaults.DefaultRoot;
import org.genericsystem.defaults.DefaultVertex;

public abstract class AbstractRoot<T extends DefaultVertex<T>> implements DefaultRoot<T>, GenericProxy<T>, ProxyObject {

	private final Map<Long, T> tMap = new ConcurrentHashMap<>();
	protected Wrapper<T> contextWrapper = buildContextWrapper();
	private final SystemCache<T> systemCache = new SystemCache<>(this);
	protected boolean isInitialized = false;

	@Override
	public AbstractRoot<T> getRoot() {
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
	public abstract AbstractContext<T> newCache();

	public static interface Wrapper<T extends DefaultVertex<T>> {

		AbstractContext<T> get();

		void set(AbstractContext<T> context);
	}

	public class ContextWrapper implements Wrapper<T> {

		private AbstractContext<T> context;

		@Override
		public AbstractContext<T> get() {
			return context;
		}

		@Override
		public void set(AbstractContext<T> context) {
			this.context = context;

		}
	}

	protected Wrapper<T> buildContextWrapper() {
		return new ContextWrapper();
	}

	@Override
	public AbstractContext<T> getCurrentCache() {
		AbstractContext<T> context = contextWrapper.get();
		if (context == null)
			throw new IllegalStateException("Unable to find the current cache. Did you miss to call start() method on it ?");
		return context;
	}

	@SuppressWarnings("unchecked")
	@Override
	public <Custom extends T> Custom find(Class<?> clazz) {
		return (Custom) systemCache.find(clazz);
	}

	@SuppressWarnings("unchecked")
	@Override
	public <Custom extends T> Custom bind(Class<?> clazz) {
		return (Custom) systemCache.bind(clazz);
	}

	@Override
	public Class<?> findAnnotedClass(T generic) {
		return systemCache.getClassByVertex(generic);
	}

	public T getGenericById(long ts) {
		return tMap.get(ts);
	}

	protected T newT(Class<?> clazz) {
		try {
			return newInstance(clazz);
		} catch (IllegalArgumentException e) {
			getCurrentCache().discardWithException(e);
		}
		return null; // Not reached
	}

	protected T build(Vertex vertex) {
		return build(vertex.getTs(), vertex.getClazz(), vertex.getMeta() == vertex.getTs() ? null : getGenericById(vertex.getMeta()), vertex.getSupers().stream().map(this::getGenericById).collect(Collectors.toList()), vertex.getValue(), vertex
				.getComponents().stream().map(this::getGenericById).collect(Collectors.toList()), vertex.getBirthTs());
	}

	protected Class<?> adaptClass(Class<?> clazz, T meta) {
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

	T build(Long ts, Class<?> clazz, T meta, List<T> supers, Serializable value, List<T> components) {
		return build(ts, clazz, meta, supers, value, components, isInitialized() ? Long.MAX_VALUE : ApiStatics.TS_SYSTEM);
	}

	protected abstract T build(Long ts, Class<?> clazz, T meta, List<T> supers, Serializable value, List<T> components, long birhTs);

	protected T init(T generic, DefaultHandler handler) {
		((ProxyObject) generic).setHandler(handler);
		assert ((ProxyObject) generic).getHandler() instanceof AbstractRoot.DefaultHandler;
		T gresult = tMap.putIfAbsent(handler.getTs(), generic);
		assert gresult == null : gresult.info();
		return generic;
	}

	// protected abstract DefaultHandler buildHandler(Class<?> clazz, T meta, List<T> supers, Serializable value, List<T> components, long ts, long[] otherTs);

	protected abstract Class<T> getTClass();

	private final static ProxyFactory PROXY_FACTORY = new ProxyFactory();
	private final static MethodFilter METHOD_FILTER = method -> method.getName().equals("toString");

	@SuppressWarnings("unchecked")
	private T newInstance(Class<?> clazz) {

		PROXY_FACTORY.setSuperclass(clazz.isInterface() ? Object.class : clazz);
		PROXY_FACTORY.setInterfaces(clazz.isInterface() ? getTClass().isAssignableFrom(clazz) ? new Class[] { clazz } : new Class[] { clazz, getTClass() } : getTClass().isAssignableFrom(clazz) ? new Class[] {} : new Class[] { getTClass() });
		try {
			return (T) PROXY_FACTORY.createClass(METHOD_FILTER).newInstance();
		} catch (InstantiationException | IllegalAccessException e) {
			throw new IllegalStateException(e);
		}
	}

	public Class<?> getAnnotedClass(T generic) {
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

	public abstract class DefaultHandler implements MethodHandler, ISignature<T> {

		private final Class<?> clazz;
		private final T meta;
		private final List<T> supers;
		private final Serializable value;
		private final List<T> components;
		private final long ts;

		// public final long[] otherTs;

		protected DefaultHandler(Class<?> clazz, T meta, List<T> supers, Serializable value, List<T> components, long ts) {
			this.clazz = clazz;
			this.meta = meta;
			this.supers = supers;
			this.value = value;
			this.components = components;
			this.ts = ts;
			// this.otherTs = otherTs.clone();
		}

		@Override
		public Object invoke(Object self, Method m, Method proceed, Object[] args) throws Throwable {
			return ((DefaultVertex<?>) self).defaultToString();
		}

		abstract protected AbstractRoot<T> getRoot();

		public Vertex getVertex() {
			return new Vertex(getClazz(), getTs(), getMeta() != null ? getMeta().getTs() : getTs(), getSupers().stream().map(T::getTs).collect(Collectors.toList()), getValue(), getComponents().stream().map(T::getTs).collect(Collectors.toList()),
					getBirthTs());
		}

		public Class<?> getClazz() {
			return clazz;
		}

		@Override
		public T getMeta() {
			return meta;
		}

		@Override
		public List<T> getSupers() {
			return supers;
		}

		@Override
		public Serializable getValue() {
			return value;
		}

		@Override
		public List<T> getComponents() {
			return components;
		}

		@Override
		public long getTs() {
			return ts;
		}

	};

	protected AbstractContext<T> start(AbstractContext<T> context) {
		contextWrapper.set(context);
		return context;
	}

	protected void stop(AbstractContext<T> context) {
		assert contextWrapper.get() == context;
		contextWrapper.set(null);
	}

	@Override
	public String toString() {
		return defaultToString();
	}

}
