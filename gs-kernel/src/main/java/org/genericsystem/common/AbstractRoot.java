package org.genericsystem.common;

import java.io.Serializable;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;

import javassist.util.proxy.MethodFilter;
import javassist.util.proxy.MethodHandler;
import javassist.util.proxy.ProxyFactory;
import javassist.util.proxy.ProxyObject;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.defaults.DefaultConfig.MetaAttribute;
import org.genericsystem.defaults.DefaultConfig.MetaRelation;
import org.genericsystem.defaults.DefaultConfig.Sequence;
import org.genericsystem.defaults.DefaultConfig.SystemMap;
import org.genericsystem.defaults.DefaultRoot;
import org.genericsystem.defaults.DefaultVertex;
import org.genericsystem.kernel.Statics;

public abstract class AbstractRoot<T extends DefaultVertex<T>> implements DefaultRoot<T>, TProxy<T> {

	private final TsGenerator generator = new TsGenerator();
	protected Wrapper<T> contextWrapper = buildContextWrapper();
	private final SystemCache<T> systemCache;
	private boolean isInitialized = false;

	public AbstractRoot(Class<?>... userClasses) {
		this(Statics.ENGINE_VALUE, userClasses);
	}

	public AbstractRoot(String persistentDirectoryPath, Class<?>... userClasses) {
		this(Statics.ENGINE_VALUE, persistentDirectoryPath, userClasses);
	}

	@Override
	public AbstractRoot<T> getRoot() {
		return this;
	}

	@SuppressWarnings("unchecked")
	public AbstractRoot(Serializable value, String persistentDirectoryPath, Class<?>... userClasses) {
		vertex = new Vertex(ApiStatics.TS_SYSTEM, ApiStatics.TS_SYSTEM, Collections.emptyList(), value, Collections.emptyList(), ApiStatics.SYSTEM_TS);
		T gresult = idsMap.putIfAbsent(ApiStatics.TS_SYSTEM, (T) this);
		assert gresult == null;
		initSubRoot(value, persistentDirectoryPath, userClasses);
		// init((T) this, ApiStatics.TS_SYSTEM, ApiStatics.TS_SYSTEM, Collections.emptyList(), value, Collections.emptyList(), ApiStatics.SYSTEM_TS);
		contextWrapper.set(newCache());
		systemCache = new SystemCache<>(this, getClass());
		systemCache.mount(Arrays.asList(MetaAttribute.class, MetaRelation.class, SystemMap.class, Sequence.class), userClasses);
		flushContext();
		// shiftContext();
		isInitialized = true;
	}

	protected void initSubRoot(Serializable value, String persistentDirectoryPath, Class<?>... userClasses) {
	};

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

	protected void flushContext() {
		// //Autoflush
	}

	public long pickNewTs() {
		return generator.pickNewTs();
	}

	@Override
	public AbstractContext<T> getCurrentCache() {
		return contextWrapper.get();
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
	public Class<?> findAnnotedClass(T vertex) {
		return systemCache.getClassByVertex(vertex);
	}

	public static class TsGenerator {
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

	protected final Map<Long, T> idsMap = new ConcurrentHashMap<>();

	public T getGenericByTs(long ts) {
		return idsMap.get(ts);
	}

	protected T init(Class<?> clazz, Vertex vertex) {
		return init(newT(clazz, vertex.getTs() == vertex.getMeta() ? null : getGenericByTs(vertex.getMeta())), vertex.getTs(), vertex.getMeta(), vertex.getSupers(), vertex.getValue(), vertex.getComponents(), vertex.getOtherTs());
	}

	T init(Long ts, Class<?> clazz, T meta, List<T> supers, Serializable value, List<T> components, long[] otherTs) {
		long newTs = ts == null ? pickNewTs() : ts;
		return init(newT(clazz, meta), newTs, meta == null ? newTs : meta.getTs(), supers.stream().map(g -> g.getTs()).collect(Collectors.toList()), value, components.stream().map(g -> g.getTs()).collect(Collectors.toList()), otherTs);
	}

	private T init(T generic, long ts, long meta, List<Long> supers, Serializable value, List<Long> components, long[] otherTs) {
		Vertex vertex = new Vertex(ts, meta, supers, value, components, otherTs);
		((ProxyObject) generic).setHandler(buildHandler(vertex));
		T gresult = idsMap.putIfAbsent(ts, generic);
		assert gresult == null;
		return generic;
	}

	protected abstract Class<T> getTClass();

	protected T newT(Class<?> clazz, T meta) {
		InstanceClass metaAnnotation = meta == null ? null : getAnnotedClass(meta).getAnnotation(InstanceClass.class);
		if (metaAnnotation != null)
			if (clazz == null || clazz.isAssignableFrom(metaAnnotation.value()))
				clazz = metaAnnotation.value();
			else if (!metaAnnotation.value().isAssignableFrom(clazz))
				getCurrentCache().discardWithException(new InstantiationException(clazz + " must extends " + metaAnnotation.value()));
		try {
			if (clazz == null || !getTClass().isAssignableFrom(clazz))
				return newInstance(getTClass());
			return newInstance(clazz);
		} catch (IllegalArgumentException e) {
			getCurrentCache().discardWithException(e);
		}
		return null; // Not reached
	}

	private final static ProxyFactory PROXY_FACTORY = new ProxyFactory();
	private final static MethodFilter METHOD_FILTER = method -> method.getName().equals("getRoot") || method.getName().equals("toString");

	@SuppressWarnings("unchecked")
	private T newInstance(Class<?> clazz) {
		if (clazz.isInterface()) {
			PROXY_FACTORY.setSuperclass(Object.class);
			if (getTClass().isAssignableFrom(clazz))
				PROXY_FACTORY.setInterfaces(new Class[] { clazz });
			else
				PROXY_FACTORY.setInterfaces(new Class[] { clazz, getTClass() });
		} else {
			PROXY_FACTORY.setSuperclass(clazz);
			if (getTClass().isAssignableFrom(clazz))
				PROXY_FACTORY.setInterfaces(new Class[] {});
			else
				PROXY_FACTORY.setInterfaces(new Class[] { getTClass() });
		}
		T instance = null;
		try {
			instance = (T) PROXY_FACTORY.createClass(METHOD_FILTER).newInstance();
		} catch (InstantiationException | IllegalAccessException e) {
			throw new IllegalStateException(e);
		}
		return instance;

	}

	protected abstract MethodHandler buildHandler(Vertex vertex);

	public Class<?> getAnnotedClass(T vertex) {
		if (vertex.isSystem()) {
			Class<?> annotedClass = findAnnotedClass(vertex);
			if (annotedClass != null)
				return annotedClass;
		}
		return vertex.getClass();
	}

	public boolean isInitialized() {
		return isInitialized;
	}

	public abstract class AbstractRootWrapper implements MethodHandler {

		private final Vertex vertex;

		protected AbstractRootWrapper(Vertex vertex) {
			this.vertex = vertex;
		}

		protected AbstractRoot<T> getRoot() {
			return AbstractRoot.this;
		}

		public Vertex getVertex() {
			return vertex;
		}

		@Override
		public Object invoke(Object self, Method m, Method proceed, Object[] args) throws Throwable {
			switch (m.getName()) {
			case ("getRoot"):
				return AbstractRoot.this;
			default:
				return ((DefaultVertex<?>) self).defaultToString();
			}
		}
	}

	protected final Vertex vertex;

	@Override
	public long getBirthTs() {
		return vertex.getOtherTs()[0];
	}

	@Override
	public long getDeathTs() {
		return vertex.getOtherTs()[2];
	}

	@Override
	public Vertex getVertex() {
		return vertex;
	}
}
