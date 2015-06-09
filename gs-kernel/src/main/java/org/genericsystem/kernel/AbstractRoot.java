package org.genericsystem.kernel;

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

import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.defaults.DefaultConfig.MetaAttribute;
import org.genericsystem.defaults.DefaultConfig.MetaRelation;
import org.genericsystem.defaults.DefaultConfig.Sequence;
import org.genericsystem.defaults.DefaultConfig.SystemMap;
import org.genericsystem.defaults.DefaultRoot;
import org.genericsystem.defaults.DefaultVertex;

public abstract class AbstractRoot<T extends DefaultVertex<T>> implements DefaultRoot<T>, MethodHandler {

	private final TsGenerator generator = new TsGenerator();
	protected Wrapper<T> contextWrapper = buildContextWrapper();
	private final SystemCache<T> systemCache;
	protected final Map<Generic, TsDependencies<Generic>> dependenciesMap = new ConcurrentHashMap<>();

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
		initSubRoot(value, persistentDirectoryPath, userClasses);
		init((T) this, LifeManager.TS_SYSTEM, null, Collections.emptyList(), value, Collections.emptyList(), new LifeManager(LifeManager.SYSTEM_TS));
		contextWrapper.set(newCache());
		systemCache = new SystemCache<>(this, getClass());
		systemCache.mount(Arrays.asList(MetaAttribute.class, MetaRelation.class, SystemMap.class, Sequence.class), userClasses);
		flushContext();
		// shiftContext();
	}

	protected void initSubRoot(Serializable value, String persistentDirectoryPath, Class<?>... userClasses) {
	};

	@Override
	public abstract AbstractContext<T> newCache();

	public interface Wrapper<T extends DefaultVertex<T>> {
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

	public T getGenericFromTs(long ts) {
		return idsMap.get(ts);
	}

	private final Map<T, Vertex> vertexMap = new ConcurrentHashMap<>();

	protected Vertex getVertex(T generic) {
		return vertexMap.get(generic);
	}

	public Vertex getVertex(long ts) {
		return vertexMap.get(getGenericFromTs(ts));
	}

	public LifeManager getLifeManager(T generic) {
		return getVertex(generic).getLifeManager();
	}

	@Override
	public long getTs(T generic) {
		return getVertex(generic).getTs();
	}

	@Override
	public long getBirthTs(T generic) {
		return getVertex(generic).getBirthTs();
	}

	@Override
	public long getDeathTs(T generic) {
		return getVertex(generic).getDeathTs();
	}

	@Override
	public T getMeta(T generic) {
		return getGenericFromTs(getVertex(generic).getMeta());
	}

	@Override
	public List<T> getSupers(T generic) {
		return getVertex(generic).getSupers().stream().map(id -> getGenericFromTs(id)).collect(Collectors.toList());
	}

	@Override
	public Serializable getValue(T generic) {
		return getVertex(generic).getValue();
	}

	@Override
	public List<T> getComponents(T generic) {
		return getVertex(generic).getComponents().stream().map(id -> getGenericFromTs(id)).collect(Collectors.toList());
	}

	T init(Long ts, Class<?> clazz, T meta, List<T> supers, Serializable value, List<T> components, LifeManager lifeMAnager) {
		return init(newT(clazz, meta), ts == null ? pickNewTs() : ts, meta, supers, value, components, lifeMAnager);
	}

	protected T init(T generic, long ts, T meta, List<T> supers, Serializable value, List<T> components, LifeManager lifeMAnager) {
		return init(generic, ts, meta == null ? ts : meta.getTs(), supers.stream().map(g -> g.getTs()).collect(Collectors.toList()), value, components.stream().map(g -> g.getTs()).collect(Collectors.toList()), lifeMAnager);
	}

	T init(Class<?> clazz, long ts, long meta, List<Long> supers, Serializable value, List<Long> components, LifeManager lifeMAnager) {
		return init(newT(clazz, getGenericFromTs(meta)), ts, meta, supers, value, components, lifeMAnager);
	}

	protected T init(T generic, long ts, long meta, List<Long> supers, Serializable value, List<Long> components, LifeManager lifeMAnager) {
		Vertex vertex = new Vertex(ts, meta, supers, value, components, lifeMAnager);
		assert generic != null;
		T gresult = idsMap.putIfAbsent(ts, generic);
		assert gresult == null;
		Vertex result = vertexMap.putIfAbsent(generic, vertex);
		assert result == null;
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
		((ProxyObject) instance).setHandler(AbstractRoot.this);
		return instance;
	}

	public Class<?> getAnnotedClass(T vertex) {
		if (vertex.isSystem()) {
			Class<?> annotedClass = findAnnotedClass(vertex);
			if (annotedClass != null)
				return annotedClass;
		}
		return vertex.getClass();
	}

	protected abstract boolean isInitialized();

	@Override
	public Object invoke(Object self, Method m, Method proceed, Object[] args) throws Throwable {
		if (m.getName().equals("getRoot"))
			return this;
		return ((DefaultVertex<T>) self).defaultToString();
	}

	@Override
	public String toString() {
		return defaultToString();
	}
}
