package org.genericsystem.defaults;

import java.io.Serializable;
import java.util.List;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;

public interface DefaultCache<T extends DefaultVertex<T>> extends DefaultContext<T> {

	T addInstance(T meta, List<T> overrides, Serializable value, List<T> components);

	T update(T update, List<T> overrides, Serializable newValue, List<T> newComponents);

	T merge(T update, List<T> overrides, Serializable newValue, List<T> newComponents);

	T setInstance(T meta, List<T> overrides, Serializable value, List<T> components);

	void forceRemove(T generic);

	void remove(T generic);

	void conserveRemove(T generic);

	void flush();

	void tryFlush() throws ConcurrencyControlException;

	void mount();

	void clear();

	long shiftTs();

	void unmount();
}
