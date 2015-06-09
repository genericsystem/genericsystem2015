package org.genericsystem.defaults;

import java.util.function.Predicate;
import java.util.stream.Stream;

import org.genericsystem.api.core.Snapshot;

public interface DefaultInheritanceSnapshot<T extends DefaultVertex<T>> extends Snapshot<T> {

	
	@Override
	default DefaultInheritanceSnapshot<T> filter(Predicate<T> predicate){
		return new DefaultInheritanceSnapshot<T>() {

			@Override
			public Stream<T> stream() {
				return DefaultInheritanceSnapshot.this.stream().filter(predicate);
			}

			@Override
			public T get(Object o) {
				T result = DefaultInheritanceSnapshot.this.get(o);
				return result != null && predicate.test(result) ? result : null;
			}
		};
	}
	
	
	default DefaultInheritanceSnapshot<T> filterDefaultsFromMeta(T attibute,int pos){
		return filter(t->t.getComponent(pos).inheritsFrom(attibute.getComponent(pos)));
	}
}
