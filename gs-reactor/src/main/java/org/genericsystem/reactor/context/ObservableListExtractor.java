package org.genericsystem.reactor.context;

import java.text.Collator;
import java.util.Arrays;
import java.util.function.Function;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;

import io.reactivex.Observable;

@FunctionalInterface
public interface ObservableListExtractor extends Function<Generic[], Observable<Snapshot<Generic>>> {

	public static ObservableListExtractor from(Class<?>... genericClasses) {
		return gs -> Observable.just(() -> Arrays.stream(genericClasses).map(gs[0].getRoot()::<Generic>find));
	}

	public static final ObservableListExtractor INSTANCES = generics -> {
		// System.out.println("INSTANCES : " + Arrays.toString(generics) + " " + generics[0].getInstances().toObservableList());
		return Observable.just(generics[0].getInstances().sorted());
	};

	public static final ObservableListExtractor SUBINSTANCES = generics -> {
		// System.out.println("INSTANCES : " + Arrays.toString(generics) + " " + generics[0].getSubInstances().toObservableList());
		return Observable.just(generics[0].getSubInstances().sorted());
	};

	public static final ObservableListExtractor SUBINSTANCES_ALPHABETICAL_ORDER = generics -> {
		return Observable.just(generics[0].getSubInstances().sort((g1, g2) -> Collator.getInstance().compare(g1.toString(), g2.toString())));
	};

	public static final ObservableListExtractor SUBINSTANCES_OF_META = generics -> {
		return Observable.just(generics[0].getMeta().getSubInstances().sorted());
	};

	public static final ObservableListExtractor ATTRIBUTES_OF_TYPE = generics -> {
		// System.out.println("ATTRIBUTES_OF_TYPE : " + Arrays.toString(generics) + " " + generics[0].getAttributes().toObservableList().filtered(attribute -> attribute.isCompositeForInstances(generics[0])));
		return Observable.just(generics[0].getAttributes().filter(attribute -> attribute.isCompositeForInstances(generics[0])).sorted());
	};

	public static final ObservableListExtractor ATTRIBUTES_OF_INSTANCES = generics -> {
		// System.out.println("ATTRIBUTES_OF_INSTANCES : " + Arrays.toString(generics) + " " + generics[1].getAttributes().toObservableList().filtered(attribute -> attribute.isCompositeForInstances(generics[1])));
		return Observable.just(generics[1].getAttributes().filter(attribute -> attribute.isCompositeForInstances(generics[1])).sorted());
	};

	public static final ObservableListExtractor COMPONENTS = generics -> {
		// System.out.println("COMPONENTS : " + Arrays.toString(generics) + " " + generics[0].getComponents());
		return Observable.just(() -> generics[0].getComponents().stream());
	};

	public static final ObservableListExtractor HOLDERS = generics -> {
		// System.out.println("HOLDERS : " + Arrays.toString(generics) + " " + generics[1].getHolders(generics[0]).toObservableList());
		return Observable.just(generics[1].getHolders(generics[0]).sorted());
	};

	public static final ObservableListExtractor OTHER_COMPONENTS_1 = gs -> ObservableListExtractor.COMPONENTS.apply(gs).map(s -> s.filter(g -> !gs[1].inheritsFrom(g)));

	public static final ObservableListExtractor OTHER_COMPONENTS_2 = gs -> ObservableListExtractor.COMPONENTS.apply(gs).map(s -> s.filter(g -> !gs[2].inheritsFrom(g)));

	public static class ATTRIBUTES_OF_TYPE implements ObservableListExtractor {
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			return ATTRIBUTES_OF_TYPE.apply(generics);
		}
	}

	public static class COMPONENTS implements ObservableListExtractor {
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			return COMPONENTS.apply(generics);
		}
	}

	public static class OTHER_COMPONENTS_1 implements ObservableListExtractor {
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			return OTHER_COMPONENTS_1.apply(generics);
		}
	}

	public static class OTHER_COMPONENTS_2 implements ObservableListExtractor {
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			return OTHER_COMPONENTS_2.apply(generics);
		}
	}

	public static class INSTANCES implements ObservableListExtractor {
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			return INSTANCES.apply(generics);
		}
	}

	public static class SUBINSTANCES implements ObservableListExtractor {
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			return SUBINSTANCES.apply(generics);
		}
	}

	public static class SUBINSTANCES_ALPHABETICAL_ORDER implements ObservableListExtractor {
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			return SUBINSTANCES_ALPHABETICAL_ORDER.apply(generics);
		}
	}

	public static class SUBINSTANCES_OF_META implements ObservableListExtractor {
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			return SUBINSTANCES_OF_META.apply(generics);
		}
	}

	public static class SUBINSTANCES_OF_LINK_COMPONENT implements ObservableListExtractor {
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			return ObservableListExtractor.SUBINSTANCES.apply(ObservableListExtractor.COMPONENTS.apply(generics).map(s -> s.filter(g -> !generics[2].inheritsFrom(g)).stream().toArray(Generic[]::new)).blockingFirst());
		}
	}

	public static class SUBINSTANCES_OF_RELATION_COMPONENT implements ObservableListExtractor {
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			return ObservableListExtractor.SUBINSTANCES.apply(ObservableListExtractor.COMPONENTS.apply(generics).map(s -> s.filter(g -> !generics[1].inheritsFrom(g)).stream().toArray(Generic[]::new)).blockingFirst());
		}
	}

	public static class ATTRIBUTES_OF_INSTANCES implements ObservableListExtractor {
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			return ATTRIBUTES_OF_INSTANCES.apply(generics);
		}
	}

	public static class HOLDERS implements ObservableListExtractor {
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			return HOLDERS.apply(generics);
		}
	}

	public static class NO_FOR_EACH implements ObservableListExtractor {
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			throw new IllegalStateException();
		}
	}
}