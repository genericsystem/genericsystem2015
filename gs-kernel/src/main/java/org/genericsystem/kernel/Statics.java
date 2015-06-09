package org.genericsystem.kernel;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.genericsystem.defaults.DefaultVertex;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Statics {

	private static Logger log = LoggerFactory.getLogger(Statics.class);

	private static ThreadLocal<Long> threadDebugged = new ThreadLocal<>();

	public final static String ENGINE_VALUE = "Engine";

	public static final long MILLI_TO_NANOSECONDS = 1000000L;

	public static final int ATTEMPT_SLEEP = 15;
	public static final int ATTEMPTS = 50;

	public static final long GARBAGE_PERIOD = 1000L;
	public static final long GARBAGE_INITIAL_DELAY = 1000L;
	public static final long LIFE_TIMEOUT = 1386174608777L;// 30 minutes

	public static void debugCurrentThread() {
		threadDebugged.set(System.currentTimeMillis());
	}

	public static void stopDebugCurrentThread() {
		threadDebugged.remove();
	}

	public static boolean isCurrentThreadDebugged() {
		return threadDebugged.get() != null;
	}

	public static void logTimeIfCurrentThreadDebugged(String message) {
		if (isCurrentThreadDebugged())
			log.info(message + " : " + (System.currentTimeMillis() - threadDebugged.get()));
	}

	public static class Supers<T extends DefaultVertex<T>> extends ArrayList<T> {
		private static final long serialVersionUID = 6163099887384346235L;

		public Supers(List<T> adds) {
			adds.forEach(this::add);
		}

		public Supers(List<T> adds, T lastAdd) {
			this(adds);
			add(lastAdd);
		}

		public Supers(List<T> adds, List<T> otherAdds) {
			this(adds);
			otherAdds.forEach(this::add);
		}

		@Override
		public boolean add(T candidate) {
			for (T element : this)
				if (element.inheritsFrom(candidate))
					return false;
			Iterator<T> it = iterator();
			while (it.hasNext())
				if (candidate.inheritsFrom(it.next()))
					it.remove();
			return super.add(candidate);
		}
	}

	// public static <T extends IVertex<T>> List<T> reverseCollections(Collection<T> linkedHashSet) {
	// List<T> dependencies = new ArrayList<>(linkedHashSet);
	// Collections.reverse(dependencies);
	// return dependencies;
	// }

}
