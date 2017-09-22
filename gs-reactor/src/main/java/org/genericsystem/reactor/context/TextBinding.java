package org.genericsystem.reactor.context;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.function.BiFunction;
import java.util.stream.Collectors;

import org.genericsystem.common.Statics;
import org.genericsystem.defaults.tools.RxJavaHelpers;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.reactivex.Observable;

public interface TextBinding extends BiFunction<Context, Tag, Observable<String>> {
	public static final Logger log = LoggerFactory.getLogger(TextBinding.class);

	public static class GENERIC_STRING implements TextBinding {
		@Override
		public Observable<String> apply(Context context, Tag tag) {
			return null;
		}
	}

	public static class CACHE_LEVEL implements TextBinding {
		@Override
		public Observable<String> apply(Context context, Tag tag) {
			return RxJavaHelpers.valuesOf(context.getCacheLevelObservableValue()).map(i -> String.valueOf(i));
		}
	}

	public static class ERROR_COMPONENTS implements TextBinding {
		@Override
		public Observable<String> apply(Context context, Tag tag) {
			if (context.getGeneric().getComponents().isEmpty())
				return Observable.just("You must enter a value.");
			return Observable.just(context.getGeneric().getComponents().stream().map(i -> i.toString()).collect(Collectors.joining(", ")) + " needed to create a " + context.getGeneric().toString() + ".");
		}
	}

	public static class LAST_UPDATE implements TextBinding {
		@Override
		public Observable<String> apply(Context context, Tag tag) {
			return RxJavaHelpers.valuesOf(context.getTsObservableValue()).map(ts -> {
				Long tsMs = (Long) ts / Statics.MILLI_TO_NANOSECONDS;
				Date dateMs = new Date(tsMs);
				Instant instant = Instant.ofEpochMilli(dateMs.getTime());
				LocalDateTime ldt = LocalDateTime.ofInstant(instant, ZoneOffset.systemDefault());
				return "Last update: " + ldt.format(DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss"));
			});
		}
	}

	public static class LOGGED_USER implements TextBinding {
		@Override
		public Observable<String> apply(Context context, Tag tag) {
			return RxJavaHelpers.optionalValuesOf(tag.getLoggedUserProperty(context))
					.map(userOpt -> userOpt.isPresent() ? "Current user: " + (String) userOpt.get().getValue() : "No user logged.");
		}
	}
}
