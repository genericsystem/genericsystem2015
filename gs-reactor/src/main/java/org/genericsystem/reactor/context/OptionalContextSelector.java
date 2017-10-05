package org.genericsystem.reactor.context;

import java.util.Optional;
import java.util.function.BiFunction;

import org.genericsystem.defaults.tools.RxJavaHelpers;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;

import io.reactivex.Observable;

public interface OptionalContextSelector extends BiFunction<Context, Tag, Observable<Optional<Context>>> {

	public static class SELECTION_SELECTOR implements OptionalContextSelector {
		@Override
		public Observable<Optional<Context>> apply(Context context, Tag tag) {
			if (SelectionDefaults.class.isAssignableFrom(tag.getClass()))
				return RxJavaHelpers.optionalValuesOf(((SelectionDefaults) tag).getSelectionProperty(context));
			else
				throw new IllegalStateException("SELECTION_SELECTOR is applicable only to tags implementing SelectionDefaults.");
		}
	}

	public static class REMOVABLE_HOLDER_SELECTOR implements OptionalContextSelector {
		@Override
		public Observable<Optional<Context>> apply(Context context, Tag tag) {
			return ForEachExtractor.HOLDERS.apply(context.getParent().getGenerics()).switchMap(s -> s.setOnChanged()).map(holders ->
			(!context.getParent().getGeneric().isRequiredConstraintEnabled(context.getGeneric().getComponents().indexOf(context.getGenerics()[2])) && holders.size() == 1) || holders.size() > 1 ?
					Optional.of(context) : Optional.empty());
		}
	}

	public static class HOLDER_ADDITION_ENABLED_SELECTOR implements OptionalContextSelector {
		@Override
		public Observable<Optional<Context>> apply(Context context, Tag tag) {
			return ForEachExtractor.HOLDERS.apply(context.getGenerics()).switchMap(s -> s.setOnChanged())
					.map(holders ->  holders.isEmpty()
							|| (!(context.getGeneric().getComponents().size() == 1 && context.getGeneric().isPropertyConstraintEnabled())
									&& !context.getGeneric().isSingularConstraintEnabled(context.getGeneric().getComponents().indexOf(context.getGenerics()[2])))
							? Optional.of(context) : Optional.empty());
		}
	}
}