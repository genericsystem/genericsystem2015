package org.genericsystem.reactor.context;

import java.util.function.BiFunction;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;

import io.reactivex.Observable;

public interface ObservableListExtractorFromContext extends BiFunction<Context, Tag, Observable<Snapshot<Generic>>> {

}
