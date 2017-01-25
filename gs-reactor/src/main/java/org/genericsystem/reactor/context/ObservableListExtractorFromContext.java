package org.genericsystem.reactor.context;

import java.util.function.BiFunction;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;

import javafx.collections.ObservableList;

public interface ObservableListExtractorFromContext extends BiFunction<Context, Tag, ObservableList<Generic>> {

}
