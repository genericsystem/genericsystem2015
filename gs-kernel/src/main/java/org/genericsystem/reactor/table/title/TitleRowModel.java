package org.genericsystem.reactor.table.title;
//package org.genericsystem.distributed.cacheonserver.ui.table.title;
//
//import java.io.Serializable;
//import java.util.Objects;
//
//import javafx.beans.property.ReadOnlyObjectWrapper;
//import javafx.beans.value.ObservableValue;
//
//import org.genericsystem.common.Generic;
//import org.genericsystem.distributed.ui.CompositeModel;
//
//public class TitleRowModel extends CompositeModel<CompositeModel> {
//
//	public TitleRowModel(Generic[] generics, StringExtractor stringExtractor) {
//		super(generics, stringExtractor);
//	}
//
//	public ObservableValue<String> getFirstCellString() {
//		Serializable value = getGeneric().getValue();
//		return new ReadOnlyObjectWrapper<>((value instanceof Class ? ((Class<?>) value).getSimpleName() : Objects.toString(value)) + "(s)");
//	}
//
// }
