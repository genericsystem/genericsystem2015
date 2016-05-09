//package org.genericsystem.distributed.cacheonserver.ui.table.title;
//
//import javafx.beans.property.ReadOnlyObjectWrapper;
//import javafx.beans.value.ObservableValue;
//
//import org.genericsystem.common.Generic;
//import org.genericsystem.distributed.cacheonserver.ui.table.TypeTableModel;
//
///**
// * @author Nicolas Feybesse
// *
// */
//public class TitleTypeTableModel extends TypeTableModel {
//
//	private final ObservableValue<TitleRowModel> titleRowModel;
//
//	public TitleTypeTableModel(Generic[] generics, StringExtractor stringExtractor) {
//		super(generics, stringExtractor);
//		titleRowModel = new ReadOnlyObjectWrapper<>(buildTableModel(generics[0]));
//	}
//
//	public ObservableValue<TitleRowModel> getTitleRowModel() {
//		return titleRowModel;
//	}
//
// }
