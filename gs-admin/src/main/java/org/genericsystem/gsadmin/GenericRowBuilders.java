package org.genericsystem.gsadmin;

import org.genericsystem.ui.table.RowBuilder;
import org.genericsystem.ui.table.Table;

public abstract class GenericRowBuilders<COL, T> extends RowBuilder<COL, T> {

	public static class TextCellRowBuilder<COL> extends GenericRowBuilders<COL, String> {

	}

	public static class TextCellFirstRowBuilder<COL> extends TextCellRowBuilder<COL> {

	}

	public static final class TableCellRowBuilder<COL> extends GenericRowBuilders<COL, Table> {

	}
}
