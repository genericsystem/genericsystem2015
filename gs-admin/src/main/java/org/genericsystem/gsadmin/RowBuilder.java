package org.genericsystem.gsadmin;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.Builder.ElementBuilder;
import org.genericsystem.gsadmin.CellBuilder.TableCellBuilder;
import org.genericsystem.gsadmin.CellBuilder.TextCellBuilder;
import org.genericsystem.gsadmin.Stylable.TableStyle;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.utils.Transformation;

public abstract class RowBuilder<COL, U, T> extends ElementBuilder {

	Row build(RowMetaModel<COL, U, T> rowMetaModel) {
		if (rowMetaModel.firstColumnString == null)
			return null;
		// return new Row(getFirstElement(firstColumnString, getFirstCellStyle(tableStyle)), getElements(columns, columnExtractor, getCellStyle(tableStyle)), getRowStyle(tableStyle));

		// RowMetaModel<COL, T> rowMetaModel = new RowMetaModel<>(firstColumnString, columns, columnExtractor, tableStyle);
		return new Row(getFirstElement(rowMetaModel), getElements(rowMetaModel), getRowStyle(rowMetaModel.getTableStyle()));
	}

	// Row build(ObservableValue<String> firstColumnString, ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, TableStyle tableStyle) {
	// if (firstColumnString == null)
	// return null;
	// // return new Row(getFirstElement(firstColumnString, getFirstCellStyle(tableStyle)), getElements(columns, columnExtractor, getCellStyle(tableStyle)), getRowStyle(tableStyle));
	//
	// RowMetaModel<COL, T> rowMetaModel = new RowMetaModel<>(firstColumnString, columns, columnExtractor, tableStyle);
	// return new Row(getFirstElement(rowMetaModel), getElements(rowMetaModel), getRowStyle(tableStyle));
	// }

	private ObservableValue<Cell<?>> getFirstElement(RowMetaModel<COL, U, T> rowModel) {
		return new ReadOnlyObjectWrapper<>(new TextCellBuilder<>().build(rowModel.getFirstColumnString(), getFirstCellStyle(rowModel.getTableStyle())));
	}

	private ObservableList<Cell<?>> getElements(RowMetaModel<COL, U, T> rowModel) {

		return new Transformation<>(rowModel.getColumns(), column -> {
			ObservableValue<U> apply = rowModel.getColumnExtractor().apply(column);
			assert apply.getValue() != null;
			ObservableValue<T> result = (ObservableValue) apply;
			return getCellBuilder().build(result, getCellStyle(rowModel.getTableStyle()));
		});
	}

	@Override
	public void init(Element<?> rowPanel) {
		new GSHBox(rowPanel).select(Row::getFirstElement).include(new TextCellBuilder<>()::init).setPrefWidth(200).setMinHeight(80).setStyleClass(Cell<T>::getStyleClass);
		new GSHBox(rowPanel).forEach(Row::getElements).include(getCellBuilder()::init).setPrefWidth(200).setMinHeight(80).setStyleClass(Cell<T>::getStyleClass);
	}

	abstract CellBuilder<T> getCellBuilder();

	public ObservableValue<String> getRowStyle(TableStyle tableStyle) {
		return tableStyle.row;
	}

	ObservableValue<String> getFirstCellStyle(TableStyle tableStyle) {
		return tableStyle.firstCell;
	}

	ObservableValue<String> getCellStyle(TableStyle tableStyle) {
		return tableStyle.cell;
	}

	static class TextCellRowBuilder<COL, U> extends RowBuilder<COL, U, String> {
		@Override
		CellBuilder<String> getCellBuilder() {
			return new TextCellBuilder<>();
		}
	}

	static class TextCellFirstRowBuilder<COL, U> extends TextCellRowBuilder<COL, U> {
		@Override
		ObservableValue<String> getFirstCellStyle(TableStyle tableStyle) {
			return tableStyle.firstRowFirstCell;
		}

		@Override
		ObservableValue<String> getCellStyle(TableStyle tableStyle) {
			return tableStyle.firstRowCell;
		}

		@Override
		public ObservableValue<String> getRowStyle(TableStyle tableStyle) {
			return tableStyle.firstRow;
		}
	}

	static final class TableCellRowBuilder<COL, U> extends RowBuilder<COL, U, Table> {

		@Override
		CellBuilder<Table> getCellBuilder() {
			return new TableCellBuilder<>();
		}
	}

}