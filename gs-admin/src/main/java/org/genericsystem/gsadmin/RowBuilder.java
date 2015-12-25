package org.genericsystem.gsadmin;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.Builder.ElementBuilder;
import org.genericsystem.gsadmin.CellBuilder.FirstRowFirstCellTextCellBuilder;
import org.genericsystem.gsadmin.CellBuilder.FirstRowTextCellBuilder;
import org.genericsystem.gsadmin.CellBuilder.RowFirstCellTextCellBuilder;
import org.genericsystem.gsadmin.CellBuilder.TableCellBuilder;
import org.genericsystem.gsadmin.CellBuilder.TextCellBuilder;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.utils.Transformation;

public abstract class RowBuilder<COL, U, T> extends ElementBuilder<Row, Cell<?>, RowModel<COL, U, T>> {

	@Override
	protected Row build(RowModel<COL, U, T> rowMetaModel) {
		return new Row(getFirstElement(rowMetaModel), getElements(rowMetaModel), getStyle(rowMetaModel));
	}

	@Override
	protected ObservableValue<Cell<?>> getFirstElement(RowModel<COL, U, T> rowModel) {
		return new ReadOnlyObjectWrapper<>(getRowFirstCellBuilder().build(new CellModel<String>(rowModel.getFirstColumnString(), rowModel.getTableStyle())));
	}

	@Override
	protected ObservableList<Cell<?>> getElements(RowModel<COL, U, T> rowModel) {
		return new Transformation<>(rowModel.getColumns(), column -> getCellBuilder().build(new CellModel<T>((ObservableValue) rowModel.getColumnExtractor().apply(column), rowModel.getTableStyle())));
	}

	@Override
	public void init(Element<?> rowPanel) {
		new GSHBox(rowPanel).select(Row::getFirstElement).include(getRowFirstCellBuilder()::init).setPrefWidth(200).setMinHeight(80).setStyleClass(Cell<T>::getStyleClass);
		new GSHBox(rowPanel).forEach(Row::getElements).include(getCellBuilder()::init).setPrefWidth(200).setMinHeight(80).setStyleClass(Cell<T>::getStyleClass);
	}

	abstract CellBuilder<String> getRowFirstCellBuilder();

	abstract CellBuilder<T> getCellBuilder();

	@Override
	public ObservableValue<String> getStyle(RowModel<COL, U, T> model) {
		return model.getTableStyle().row;
	}

	static class TextCellRowBuilder<COL, U> extends RowBuilder<COL, U, String> {

		@Override
		CellBuilder<String> getRowFirstCellBuilder() {
			return new RowFirstCellTextCellBuilder();
		}

		@Override
		CellBuilder<String> getCellBuilder() {
			return new TextCellBuilder();
		}
	}

	static class TextCellFirstRowBuilder<COL, U> extends TextCellRowBuilder<COL, U> {
		@Override
		CellBuilder<String> getRowFirstCellBuilder() {
			return new FirstRowFirstCellTextCellBuilder();
		}

		@Override
		CellBuilder<String> getCellBuilder() {
			return new FirstRowTextCellBuilder();
		}

		@Override
		public ObservableValue<String> getStyle(RowModel<COL, U, String> model) {
			return model.getTableStyle().firstRow;
		}
	}

	static final class TableCellRowBuilder<COL, U> extends RowBuilder<COL, U, Table> {

		@Override
		CellBuilder<String> getRowFirstCellBuilder() {
			return new RowFirstCellTextCellBuilder();
		}

		@Override
		CellBuilder<Table> getCellBuilder() {
			return new TableCellBuilder<>();
		}
	}

}