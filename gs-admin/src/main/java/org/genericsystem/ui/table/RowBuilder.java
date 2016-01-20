package org.genericsystem.ui.table;

import java.util.function.Function;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.geometry.Insets;
import javafx.scene.control.Button;
import org.genericsystem.common.Generic;
import org.genericsystem.gsadmin.GenericRow;
import org.genericsystem.gsadmin.GenericRowBuilders.TableCellRowBuilder;
import org.genericsystem.gsadmin.GenericTableBuilders.TextCellTableBuilder;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.components.GSVBox;
import org.genericsystem.ui.table.Stylable.TableStyle;

public abstract class RowBuilder<COL, T> implements Builder {

	@Override
	public void init(Element<?> rowPanel) {
		GSHBox firstCell = new GSHBox(rowPanel).select(GenericRow::getFirstElement).setMinWidth(Table::getFirstColumnWidth).setPrefWidth(Table::getFirstColumnWidth).setMaxWidth(Table::getFirstColumnWidth).setStyleClass(Cell<Generic>::getStyleClass);
		{
			new GSLabel(firstCell, Cell<String>::getObservableModel);
		}

		GSHBox secondCell = new GSHBox(rowPanel).select(GenericRow::getSecondElement).setMinWidth(Table::getSecondColumnWidth).setPrefWidth(Table::getSecondColumnWidth).setMaxWidth(Table::getSecondColumnWidth).setStyleClass(Cell<Generic>::getStyleClass);
		{
			if (this instanceof TableCellRowBuilder)
				new GSVBox(secondCell).select(Cell<Table>::getObservableModel).include(new TextCellTableBuilder()::init);
			else
				new GSLabel(secondCell, Cell<String>::getObservableModel);
		}

		GSHBox cells = new GSHBox(rowPanel).forEach(GenericRow::getElements).setMinWidth(Table::getColumnWidth).setPrefWidth(Table::getColumnWidth).setMaxWidth(Table::getColumnWidth).setStyleClass(Cell<Generic>::getStyleClass);
		{
			if (this instanceof TableCellRowBuilder)
				new GSVBox(cells).select(Cell<Table>::getObservableModel).include(new TextCellTableBuilder()::init);
			else
				new GSLabel(cells, Cell<String>::getObservableModel);
		}

		GSHBox lastCell = new GSHBox(rowPanel).select(GenericRow::getLastElement).setMinWidth(Table::getLastColumnWidth).setPrefWidth(Table::getLastColumnWidth).setMaxWidth(Table::getLastColumnWidth).setStyleClass(Cell<Generic>::getStyleClass);
		{
			new GSButton(lastCell, Cell<String>::getObservableModel).setAction(GenericRow::delete).addBoot(Button::paddingProperty, new Insets(2, 2, 2, 2));
		}
	}

	public abstract Row build(Object item, ObservableValue<String> firstColumnString, ObservableValue<T> secondColumnExtractor, ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, ObservableValue<String> lastColumnString,
			TableStyle tableStyle);

}