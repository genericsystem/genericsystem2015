package org.genericsystem.exampleswing.application;

import java.awt.Component;
import java.util.List;

import javax.swing.DefaultCellEditor;
import javax.swing.JComboBox;
import javax.swing.JTable;
import javax.swing.table.TableCellRenderer;

public class ComboBoxEditor extends DefaultCellEditor implements TableCellRenderer {
	private static final long serialVersionUID = 2803140437363960800L;

	private final JComboBox<?> combo;

	public ComboBoxEditor(List<String> items) {
		super(new JComboBox<String>(items.toArray(new String[items.size()])));
		combo = new JComboBox<String>(items.toArray(new String[items.size()]));
	}

	@Override
	public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
		if (isSelected) {
			combo.setForeground(table.getSelectionForeground());
			combo.setBackground(table.getSelectionBackground());
		} else {
			combo.setForeground(table.getForeground());
			combo.setBackground(table.getBackground());
		}
		combo.setSelectedItem(value);
		return combo;
	}
}
