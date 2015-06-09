package org.genericsystem.exampleswing.application;

import java.awt.Component;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.AbstractCellEditor;
import javax.swing.JButton;
import javax.swing.JTable;
import javax.swing.UIManager;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumnModel;

public class ButtonEditor extends AbstractCellEditor implements TableCellRenderer, TableCellEditor, MouseListener {
	private static final long serialVersionUID = 2833750011734533890L;

	private final JButton renderButton;
	private boolean isButtonColumnEditor;

	TableColumnModel columnModel;
	int column;

	public ButtonEditor(TableColumnModel columnModel, int column) {
		this.columnModel = columnModel;
		this.column = column;

		this.renderButton = new JButton();
	}

	@Override
	public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
		return renderButton;
	}

	@Override
	public Object getCellEditorValue() {
		return renderButton.getText();
	}

	@Override
	public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
		if (isSelected) {
			renderButton.setForeground(table.getSelectionForeground());
			renderButton.setBackground(table.getSelectionBackground());
		} else {
			renderButton.setForeground(table.getForeground());
			renderButton.setBackground(UIManager.getColor("Button.background"));
		}
		renderButton.setText(value.toString());
		return renderButton;
	}

	@Override
	public void mousePressed(MouseEvent e) {
		if (columnModel.getColumn(column).getCellEditor() == this)
			isButtonColumnEditor = true;
	}

	@Override
	public void mouseReleased(MouseEvent e) {
		if (isButtonColumnEditor)
			columnModel.getColumn(column).getCellEditor().stopCellEditing();

		isButtonColumnEditor = false;
	}

	@Override
	public void mouseClicked(MouseEvent e) {
	}

	@Override
	public void mouseEntered(MouseEvent e) {
	}

	@Override
	public void mouseExited(MouseEvent e) {
	}
}
