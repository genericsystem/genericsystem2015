package org.genericsystem.adminold;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Objects;

import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.table.AbstractTableModel;

import org.genericsystem.mutability.Generic;

public class InstancesManager extends JPanel implements Refreshable {
	private static final long serialVersionUID = 5868325769001340979L;

	private final InstancesTableModel tableModel;

	InstancesManager(Generic type) {
		setPreferredSize(new Dimension(500, 500));

		JTable table = new JTable(tableModel = new InstancesTableModel(type));
		new ButtonColumn(table, tableModel.getColumnCount() - 1);

		add(new JScrollPane(table), BorderLayout.CENTER);
		add(new CreateInstanceManager(type), BorderLayout.EAST);
	}

	@Override
	public void refresh() {
		tableModel.fireTableDataChanged();
	}

	private class CreateInstanceManager extends JPanel {

		private static final long serialVersionUID = 2790743754106657404L;
		private final JTextField newInstanceField;

		private final Generic type;

		CreateInstanceManager(Generic type) {
			this.type = type;
			newInstanceField = new JTextField("new Instance");
			newInstanceField.setColumns(10);
			add(newInstanceField);
			add(new CreateButton("Create"));
		}

		private class CreateButton extends JButton implements ActionListener {

			private static final long serialVersionUID = -9204494808782375894L;

			public CreateButton(String text) {
				super(text);
				addActionListener(this);
			}

			@Override
			public void actionPerformed(ActionEvent e) {
				type.setInstance(newInstanceField.getText());
				tableModel.fireTableDataChanged();
			}
		}
	}

	private class InstancesTableModel extends AbstractTableModel {
		private static final long serialVersionUID = -8137410628615273305L;

		private final Generic type;

		public InstancesTableModel(Generic type) {
			this.type = type;
		}

		@Override
		public int getRowCount() {
			return type.getSubInstances().size();
		}

		@Override
		public String getColumnName(int columnIndex) {
			if (columnIndex == 0)
				return Objects.toString(type.getValue());
			if (columnIndex == getColumnCount() - 1)
				return "Delete";
			return Objects.toString(type.getAttributes().getByIndex(columnIndex - 1).getValue());
		}

		@Override
		public int getColumnCount() {
			return type.getAttributes().size() + 2;
		}

		@Override
		public boolean isCellEditable(int rowIndex, int columnIndex) {
			return true;
		}

		@Override
		public Object getValueAt(int rowIndex, int columnIndex) {
			Generic generic = type.getSubInstances().getByIndex(rowIndex);
			if (columnIndex == 0)
				return Objects.toString(generic.getValue());
			if (columnIndex == getColumnCount() - 1)
				return "Delete";
			return Objects.toString(generic.getValue(type.getAttributes().getByIndex(columnIndex - 1)));
		}

		@Override
		public void setValueAt(Object value, int rowIndex, int columnIndex) {
			Generic generic = type.getSubInstances().getByIndex(rowIndex);
			if (columnIndex == 0)
				generic.updateValue(Objects.toString(value));
			else if (columnIndex == getColumnCount() - 1) {
				int returnCode = JOptionPane.showConfirmDialog(JOptionPane.getFrameForComponent(InstancesManager.this), "Are you sure you want to delete generic : " + generic.info());
				if (JOptionPane.OK_OPTION == returnCode)
					generic.remove();
			}
			// else
			// generic.setHolder(attributes[columnIndex - 1], Integer.parseInt(Objects.toString(value)));
			fireTableDataChanged();
		}
	}
}
