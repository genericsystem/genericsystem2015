package org.genericsystem.exampleswing.application;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.genericsystem.mutability.Engine;

class CacheManager extends JPanel implements ActionListener {

	private static final long serialVersionUID = 6698753884141032302L;
	private final Engine engine;
	private final JButton flushButton = new JButton("Flush");
	private final JButton clearButton = new JButton("Clear");
	private final JButton mountButton = new JButton("Mount");
	private final JButton unmountButton = new JButton("Unmount");
	private final JLabel level = new JLabel();
	private final List<Refreshable> refreshables = new ArrayList<>();

	CacheManager(Engine engine, Refreshable... refreshables) {
		this.engine = engine;
		Arrays.stream(refreshables).forEach(this::addRefreshable);
		flushButton.addActionListener(this);
		clearButton.addActionListener(this);
		mountButton.addActionListener(this);
		unmountButton.addActionListener(this);
		level.setText("Cache level : " + engine.getCurrentCache().getCacheLevel());
		add(flushButton);
		add(clearButton);
		add(mountButton);
		add(unmountButton);
		add(level);
	}

	public void addRefreshable(Refreshable refreshable) {
		refreshables.add(refreshable);
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		if (flushButton.equals(e.getSource())) {
			engine.getCurrentCache().flush();
			return;
		}
		if (clearButton.equals(e.getSource())) {
			engine.getCurrentCache().clear();
			refreshables.forEach(Refreshable::refresh);
			return;
		}
		if (mountButton.equals(e.getSource())) {
			engine.getCurrentCache().mount();
			level.setText("Cache level : " + engine.getCurrentCache().getCacheLevel());
			return;
		}
		if (unmountButton.equals(e.getSource())) {
			engine.getCurrentCache().unmount();
			refreshables.forEach(Refreshable::refresh);
			level.setText("Cache level : " + engine.getCurrentCache().getCacheLevel());
			return;
		}
	}

	public static interface Refreshable {
		public void refresh();
	}
}