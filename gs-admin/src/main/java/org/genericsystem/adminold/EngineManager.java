package org.genericsystem.adminold;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.genericsystem.mutability.Engine;

class EngineManager extends JPanel implements Refreshable, ActionListener {

	private static final long serialVersionUID = 6698753884141032302L;

	private final Refreshable container;

	private Engine engine;

	private JPanel chooseEngine;
	private JPanel currentEngine;
	private String currentEnginePath;

	EngineManager(Refreshable container) {
		this.container = container;
		add(buildChooseEngine());
	}

	public Engine getEngine() {
		return engine;
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getSource() instanceof JFileChooser) {
			File selectedFile = ((JFileChooser) e.getSource()).getSelectedFile();
			if (selectedFile == null)
				return;
			currentEnginePath = selectedFile.getAbsolutePath();
			engine = new Engine("Engine", currentEnginePath);
			if (currentEngine == null)
				add(buildCurrentEngine());
			currentEngine.setVisible(true);
			chooseEngine.setVisible(false);
		}
		if (e.getSource() instanceof JButton) {
			engine.close();
			engine = null;
			currentEnginePath = null;
			currentEngine.setVisible(false);
			chooseEngine.setVisible(true);
		}
		container.refresh();
	}

	@Override
	public void refresh() {
		currentEngine.revalidate();
	}

	private JPanel buildChooseEngine() {
		chooseEngine = new JPanel();
		JFileChooser chooser = new JFileChooser(System.getProperty("user.home"));
		chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		chooser.addActionListener(this);
		chooseEngine.add(new JLabel("Choose Engine"));
		chooseEngine.add(chooser);
		return chooseEngine;
	}

	private JPanel buildCurrentEngine() {
		currentEngine = new JPanel();
		currentEngine.add(new JLabel("Current Engine : " + engine.getValue() + " , persistence path : " + currentEnginePath));
		JButton stopEngine = new JButton("Stop Engine");
		stopEngine.addActionListener(this);
		currentEngine.add(stopEngine);
		currentEngine.setVisible(false);
		return currentEngine;
	}

}