package org.genericsystem.adminold;

import java.awt.BorderLayout;
import java.awt.Dimension;

import javax.swing.JFrame;
import javax.swing.JPanel;

public class AdminFrame extends JFrame implements Refreshable {
	private static final long serialVersionUID = 5868325769001340979L;

	EngineManager engineManager;
	AdminManager adminPanel;

	public AdminFrame() {
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setTitle("Admin GS");
		engineManager = new EngineManager(this);
		getContentPane().add(engineManager, BorderLayout.NORTH);
		getContentPane().add(new ConsoleManager(), BorderLayout.SOUTH);

		getContentPane().setPreferredSize(new Dimension(700, 900));
		pack();
		setVisible(true);
	}

	@Override
	public void refresh() {
		if (adminPanel == null) {
			adminPanel = new AdminManager();
			getContentPane().add(adminPanel, BorderLayout.CENTER);
		}
		engineManager.refresh();
		adminPanel.refresh();
	}

	class AdminManager extends JPanel implements Refreshable {
		private static final long serialVersionUID = 3586350333741588123L;

		private CacheManager cacheManager;
		private InstancesManager instancesManager;

		public AdminManager() {
			setLayout(new BorderLayout());
			if (cacheManager == null) {
				cacheManager = new CacheManager(engineManager.getEngine(), AdminFrame.this);
				add(cacheManager, BorderLayout.NORTH);
			}
			if (instancesManager == null) {
				instancesManager = new InstancesManager(engineManager.getEngine());
				add(instancesManager, BorderLayout.CENTER);
			}
		}

		@Override
		public void refresh() {
			if (engineManager.getEngine() != null) {
				getContentPane().add(new AdminManager(), BorderLayout.CENTER);
				instancesManager.setVisible(true);
				cacheManager.setVisible(true);
			} else {
				instancesManager.setVisible(false);
				cacheManager.setVisible(false);
			}
			instancesManager.refresh();
			cacheManager.refresh();
		}
	}
}
