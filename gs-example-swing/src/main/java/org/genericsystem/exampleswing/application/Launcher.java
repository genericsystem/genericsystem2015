package org.genericsystem.exampleswing.application;

import javax.swing.SwingUtilities;

import org.genericsystem.exampleswing.model.Car;
import org.genericsystem.exampleswing.model.CarColor;
import org.genericsystem.exampleswing.model.Power;
import org.genericsystem.mutability.Engine;

public class Launcher {

	public static void main(String[] args) {
		Engine engine = new Engine(Car.class, CarColor.class, Power.class);

		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				new InstancesEditor(engine.find(Car.class));
			}
		});
	}
}
