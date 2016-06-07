package org.genericsystem.spring;

import org.genericsystem.carcolor.model.Car;
import org.genericsystem.common.Generic;
import org.springframework.context.support.ClassPathXmlApplicationContext;

public class App {

	public static void main(String[] args) {
		ClassPathXmlApplicationContext ctx = new ClassPathXmlApplicationContext("applicationContext.xml");

		EngineProvider engineProvider = ctx.getBean(EngineProvider.class);

		Engine engine = engineProvider.getEngine();
		Generic car = engine.find(Car.class);
		assert car.isAlive();
		engine.getCurrentCache().flush();
		assert car.isAlive();
		car.remove();
		engine.getCurrentCache().flush();
		assert !car.isAlive();

	}

}
