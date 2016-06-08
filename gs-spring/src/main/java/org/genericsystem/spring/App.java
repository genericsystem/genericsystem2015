package org.genericsystem.spring;

import org.genericsystem.common.Generic;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.stereotype.Service;

/**
 * Hello world!
 *
 */
public class App {
	/*
	 * @Autowired private static MyBean myBean;
	 * 
	 * @Autowired private Engine engine;
	 */

	public static void main(String[] args) {
		System.out.println("START!");
		ApplicationContext context = new ClassPathXmlApplicationContext("META-INF/applicationContext.xml");
		// Engine engine = engineProvider.getEngine();
		System.out.println("START!1");
		Engine engine = context.getBean(Engine.class);
		System.out.println("START!2");
		Generic vehicle = engine.addInstance("Vehicle");
		assert vehicle.isAlive();
		engine.getCurrentCache().flush();
		assert vehicle.isAlive();
		vehicle.remove();
		engine.getCurrentCache().flush();
		assert !vehicle.isAlive();

		System.out.println("END!");
		/*
		 * MyBean p = context.getBean(MyBean.class); System.out.println("my beans method: " + p.getStr());
		 */
	}

	@Service
	public class MyBean {
		public String getStr() {
			return "string";
		}
	}
}
