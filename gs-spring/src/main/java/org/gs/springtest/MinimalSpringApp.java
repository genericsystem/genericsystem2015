package org.gs.springtest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.stereotype.Component;

@Component
public class MinimalSpringApp {

	private static final String CONFIG_PATH = "classpath*:org/gs/springtest/applicationContext.xml";

	@Autowired
	Foo myBean;

	void sayHello() {
		System.out.println(myBean.getTest());
		// System.out.println("Hello World - ");
	}

	public static void main(final String[] args) {
		final ApplicationContext context = new ClassPathXmlApplicationContext(CONFIG_PATH);
		final MinimalSpringApp minimalSpringApp = (MinimalSpringApp) context.getBean("minimalSpringApp");
		minimalSpringApp.sayHello();
	}

}
