package org.genericsystem.kernel;

import java.lang.reflect.Method;

import org.testng.annotations.Test;

import javassist.util.proxy.MethodFilter;
import javassist.util.proxy.MethodHandler;
import javassist.util.proxy.ProxyFactory;
import javassist.util.proxy.ProxyObject;

@Test
public class JavassistTest {

	static <T> T newInstance(Class<T> clazz) throws InstantiationException, IllegalAccessException {
		ProxyFactory f = new ProxyFactory();
		f.setSuperclass(clazz);
		System.out.println("getSuperClass ProxyFactory: " + f.getSuperclass());
		f.setFilter(new MethodFilter() {
			@Override
			public boolean isHandled(Method m) {
				return m.getName().equals("test");
			}
		});
		Class<T> proxyClass = f.createClass();
		System.out.println("getClass ProxyClass: " + proxyClass);
		MethodHandler handler = new MethodHandler() {
			@Override
			public Object invoke(Object self, Method m, Method proceed, Object[] args) throws Throwable {
				System.out.println("method proceed : " + proceed.getName());
				System.out.println("Before method : " + m.getName());

				Object o = proceed.invoke(self, args);
				System.out.println("After method : " + m.getName());
				return o;
			}
		};
		T instance = proxyClass.newInstance();
		((ProxyObject) instance).setHandler(handler);
		return instance;
	}

	public static class Entity {
		public void test() {
			System.out.println("Call method test from class Entity");
		}

		public void test2() {
			System.out.println("Call method test2 from class Entity");
		}

	}

	public static class Entity2 {
		public void test() {
			System.out.println("Call method test from class Entity2");
		}

		public void test2() {
			System.out.println("Call method test2 from class Entity2");
		}

	}

	public void testJavassist() throws Exception {
		Entity entity = newInstance(Entity.class);
		assert (entity instanceof Entity);

		assert !Entity.class.equals(entity.getClass());
		System.out.println("Entity.class: " + Entity.class);
		System.out.println("entity.getClass(): " + entity.getClass());

		assert Entity.class.isAssignableFrom(entity.getClass());
		assert !entity.getClass().isAssignableFrom(Entity.class);

		entity.test();
		entity.test2();

	}
}
