package org.genericsystem.cdi;

import java.util.function.Supplier;

import javax.enterprise.inject.Instance;
import javax.inject.Inject;

import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.cdi.PersistenceTest.Count;
import org.genericsystem.mutability.Cache;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.arquillian.testng.Arquillian;
import org.jboss.shrinkwrap.api.ShrinkWrap;
import org.jboss.shrinkwrap.api.asset.StringAsset;
import org.jboss.shrinkwrap.api.spec.JavaArchive;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class AbstractTest extends Arquillian {

	protected static Logger log = LoggerFactory.getLogger(AbstractTest.class);

	@Inject
	Engine engine;

	@Inject
	Instance<Cache> cacheProvider;

	@Deployment
	public static JavaArchive createDeployment() {
		JavaArchive javaArchive = ShrinkWrap.create(JavaArchive.class);
		javaArchive.addClasses(UserClassesProvider.class, PersistentDirectoryProvider.class, MockPersistentDirectoryProvider.class, /* EventLauncher.class, */CacheSessionProvider.class, CacheRequestProvider.class, EngineProvider.class, Count.class);
		createBeansXml(javaArchive);
		return javaArchive;
	}

	private static void createBeansXml(JavaArchive javaArchive) {
		StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append("<beans xmlns=\"http://java.sun.com/xml/ns/javaee\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\" http://java.sun.com/xml/ns/javaee http://java.sun.com/xml/ns/javaee/beans_1_0.xsd\">");
		stringBuilder.append("<alternatives> ");
		stringBuilder.append("<class>org.genericsystem.cdi.MockPersistentDirectoryProvider</class>");
		stringBuilder.append("</alternatives>");
		stringBuilder.append("</beans>");
		javaArchive.addAsManifestResource(new StringAsset(stringBuilder.toString()), "beans.xml");
	}

	@FunctionalInterface
	public static interface VoidSupplier {
		public void getNothing();
	}

	public void catchAndCheckCause(VoidSupplier supplier, Class<? extends Throwable> clazz) {
		try {
			supplier.getNothing();
		} catch (RollbackException ex) {
			if (ex.getCause() == null)
				throw new IllegalStateException("Rollback Exception has not any cause", ex);
			if (!clazz.isAssignableFrom(ex.getCause().getClass()))
				throw new IllegalStateException("Cause of rollback exception is not of type : " + clazz.getSimpleName() + ", but is " + ex.getCause(), ex);
			return;
		}
		assert false : "Unable to catch any rollback exception!";
	}

	public void catchAndCheckCause(Supplier<?> supplier, Class<? extends Throwable> clazz) {
		try {
			supplier.get();
		} catch (RollbackException ex) {
			if (ex.getCause() == null)
				throw new IllegalStateException("Rollback Exception has not any cause", ex);
			if (!clazz.isAssignableFrom(ex.getCause().getClass()))
				throw new IllegalStateException("Cause of rollback exception is not of type : " + clazz.getSimpleName() + ", but is " + ex.getCause(), ex);
			return;
		}
		assert false : "Unable to catch any rollback exception!";
	}
}
