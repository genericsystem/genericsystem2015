package org.genericsystem.issuetracker;

import org.genericsystem.cdi.CacheRequestProvider;
import org.genericsystem.cdi.CacheSessionProvider;
import org.genericsystem.cdi.EngineProvider;
import org.genericsystem.cdi.PersistentDirectoryProvider;
import org.genericsystem.cdi.UserClassesProvider;
import org.genericsystem.issuetracker.IssueTrackerTest.A;
import org.genericsystem.issuetracker.IssueTrackerTest.B;
import org.genericsystem.issuetracker.IssueTrackerTest.ProviderA;
import org.genericsystem.issuetracker.IssueTrackerTest.ProviderB;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.arquillian.testng.Arquillian;
import org.jboss.shrinkwrap.api.ShrinkWrap;
import org.jboss.shrinkwrap.api.asset.StringAsset;
import org.jboss.shrinkwrap.api.spec.JavaArchive;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class AbstractTest extends Arquillian {

	protected static Logger log = LoggerFactory.getLogger(AbstractTest.class);

	@Deployment
	public static JavaArchive createDeployment() {
		JavaArchive javaArchive = ShrinkWrap.create(JavaArchive.class);
		javaArchive.addPackage("org.genericsystem.issuetracker.model");
		javaArchive.addClasses(UserClassesProvider.class, PersistentDirectoryProvider.class, B.class, A.class, ProviderA.class, ProviderB.class, /* EventLauncher.class, */CacheSessionProvider.class, CacheRequestProvider.class, EngineProvider.class);
		createBeansXml(javaArchive);
		return javaArchive;
	}

	private static void createBeansXml(JavaArchive javaArchive) {
		StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append("<beans xmlns=\"http://java.sun.com/xml/ns/javaee\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\" http://java.sun.com/xml/ns/javaee http://java.sun.com/xml/ns/javaee/beans_1_0.xsd\">");
		stringBuilder.append("</beans>");
		javaArchive.addAsManifestResource(new StringAsset(stringBuilder.toString()), "beans.xml");
	}

}
