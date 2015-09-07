package org.genericsystem.cache;

import java.io.File;
import java.util.function.Supplier;

import org.genericsystem.api.core.exceptions.RollbackException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;

public abstract class AbstractTest {

	protected static Logger log = LoggerFactory.getLogger(AbstractTest.class);
	String ServerVerticleId;
	protected final String directoryPath = System.getenv("HOME") + "/test/Vertx_tests/snapshot_save";
	private HttpGSServer httpGsServer;

	private void cleanDirectory(String directoryPath) {
		File file = new File(directoryPath);
		if (file.exists())
			for (File f : file.listFiles()) {
				if (!".lock".equals(f.getName()))
					f.delete();
			}
	}

	public abstract GSDeploymentOptions getDeploymentOptions();

	@BeforeMethod
	public void beforeClass() {
		// System.out.println("before class");
		cleanDirectory(directoryPath);
		httpGsServer = new HttpGSServer(getDeploymentOptions());
		httpGsServer.start();
		// System.out.println("beforeClass ok");
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

	@AfterMethod
	public void afterClass() {
		httpGsServer.stop();
		System.gc();
	}

}
