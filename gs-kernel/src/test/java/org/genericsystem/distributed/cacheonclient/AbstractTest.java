package org.genericsystem.distributed.cacheonclient;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;

import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.GSDeploymentOptions;
import org.genericsystem.kernel.Statics;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;

public abstract class AbstractTest {

	protected static Logger log = LoggerFactory.getLogger(AbstractTest.class);
	String ServerVerticleId;
	protected final String directoryPath = System.getenv("HOME") + "/test/Vertx_tests/snapshot_save";
	private EngineServer httpGsServer;

	private void cleanDirectory(String directoryPath) {
		File file = new File(directoryPath);
		if (file.exists())
			for (File f : file.listFiles()) {
				if (!".lock".equals(f.getName()))
					f.delete();
			}
	}

	public GSDeploymentOptions getDeploymentOptions() {
		return new GSDeploymentOptions().addEngine(Statics.ENGINE_VALUE, directoryPath);
	}

	@BeforeMethod
	public void beforeClass() {
		cleanDirectory(directoryPath);
		httpGsServer = new EngineServer(getDeploymentOptions());
		httpGsServer.start();
	}

	@AfterMethod
	public void afterClass() {
		httpGsServer.stop();
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

	protected static void compareGraph(Generic persisted, Generic read) {
		List<Generic> persistVisit = new ArrayList<>(persisted.getCurrentCache().computeDependencies(persisted));
		List<Generic> readVisit = new ArrayList<>(read.getCurrentCache().computeDependencies(read));
		assert persistVisit.size() == readVisit.size() : persistVisit + " \n " + readVisit;
		for (int i = 0; i < persistVisit.size(); i++) {
			assert persistVisit.get(i).genericEquals(readVisit.get(i));
			Generic persitedGeneric = persistVisit.get(i);
			Generic readGeneric = readVisit.get(i);
			assert persitedGeneric.getBirthTs() == readGeneric.getBirthTs() : persistVisit.get(i).info() + " " + persitedGeneric.getBirthTs() + "  " + readGeneric.getBirthTs();
			assert persistVisit.get(i).genericEquals(readVisit.get(i)) : persistVisit.get(i).info() + " " + readVisit.get(i).info();
		}
	}

}
