package org.genericsystem.cache;

import java.io.File;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

import org.genericsystem.kernel.Statics;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;

public abstract class AbstractPersistenceTest {

	String ServerVerticleId;
	private final String directoryPath = System.getenv("HOME") + "/test/Vertx_tests/snapshot_save";

	private void cleanDirectory(String directoryPath) {
		File file = new File(directoryPath);
		if (file.exists())
			for (File f : file.listFiles())
				f.delete();
	}

	@BeforeMethod
	public void beforeClass() {
		cleanDirectory(directoryPath);
		BlockingQueue<String> queue = new ArrayBlockingQueue<>(1);
		GSVertx.vertx().getVertx().deployVerticle(HttpGSServer.class.getName(), new GSDeploymentOptions().addEngine(Statics.ENGINE_VALUE, directoryPath), result -> {
			try {
				queue.put(result.result());
			} catch (Exception e1) {
				e1.printStackTrace();
			}
		});
		try {
			ServerVerticleId = queue.take();
		} catch (InterruptedException e) {
			e.printStackTrace();
			return;
		}
		System.out.println("server started!");
	}

	@AfterMethod
	public void afterClass() {
		BlockingQueue<Integer> queue = new ArrayBlockingQueue<>(1);
		GSVertx.vertx().getVertx().undeploy(ServerVerticleId, result -> {
			try {
				queue.put(0);
			} catch (Exception e1) {
				e1.printStackTrace();
			}
		});
		try {
			queue.take();
		} catch (InterruptedException e) {
			e.printStackTrace();
			return;
		}
		System.out.println("afterClass ok");
	}
}
