package org.genericsystem.watch;

import org.genericsystem.common.GSVertx;
import org.genericsystem.common.Root;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.Doc.DocFilename;
import org.genericsystem.cv.model.Doc.DocTimestamp;
import org.genericsystem.cv.model.Doc.RefreshTimestamp;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.LevDistance;
import org.genericsystem.cv.model.MeanLevenshtein;
import org.genericsystem.cv.model.Score;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.cv.model.ZoneText.ZoneTimestamp;
import org.genericsystem.kernel.Engine;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;
import io.vertx.core.Verticle;

public class VerticleDeployerFromWatchApp extends AbstractVerticle {

	private static final String gsPath = System.getenv("HOME") + "/genericsystem/gs-cv_model3/";
	private static final DeploymentOptions OPTIONS_NORMAL = new DeploymentOptions();
	private static final DeploymentOptions OPTIONS_WORKER = new DeploymentOptions().setWorker(true).setMaxWorkerExecuteTime(Long.MAX_VALUE);
	private Root root;

	public VerticleDeployerFromWatchApp() {
		this.root = new Engine(gsPath, Doc.class, RefreshTimestamp.class, DocTimestamp.class, DocFilename.class, DocClass.class, ZoneGeneric.class, ZoneText.class, ZoneTimestamp.class, ImgFilter.class, LevDistance.class, MeanLevenshtein.class,
				Score.class);
	}

	public VerticleDeployerFromWatchApp(Root root) {
		this.root = root;
	}

	public static void deployWorkerVerticle(Verticle verticle, String errorMessage) throws IllegalStateException {
		GSVertx.vertx().getVertx().deployVerticle(verticle, OPTIONS_WORKER, res -> {
			if (res.failed())
				throw new IllegalStateException(errorMessage != null ? errorMessage : "Deployment of worker verticle failed.", res.cause());
		});
	}

	public void doDeploy() {
		GSVertx.vertx().getVertx().deployVerticle(this, OPTIONS_WORKER, res -> {
			if (res.failed())
				throw new IllegalStateException("Deployment of main verticle (" + deploymentID() + ") failed.", res.cause());
			else
				System.out.println("Main verticle (" + deploymentID() + ") deployed");
		});
	}

	private void deployVerticle(Verticle verticle, boolean worker) throws IllegalStateException {
		vertx.deployVerticle(verticle, worker ? OPTIONS_WORKER : OPTIONS_NORMAL, res -> {
			if (res.failed())
				throw new IllegalStateException("Deployment of verticle failed.", res.cause());
			else
				System.out.println("Verticle deployed");
		});
	}

	@Override
	public void start(Future<Void> startFuture) throws Exception {
		deployVerticle(new MailWatcherVerticle(), true);
		deployVerticle(new PdfsConverterVerticle(), true);
		deployVerticle(new ClassifierVerticle(), true);
		deployVerticle(new DezonerVerticle(), true);
		deployVerticle(new AddImageToEngineVerticle(root), true);
		deployVerticle(new OcrVerticle(root), true);
		startFuture.complete();
	}
}
