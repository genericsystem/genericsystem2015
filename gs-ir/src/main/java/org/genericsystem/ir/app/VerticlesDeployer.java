package org.genericsystem.ir.app;

import org.genericsystem.common.Root;
import org.genericsystem.cv.newmodel.FillNewModelWithData;
import org.genericsystem.ir.Dispatcher;
import org.genericsystem.ir.DistributedVerticle;
import org.genericsystem.ir.OcrEngineHolderVerticle;

public class VerticlesDeployer {

	private static final String gsPath = System.getenv("HOME") + "/genericsystem/gs-cv-newmodel";

	public static void main(String[] args) {
		Root engine = FillNewModelWithData.getEngine(gsPath);
		deployIrVerticles(engine);
	}

	private static void deployIrVerticles(Root root) {
		deployOcrEngineHolderVerticle(root);
		deployDispatcher();
		deployDistributedVerticle();
	}

	private static void deployOcrEngineHolderVerticle(Root root) {
		OcrEngineHolderVerticle ocrEngineHolderVerticle = new OcrEngineHolderVerticle(root);
		ocrEngineHolderVerticle.doDeploy();
	}

	private static void deployDispatcher() {
		Dispatcher dispatcher = new Dispatcher();
		dispatcher.doDeploy();
	}

	private static void deployDistributedVerticle() {
		DistributedVerticle distributedVerticle = new DistributedVerticle();
		distributedVerticle.doDeploy();
	}
}
