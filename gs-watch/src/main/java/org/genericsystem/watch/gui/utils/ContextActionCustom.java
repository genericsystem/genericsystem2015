package org.genericsystem.watch.gui.utils;

import java.util.Arrays;

import org.genericsystem.common.Root;
import org.genericsystem.cv.comparator.ComputeBestTextPerZone;
import org.genericsystem.cv.comparator.ComputeTrainedScores;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.cv.model.ModelTools;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.gscomponents.Modal.ModalWithDisplay;
import org.genericsystem.watch.VerticleDeployerFromWatchApp;

import io.vertx.core.Verticle;

/**
 * This class contains all the {@link ContextAction} needed across the app.
 * 
 * @author Pierrik Lassalas
 */
public class ContextActionCustom {

	/*
	 * === PAGE NAVIGATION ===
	 */

	public static class CALL_STATISTICS_PAGE implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			System.out.println("Redirecting to statistics page");
			tag.setInheritedContextPropertyValue(PageSwitcher.PAGE, context, PageSwitcher.FILTERS_STATISTICS);
		}
	}

	public static class CALL_HOME_PAGE implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			System.out.println("Redirecting to home page");
			tag.setInheritedContextPropertyValue(PageSwitcher.PAGE, context, PageSwitcher.HOME_PAGE);
		}
	}

	public static class TEST implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			System.out.println("called from " + tag.getTag());
		}
	}

	/*
	 * === GENERIC SYSTEM ===
	 */

	public static class REMOVE_CUSTOM implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			context.remove();
			context.flush();
		}
	}

	public static class SAVE implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			System.out.println("Saving...");
			Root root = context.getGeneric().getRoot();
			System.out.println("Current thread (save): " + Thread.currentThread().getName());
			long start = System.nanoTime();
			root.getCurrentCache().flush();
			long stop = System.nanoTime();
			System.out.println("Saved in " + (stop - start) / 1_000_000 + "ms");
		}
	}

	public static class MODAL_DISPLAY_FLEX_CUSTOM implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			Tag ancestor = tag.getParent().getParent(); // ZoneTextDiv
			ancestor.find(ModalWithDisplay.class).getDisplayProperty(context).setValue("flex");
		}
	}

	/*
	 * === BEST OCR TEXT ===
	 */

	public static class REFRESH_BEST_TEXT implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			System.out.println("Refreshing best text...");
			Root root = context.getGeneric().getRoot();
			DocInstance docInstance = (DocInstance) context.getGeneric();
			String docType = docInstance.getDocClass().getValue().toString();
			// System.out.println("Current thread (refresh): " + Thread.currentThread().getName());
			Verticle worker = new WorkerVerticle(root) {
				@Override
				public void start() throws Exception {
					ComputeBestTextPerZone.computeOneFile(root, docInstance, docType);
					docInstance.setRefreshTimestamp(ModelTools.getCurrentDate());
					root.getCurrentCache().flush();
					System.out.println("Done!");
				}
			};
			VerticleDeployerFromWatchApp.deployWorkerVerticle(worker, "Failed to execute the task");
		}
	}

	/*
	 * === STATISTICS ===
	 */

	public static class COMPUTE_STATS implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			System.out.println("Computing scores...");
			computeStatistics(context, tag, false);
		}
	}

	public static class COMPUTE_STATS_STRICT implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			System.out.println("Computing scores (using strict mode)...");
			computeStatistics(context, tag, true);
		}
	}

	public static void computeStatistics(Context context, Tag tag, boolean useStrict) {
		DocClassInstance docClassInstance = (DocClassInstance) context.getGeneric();
		Root root = docClassInstance.getRoot();
		Arrays.asList(context.getGenerics()).forEach(g -> System.out.println(g.info()));
		Verticle worker = new WorkerVerticle() {
			@Override
			public void start() throws Exception {
				ComputeTrainedScores.compute(root, docClassInstance.getValue().toString(), useStrict);
				System.out.println("Done computing scores!");
			}
		};
		VerticleDeployerFromWatchApp.deployWorkerVerticle(worker, "Failed to execute the task");
	}

}
