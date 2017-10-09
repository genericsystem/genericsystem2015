package org.genericsystem.ir.app.gui.utils;

import java.text.Collator;
import java.util.function.Predicate;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ImgType;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ImgType.ImgInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ZoneType.ZoneInstance;
import org.genericsystem.reactor.context.ForEachExtractor;

import io.reactivex.Observable;

/**
 * This class contains all the {@link ForEachExtractor} needed across the app.
 * 
 * @author Pierrik Lassalas
 */
@SuppressWarnings({ "unchecked", "rawtypes" })
public class ObservableListExtractorCustom {

	/*
	 * === DOC CLASS AND DOC ===
	 */

	// public static class DOC_CLASS_SELECTOR implements ForEachExtractor {
	// @Override
	// public Observable<Snapshot<Generic>> apply(Generic[] generics) {
	// Root root = generics[0].getRoot();
	// DocClassType docClass = root.find(DocClassType.class);
	// Snapshot<Generic> docClassInstances = (Snapshot) docClass.getAllDocClasses();
	// if (null == docClassInstances)
	// return Observable.just(Snapshot.empty());
	// return Observable.just(docClassInstances);
	// }
	// }

	public static class DOC_SELECTOR implements ForEachExtractor {
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			Root root = generics[0].getRoot();
			ImgType imgType = root.find(ImgType.class);
			Snapshot<Generic> imgInstances = (Snapshot) imgType.getImgInstances();
			if (null == imgInstances)
				return Observable.just(Snapshot.empty());
			return Observable.just(imgInstances);
		}
	}

	/*
	 * === ZONES ===
	 */

	// public static class ZONE_SELECTOR implements ForEachExtractor {
	// @Override
	// public Observable<Snapshot<Generic>> apply(Generic[] generics) {
	// Generic currentDocClass = generics[0];
	// Root root = currentDocClass.getRoot();
	// System.out.println("Current docClass: " + currentDocClass.info());
	// Snapshot<ZoneInstance> zones = (Snapshot) currentDocClass.getHolders(root.find(ZoneGeneric.class));
	// if (zones == null)
	// return Observable.just(Snapshot.empty());
	// return Observable.just((Snapshot) zones.sorted());
	// }
	// }

	public static class ZONE_SELECTOR_BEST implements ForEachExtractor {
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			ImgInstance currentImg = (ImgInstance) generics[0];
			System.out.println("Document: " + currentImg.info());
			Snapshot<ZoneInstance> zoneInstances = currentImg.getZoneInstances();
			if (zoneInstances == null)
				return Observable.just(Snapshot.empty());
			return Observable.just((Snapshot) zoneInstances.sort((g1, g2) -> Integer.compare(getNum(g1), getNum(g2))));
		}

		private int getNum(ZoneInstance zoneInstance) {
			return Integer.valueOf(zoneInstance.getZoneNum().getValue().toString(), 10);
		}
	}

	// public static class ZONE_SELECTOR_REALITY implements ForEachExtractor {
	// @Override
	// public Observable<Snapshot<Generic>> apply(Generic[] generics) {
	// Generic currentDoc = generics[0];
	// Root root = currentDoc.getRoot();
	// System.out.println("Document: " + currentDoc.info());
	// Snapshot<ZoneTextInstance> zoneTextInstances = (Snapshot) currentDoc.getHolders(root.find(ZoneText.class));
	// if (zoneTextInstances == null)
	// return Observable.just(Snapshot.empty());
	// long start = System.nanoTime();
	// Snapshot<Generic> ol = (Snapshot) zoneTextInstances.filter(zt -> "reality".equals(zt.getImgFilter().getValue())).sort((g1, g2) -> Integer.compare(g1.getZoneNum(), g2.getZoneNum()));
	// long stop = System.nanoTime();
	// System.out.println("--------- zone selector: " + (stop - start) / 1_000_000 + "ms");
	// return Observable.just(ol);
	// }
	// }

	/*
	 * === DATALIST ===
	 */

	public static class DATALIST_SELECTOR implements ForEachExtractor {
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			ZoneInstance zoneInstance = (ZoneInstance) generics[1];
			ImgInstance currentImg = (ImgInstance) generics[2];
			Predicate<ZoneInstance> filterByZone = z -> z.getZoneNum() == zoneInstance.getZoneNum() && !z.getValue().toString().isEmpty();
			Snapshot<ZoneInstance> zoneInstances = currentImg.getZoneInstances();
			return Observable.just((Snapshot) zoneInstances.filter(filterByZone).map(z -> z.getConsolidated()).sort((g1, g2) -> Collator.getInstance().compare(g1.getValue(), g2.getValue())));
		}
	}

	/*
	 * === OCR TEXT ===
	 */

	// public static class OCR_SELECTOR implements ForEachExtractor {
	// @Override
	// public Observable<Snapshot<Generic>> apply(Generic[] generics) {
	// Root root = generics[0].getRoot();
	// Snapshot<Generic> filters = root.find(ImgFilter.class).getInstances();
	// if (filters == null)
	// return Observable.just(Snapshot.empty());
	// return Observable.just(filters);
	// }
	// }

	/*
	 * === SCORE ===
	 */

	// public static class SCORE_SELECTOR implements ForEachExtractor {
	// @SuppressWarnings({ "unchecked", "rawtypes" })
	// @Override
	// public Observable<Snapshot<Generic>> apply(Generic[] generics) {
	// ZoneInstance zoneInstance = (ZoneInstance) generics[0];
	// Root root = zoneInstance.getRoot();
	// System.out.println("Current zone: " + zoneInstance.info());
	// Snapshot<ScoreInstance> scores = (Snapshot) zoneInstance.getHolders(root.find(Score.class));
	// // scores.forEach(g -> System.out.println(g.info()));
	// if (scores == null)
	// return Observable.just(Snapshot.empty());
	// return Observable.just((Snapshot) scores);
	// }
	// }

}
