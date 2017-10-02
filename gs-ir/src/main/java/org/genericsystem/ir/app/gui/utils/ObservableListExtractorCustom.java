package org.genericsystem.ir.app.gui.utils;

import java.text.Collator;
import java.util.function.Predicate;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.Score;
import org.genericsystem.cv.model.Score.ScoreInstance;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
import org.genericsystem.reactor.context.ObservableListExtractor;

import io.reactivex.Observable;

/**
 * This class contains all the {@link ObservableListExtractor} needed across the app.
 * 
 * @author Pierrik Lassalas
 */
public class ObservableListExtractorCustom {

	/*
	 * === DOC CLASS AND DOC ===
	 */

	public static class DOC_CLASS_SELECTOR implements ObservableListExtractor {
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			Root root = generics[0].getRoot();
			DocClass docClass = root.find(DocClass.class);
			Snapshot<Generic> docClassInstances = docClass.getInstances();
			if (null == docClassInstances)
				return Observable.just(Snapshot.empty());
			return Observable.just(docClassInstances);
		}
	}

	public static class DOC_SELECTOR implements ObservableListExtractor {
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			DocClassInstance currentDocClass = (DocClassInstance) generics[0];
			Doc doc = generics[0].getRoot().find(Doc.class);
			if (null == currentDocClass)
				return Observable.just(Snapshot.empty());
			System.out.println("Current doc class : " + currentDocClass.info());
			Snapshot<Generic> docInstances = currentDocClass.getHolders(doc);
			if (null == docInstances)
				return Observable.just(Snapshot.empty());
			return Observable.just(docInstances);

		}
	}

	/*
	 * === ZONES ===
	 */

	public static class ZONE_SELECTOR implements ObservableListExtractor {
		@SuppressWarnings({ "unchecked", "rawtypes" })
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			Generic currentDocClass = generics[0];
			Root root = currentDocClass.getRoot();
			System.out.println("Current docClass: " + currentDocClass.info());
			Snapshot<ZoneInstance> zones = (Snapshot) currentDocClass.getHolders(root.find(ZoneGeneric.class));
			if (zones == null)
				return Observable.just(Snapshot.empty());
			return Observable.just((Snapshot) zones.sorted());
		}
	}

	public static class ZONE_SELECTOR_BEST implements ObservableListExtractor {
		@SuppressWarnings({ "unchecked", "rawtypes" })
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			Generic currentDoc = generics[0];
			Root root = currentDoc.getRoot();
			System.out.println("Document: " + currentDoc.info());
			Snapshot<ZoneTextInstance> zoneTextInstances = (Snapshot) currentDoc.getHolders(root.find(ZoneText.class));
			if (zoneTextInstances == null)
				return Observable.just(Snapshot.empty());
			return Observable.just((Snapshot) zoneTextInstances.filter(zt -> "best".equals(zt.getImgFilter().getValue())).sort((g1, g2) -> Integer.compare(g1.getZoneNum(), g2.getZoneNum())));
		}
	}

	public static class ZONE_SELECTOR_REALITY implements ObservableListExtractor {
		@SuppressWarnings({ "unchecked", "rawtypes" })
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			Generic currentDoc = generics[0];
			Root root = currentDoc.getRoot();
			System.out.println("Document: " + currentDoc.info());
			Snapshot<ZoneTextInstance> zoneTextInstances = (Snapshot) currentDoc.getHolders(root.find(ZoneText.class));
			if (zoneTextInstances == null)
				return Observable.just(Snapshot.empty());
			long start = System.nanoTime();
			Snapshot<Generic> ol = (Snapshot) zoneTextInstances.filter(zt -> "reality".equals(zt.getImgFilter().getValue())).sort((g1, g2) -> Integer.compare(g1.getZoneNum(), g2.getZoneNum()));
			long stop = System.nanoTime();
			System.out.println("--------- zone selector: " + (stop - start) / 1_000_000 + "ms");
			return Observable.just(ol);
		}
	}

	/*
	 * === DATALIST ===
	 */

	public static class DATALIST_SELECTOR implements ObservableListExtractor {
		@SuppressWarnings({ "unchecked", "rawtypes" })
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			ZoneTextInstance zti = (ZoneTextInstance) generics[0];
			Generic currentDoc = generics[1];
			Root root = currentDoc.getRoot();
			Predicate<ZoneTextInstance> filterByZone = z -> z.getZoneNum() == zti.getZoneNum() && !z.getValue().toString().isEmpty();
			Snapshot<ZoneTextInstance> zoneTextInstances = (Snapshot) currentDoc.getHolders(root.find(ZoneText.class));
			return Observable.just(zoneTextInstances.filter(filterByZone).sort((g1, g2) -> Collator.getInstance().compare(g1.getValue(), g2.getValue())));
		}
	}

	/*
	 * === OCR TEXT ===
	 */

	public static class OCR_SELECTOR implements ObservableListExtractor {
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			Root root = generics[0].getRoot();
			Snapshot<Generic> filters = root.find(ImgFilter.class).getInstances();
			if (filters == null)
				return Observable.just(Snapshot.empty());
			return Observable.just(filters);
		}
	}

	/*
	 * === SCORE ===
	 */

	public static class SCORE_SELECTOR implements ObservableListExtractor {
		@SuppressWarnings({ "unchecked", "rawtypes" })
		@Override
		public Observable<Snapshot<Generic>> apply(Generic[] generics) {
			ZoneInstance zoneInstance = (ZoneInstance) generics[0];
			Root root = zoneInstance.getRoot();
			System.out.println("Current zone: " + zoneInstance.info());
			Snapshot<ScoreInstance> scores = (Snapshot) zoneInstance.getHolders(root.find(Score.class));
			// scores.forEach(g -> System.out.println(g.info()));
			if (scores == null)
				return Observable.just(Snapshot.empty());
			return Observable.just((Snapshot) scores);
		}
	}

}
