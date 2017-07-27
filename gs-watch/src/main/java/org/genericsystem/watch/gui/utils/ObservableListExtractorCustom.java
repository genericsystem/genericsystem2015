package org.genericsystem.watch.gui.utils;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
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

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

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
		public ObservableList<Generic> apply(Generic[] generics) {
			Root root = generics[0].getRoot();
			DocClass docClass = root.find(DocClass.class);
			Snapshot<Generic> docClassInstances = docClass.getInstances();
			if (null == docClassInstances)
				return FXCollections.emptyObservableList();
			return docClassInstances.toObservableList();
		}
	}

	public static class DOC_SELECTOR implements ObservableListExtractor {
		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			DocClassInstance currentDocClass = (DocClassInstance) generics[0];
			Doc doc = generics[0].getRoot().find(Doc.class);
			if (null == currentDocClass)
				return FXCollections.emptyObservableList();
			System.out.println("Current doc class : " + currentDocClass.info());
			Snapshot<Generic> docInstances = currentDocClass.getHolders(doc);
			if (null == docInstances)
				return FXCollections.emptyObservableList();
			return docInstances.toObservableList();

		}
	}

	/*
	 * === ZONES ===
	 */

	public static class ZONE_SELECTOR implements ObservableListExtractor {
		@SuppressWarnings({ "unchecked", "rawtypes" })
		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			Generic currentDocClass = generics[0];
			Root root = currentDocClass.getRoot();
			System.out.println("Current docClass: " + currentDocClass.info());
			Snapshot<ZoneInstance> zones = (Snapshot) currentDocClass.getHolders(root.find(ZoneGeneric.class));
			if (zones == null)
				return FXCollections.emptyObservableList();
			return (ObservableList) zones.toObservableList().sorted();
		}
	}

	public static class ZONE_SELECTOR_BEST implements ObservableListExtractor {
		@SuppressWarnings({ "unchecked", "rawtypes" })
		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			Generic currentDoc = generics[0];
			Root root = currentDoc.getRoot();
			System.out.println("Document: " + currentDoc.info());
			Snapshot<ZoneTextInstance> zoneTextInstances = (Snapshot) currentDoc.getHolders(root.find(ZoneText.class));
			if (zoneTextInstances == null)
				return FXCollections.emptyObservableList();
			return (ObservableList) zoneTextInstances.toObservableList().filtered(zt -> "best".equals(zt.getImgFilter().getValue())).sorted((g1, g2) -> Integer.compare(g1.getZoneNum(), g2.getZoneNum()));
		}
	}

	public static class ZONE_SELECTOR_REALITY implements ObservableListExtractor {
		@SuppressWarnings({ "unchecked", "rawtypes" })
		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			Generic currentDoc = generics[0];
			Root root = currentDoc.getRoot();
			System.out.println("Document: " + currentDoc.info());
			Snapshot<ZoneTextInstance> zoneTextInstances = (Snapshot) currentDoc.getHolders(root.find(ZoneText.class));
			if (zoneTextInstances == null)
				return FXCollections.emptyObservableList();
			long start = System.nanoTime();
			ObservableList ol = zoneTextInstances.toObservableList().filtered(zt -> "reality".equals(zt.getImgFilter().getValue())).sorted((g1, g2) -> Integer.compare(g1.getZoneNum(), g2.getZoneNum()));
			long stop = System.nanoTime();
			System.out.println("--------- zone selector: " + (stop - start) / 1_000_000 + "ms");
			return ol;
		}
	}

	/*
	 * === DATALIST ===
	 */

	public static class DATALIST_SELECTOR implements ObservableListExtractor {
		@SuppressWarnings({ "unchecked", "rawtypes" })
		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			ZoneTextInstance zti = (ZoneTextInstance) generics[0];
			Generic currentDoc = generics[1];
			Root root = currentDoc.getRoot();
			Predicate<ZoneTextInstance> filterByZone = z -> z.getZoneNum() == zti.getZoneNum() && !z.getValue().toString().isEmpty();
			Snapshot<ZoneTextInstance> zoneTextInstances = (Snapshot) currentDoc.getHolders(root.find(ZoneText.class));
			return (ObservableList) zoneTextInstances.toObservableList().filtered(filterByZone.and(distinctByKey(g -> g.getValue()))).sorted((g1, g2) -> Integer.compare(g1.getZoneNum(), g2.getZoneNum()));
		}

		/**
		 * Utility function that can be used to filter a stream to get distinct values, using a personalized function.
		 * 
		 * @param keyExtractor - lambda expression that will provide the filter criteria
		 * @return a predicate
		 */
		public <T> Predicate<T> distinctByKey(Function<? super T, Object> keyExtractor) {
			Map<Object, Boolean> seen = new ConcurrentHashMap<>();
			return t -> seen.putIfAbsent(keyExtractor.apply(t), Boolean.TRUE) == null;
		}
	}

	/*
	 * === OCR TEXT ===
	 */

	public static class OCR_SELECTOR implements ObservableListExtractor {
		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			Root root = generics[0].getRoot();
			Snapshot<Generic> filters = root.find(ImgFilter.class).getInstances();
			if (filters == null)
				return FXCollections.emptyObservableList();
			return filters.toObservableList();
		}
	}

	/*
	 * === SCORE ===
	 */

	public static class SCORE_SELECTOR implements ObservableListExtractor {
		@SuppressWarnings({ "unchecked", "rawtypes" })
		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			ZoneInstance zoneInstance = (ZoneInstance) generics[0];
			Root root = zoneInstance.getRoot();
			System.out.println("Current zone: " + zoneInstance.info());
			Snapshot<ScoreInstance> scores = (Snapshot) zoneInstance.getHolders(root.find(Score.class));
			// scores.forEach(g -> System.out.println(g.info()));
			if (scores == null)
				return FXCollections.emptyObservableList();
			return (ObservableList) scores.toObservableList();
		}
	}

}
