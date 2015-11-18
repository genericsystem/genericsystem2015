package org.genericsystem.distributed.cacheonclient;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;

import javafx.beans.binding.Bindings;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.observables.ContainerObservableSnapshot;
import org.genericsystem.distributed.cacheonclient.observables.ObservableSnapshot;
import org.testng.annotations.Test;

@Test
public class ObservableSnapshotTest extends AbstractTest {

	public Set<Generic> binder(ObservableSnapshot<Generic> obsSnapshot) {
		Set<Generic> setChecker = new HashSet<>();
		Bindings.bindContent(setChecker, obsSnapshot);
		return setChecker;
	}

	public void test001_ContainerObservableSnapshotAddTest() throws InterruptedException {

		CocClientEngine engine = new CocClientEngine();

		ContainerObservableSnapshot<Generic> containerObsSnapshot = new ContainerObservableSnapshot<>();
		Set<Generic> setChecker = binder(containerObsSnapshot);

		containerObsSnapshot.add(engine);

		if (setChecker.size() == 0) {
			Thread.sleep(100);
		}
		assert setChecker.size() == 1;
	}

	public void test002_ContainerObservableSnapshotAddAllRemoveTest() throws InterruptedException {

		CocClientEngine engine = new CocClientEngine();

		ContainerObservableSnapshot<Generic> containerObsSnapshot = new ContainerObservableSnapshot<>();
		Set<Generic> setChecker = binder(containerObsSnapshot);

		containerObsSnapshot.add(engine);
		containerObsSnapshot.add(engine.addInstance("instance1"));
		containerObsSnapshot.add(engine.addInstance("instance2"));
		
		
		List<Generic> list = new ArrayList<>();
		list.add(engine.addInstance("instance3"));
		list.add(engine.addInstance("instance4"));
		list.add(engine.addInstance("instance5"));
		list.add(engine.addInstance("instance6"));
		list.add(engine.addInstance("instance7"));
		list.add(engine.addInstance("instance8"));
		containerObsSnapshot.addAll(list);
		
		if (setChecker.size() != 9) {
			Thread.sleep(100);
		}
		assert setChecker.size() == 9;

		containerObsSnapshot.remove(engine);
		if (containerObsSnapshot.size() != 8) {
			Thread.sleep(100);
		}
		assert containerObsSnapshot.size() == 8;
	}

//	 public void test003_ConcatObservableSnapshotTest() throws InterruptedException {
//	
//	 CocClientEngine engine = new CocClientEngine();
//	
//	 ContainerObservableSnapshot<Generic> containerObsSnapshot = new ContainerObservableSnapshot<>();
//	 ContainerObservableSnapshot<Generic> containerObsSnapshot2 = new ContainerObservableSnapshot<>();
//	
//	 Set<Generic> setChecker = binder(containerObsSnapshot.concat(containerObsSnapshot2));
//
//	 // containerObsSnapshot.addListener((c) -> setChecker = binder(containerObsSnapshot.concat(containerObsSnapshot2)));
//	 // containerObsSnapshot2.addListener((c) -> setChecker = binder(containerObsSnapshot.concat(containerObsSnapshot2)));
//	 
//	 // containerObsSnapshot.add(engine.addInstance("containerObsSnapshotInstance1"));
//	 containerObsSnapshot2.add(engine.addInstance("containerObsSnapshotInstance7"));
//	 // containerObsSnapshot.add(engine.addInstance("containerObsSnapshotInstance2"));
//	 // containerObsSnapshot.add(engine.addInstance("containerObsSnapshotInstance3"));
//	 // containerObsSnapshot.add(engine.addInstance("containerObsSnapshotInstance4"));
//	 // containerObsSnapshot.add(engine.addInstance("containerObsSnapshotInstance5"));
//	 // containerObsSnapshot2.add(engine.addInstance("containerObsSnapshotInstance8"));
//	 containerObsSnapshot2.add(engine.addInstance("containerObsSnapshotInstance9"));
//	 // containerObsSnapshot.add(engine.addInstance("containerObsSnapshotInstance6"));
//	 // containerObsSnapshot2.add(engine.addInstance("containerObsSnapshotInstance10"));
//	
//	 if (setChecker.size() != 2) {
//	 Thread.sleep(100);
//	 }
//	 System.out.println("setChecker" + setChecker.size());
//	 assert setChecker.size() == 2;
//	 }

	public void test004_FilterObservableSnapshotTest() throws InterruptedException {

		CocClientEngine engine = new CocClientEngine();

		ContainerObservableSnapshot<Generic> containerObsSnapshot = new ContainerObservableSnapshot<>();
		ContainerObservableSnapshot<Generic> removesObservableList = new ContainerObservableSnapshot<>();// si ObservableSnapshot, add est sur le set...
	
		Set<Generic> lastPredicate = binder(removesObservableList);
		Predicate<Generic> predicate = x -> !lastPredicate.contains(x);
		removesObservableList.addListener((c) -> predicate = x -> !lastPredicate.contains(x););
		
		Set<Generic> setChecker = binder(containerObsSnapshot.filtered(predicate));

		containerObsSnapshot.add(engine.addInstance("containerObsSnapshotInstance1"));
		containerObsSnapshot.add(engine.addInstance("containerObsSnapshotInstance2"));
		containerObsSnapshot.add(engine.addInstance("containerObsSnapshotInstance3"));
		Generic g4 = engine.addInstance("containerObsSnapshotInstance4");
		containerObsSnapshot.add(g4);
		removesObservableList.add(g4);

		if (setChecker.size() != 3) {
			Thread.sleep(100);
		}
		assert setChecker.size() == 3;
		

		containerObsSnapshot.add(engine.addInstance("containerObsSnapshotInstance5"));
		containerObsSnapshot.add(engine.addInstance("containerObsSnapshotInstance6"));
		Generic g7 = engine.addInstance("containerObsSnapshotInstance7");
		containerObsSnapshot.add(g7);
		containerObsSnapshot.add(engine.addInstance("containerObsSnapshotInstance8"));
		containerObsSnapshot.add(engine.addInstance("containerObsSnapshotInstance9"));

		if (setChecker.size() != 8) {
			Thread.sleep(100);
		}
		assert setChecker.size() == 8;
		
		removesObservableList.add(g7);

		if (setChecker.size() != 7) {
			Thread.sleep(100);
		}
		assert setChecker.size() == 7;

	}

	//

	public void testultimate() throws InterruptedException {

		ObservableList<Object> ol0 = FXCollections.observableArrayList();

		ol0.addListener((ListChangeListener<Object>) (c) -> {
			while (c.next()) {
				System.out.println("c.toString() : " + c.toString());
				System.out.println("c.getClass() : " + c.getClass());
				System.out.println("c.wasAdded() : " + c.wasAdded());
				System.out.println("c.wasRemoved() : " + c.wasRemoved());
				System.out.println("c.getFrom() : " + c.getFrom());
				System.out.println("c.getTo() : " + c.getTo());
				System.out.println("c.getAddedSize() : " + c.getAddedSize());
				System.out.println("c.getAddedSubList() : " + c.getAddedSubList());
				System.out.println("c.getRemovedSize() : " + c.getRemovedSize());
				System.out.println("c.getRemoved() : " + c.getRemoved());
			}
		});

		System.out.println("---------------  add 0");
		ol0.add(0);
		System.out.println("---------------  addAll 1,2,3,4");
		ol0.addAll(1, 2, 3, 4);
		System.out.println("---------------  remove i=3");
		ol0.remove(3);
		System.out.println("---------------  setAll 8,9,10");
		ol0.setAll(Arrays.asList(8, 9, 10));

	}
}
