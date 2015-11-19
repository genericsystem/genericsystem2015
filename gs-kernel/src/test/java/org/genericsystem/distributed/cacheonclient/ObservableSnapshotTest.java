package org.genericsystem.distributed.cacheonclient;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import javafx.beans.binding.Bindings;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.ObservableList;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.observables.ContainerObservableSnapshot;
import org.testng.annotations.Test;

@Test
public class ObservableSnapshotTest extends AbstractTest {

	public void test001_ContainerObservableSnapshotAddTest() throws InterruptedException {

		ContainerObservableSnapshot<Integer> containerObsSnapshot = new ContainerObservableSnapshot<>();
		Set<Integer> setToCheck = new LinkedHashSet<>();
		Set<Integer> setChecker = new LinkedHashSet<>();
		Bindings.bindContent(setToCheck, containerObsSnapshot);

		containerObsSnapshot.add(1);
		setChecker.add(1);

		containerObsSnapshot.add(2);
		setChecker.add(2);

		containerObsSnapshot.add(3);
		setChecker.add(3);

		containerObsSnapshot.remove(2);
		setChecker.remove(2);

		containerObsSnapshot.add(4);
		setChecker.add(4);

		containerObsSnapshot.add(5);
		setChecker.add(5);

		containerObsSnapshot.add(6);
		setChecker.add(6);

		containerObsSnapshot.add(7);
		setChecker.add(7);

		containerObsSnapshot.add(8);
		setChecker.add(8);

		containerObsSnapshot.add(9);
		setChecker.add(9);

		containerObsSnapshot.remove(1);
		setChecker.remove(1);

		containerObsSnapshot.remove(5);
		setChecker.remove(5);

		containerObsSnapshot.remove(9);
		setChecker.remove(9);

		assert setToCheck.equals(containerObsSnapshot);
		assert new ArrayList<>(setToCheck).equals(new ArrayList<>(containerObsSnapshot));

		containerObsSnapshot.clear();
		containerObsSnapshot.addAll(Arrays.asList(1, 5, 8));
		setToCheck.clear();
		setToCheck.addAll(Arrays.asList(1, 5, 8));

		assert setToCheck.size() == 3;
		assert setToCheck.equals(containerObsSnapshot);
		assert new ArrayList<>(setToCheck).equals(new ArrayList<>(containerObsSnapshot));

	}

	public void test002_FilterObservableSnapshotTest() throws InterruptedException {

		ContainerObservableSnapshot<Integer> containerObsSnapshot = new ContainerObservableSnapshot<>();
		Predicate<Integer> predicate = x -> (x % 3 == 0);
		Set<Integer> setToCheck = new LinkedHashSet<>();
		Bindings.bindContent(setToCheck, containerObsSnapshot.filtered(predicate));

		List<Integer> setChecker = new LinkedList<>();

		containerObsSnapshot.addAll(Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12));
		containerObsSnapshot.add(13);
		containerObsSnapshot.add(14);
		setChecker.addAll(Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12));
		setChecker.add(13);
		setChecker.add(14);

		Set<Integer> filteredSetChecker = setChecker.stream().filter(predicate).collect(Collectors.toSet());

		assert setToCheck.size() == filteredSetChecker.size();
		assert setToCheck.equals(filteredSetChecker);
		assert new ArrayList<>(setToCheck).equals(new ArrayList<>(filteredSetChecker));
	}

	public void test003_ObservableFilterObservableSnapshotTest() throws InterruptedException {

		ContainerObservableSnapshot<Integer> containerObsSnapshot = new ContainerObservableSnapshot<>();
		Predicate<Integer> predicate = x -> (x % 3 == 0);
		SimpleObjectProperty<Predicate<Integer>> obsPredicate = new SimpleObjectProperty<>(predicate);
		Set<Integer> setToCheck = new LinkedHashSet<>();
		Bindings.bindContent(setToCheck, containerObsSnapshot.filtered(obsPredicate));

		List<Integer> setChecker = new LinkedList<>();

		containerObsSnapshot.addAll(Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12));
		containerObsSnapshot.remove(3);
		setChecker.addAll(Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12));
		setChecker.remove(3);
		Set<Integer> filteredSetChecker = setChecker.stream().filter(predicate).collect(Collectors.toSet());

		assert setToCheck.size() == filteredSetChecker.size();
		assert setToCheck.equals(filteredSetChecker);
		assert new ArrayList<>(setToCheck).equals(new ArrayList<>(filteredSetChecker));

		predicate = x -> (x % 2 == 0);
		obsPredicate.set(predicate);
		filteredSetChecker = setChecker.stream().filter(predicate).collect(Collectors.toSet());

		assert setToCheck.size() == filteredSetChecker.size();
		assert setToCheck.equals(filteredSetChecker);
		assert new ArrayList<>(setToCheck).equals(new ArrayList<>(filteredSetChecker));
	}

	public void test004_ConcatObservableSnapshotTest() throws InterruptedException {

		ContainerObservableSnapshot<Integer> containerObsSnapshot = new ContainerObservableSnapshot<>();
		ContainerObservableSnapshot<Integer> containerObsSnapshot2 = new ContainerObservableSnapshot<>();
		Set<Integer> setToCheck = new LinkedHashSet<>();
		Bindings.bindContent(setToCheck, containerObsSnapshot.concat(containerObsSnapshot2));

		List<Integer> setChecker = new LinkedList<>();

		containerObsSnapshot.addAll(Arrays.asList(0, 1, 2, 3, 4, 5));
		setChecker.addAll(Arrays.asList(0, 1, 2, 3, 4, 5));
		containerObsSnapshot2.addAll(Arrays.asList(6, 7, 8, 9, 10, 11, 12));
		setChecker.addAll(Arrays.asList(6, 7, 8, 9, 10, 11, 12));

		assert new ArrayList<>(setChecker).equals(new ArrayList<>(setToCheck));
	}

	public void test006_ObservableListSnapshotTest() throws InterruptedException {

		ContainerObservableSnapshot<Integer> containerObsSnapshot = new ContainerObservableSnapshot<>();
		List<Integer> listToCheck = containerObsSnapshot.toObservableList();

		containerObsSnapshot.add(0);
		containerObsSnapshot.addAll(Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9));
		containerObsSnapshot.remove(2);
		containerObsSnapshot.remove(9);

		assert listToCheck.size() == containerObsSnapshot.size();
		assert new ArrayList<>(containerObsSnapshot).equals(listToCheck);
	}

	@Test(invocationCount = 5)
	public void test007_EngineDependenciesTest() throws InterruptedException {
		CocClientEngine engine = new CocClientEngine();

		ObservableList<Generic> dependenciesObservableList = engine.getCurrentCache().getWrappableDependenciesSnap(engine);

		if (dependenciesObservableList.size() == 0) {
			Thread.sleep(100);
		}

		assert dependenciesObservableList.size() != 0;

		assert engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList);
	}

	@Test(invocationCount = 5)
	public void test008_ObservableEngineDependenciesAddTest() throws InterruptedException {
		CocClientEngine engine = new CocClientEngine();

		ObservableList<Generic> dependenciesObservableList = engine.getCurrentCache().getWrappableDependenciesSnap(engine);

		if (!engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList)) {
			Thread.sleep(100);
		}

		assert engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList);

		engine.addInstance("Instance0");
		engine.addInstance("Instance1");
		engine.addInstance("Instance2");

		if (!engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList)) {
			Thread.sleep(100);
		}

		assert engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList);
	}

	@Test(invocationCount = 5)
	public void test009_ObservableEngineDependenciesRemoveTest() throws InterruptedException, ConcurrencyControlException {
		CocClientEngine engine = new CocClientEngine();

		ObservableList<Generic> dependenciesObservableList = engine.getCurrentCache().getWrappableDependenciesSnap(engine);

		engine.addInstance("Instance0");
		engine.addInstance("Instance1");
		Generic g2 = engine.addInstance("Instance2");
		engine.addInstance("Instance3");
		engine.addInstance("Instance4");
		if (!engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList)) {
			Thread.sleep(100);
		}

		assert engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList);

		g2.remove();
		if (!engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList)) {
			Thread.sleep(100);
		}
		assert engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList);
	}

	// public void test010_ObservableEngineDependenciesDifferentialTest() throws InterruptedException, ConcurrencyControlException {
	// CocClientEngine engine = new CocClientEngine();
	//
	// ObservableList<Generic> dependenciesObservableList = engine.getCurrentCache().getWrappableDependenciesSnap(engine);
	//
	// engine.addInstance("InstanceL1_0");
	// engine.addInstance("InstanceL1_1");
	// Generic g2 = engine.addInstance("InstanceL1_2");
	// engine.addInstance("InstanceL1_3");
	// engine.addInstance("InstanceL1_4");
	//
	// if (!engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList)) {
	// Thread.sleep(100);
	// }
	//
	// assert engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList);
	//
	// System.out.println(engine.getCurrentCache().getDependencies(engine).toList());
	// System.out.println(dependenciesObservableList);
	// System.out.println("_____2ND_LEVEL_____");
	//
	// engine.getCurrentCache().asyncMount();
	//
	// engine.addInstance("InstanceL2_0");
	// engine.addInstance("InstanceL2_1");
	// engine.addInstance("InstanceL2_2");
	// // g2.remove();
	// if (!engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList)) {
	// Thread.sleep(100);
	// }
	// System.out.println(engine.getCurrentCache().getDependencies(engine).toList());
	// System.out.println(dependenciesObservableList);
	//
	// assert engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList);
	// }

	//
	//
	//
	//
	// public void testultimate() throws InterruptedException {
	//
	// ObservableList<Object> ol0 = FXCollections.observableArrayList();
	//
	// ol0.addListener((ListChangeListener<Object>) (c) -> {
	// while (c.next()) {
	// System.out.println("c.toString() : " + c.toString());
	// System.out.println("c.getClass() : " + c.getClass());
	// System.out.println("c.wasAdded() : " + c.wasAdded());
	// System.out.println("c.wasRemoved() : " + c.wasRemoved());
	// System.out.println("c.getFrom() : " + c.getFrom());
	// System.out.println("c.getTo() : " + c.getTo());
	// System.out.println("c.getAddedSize() : " + c.getAddedSize());
	// System.out.println("c.getAddedSubList() : " + c.getAddedSubList());
	// System.out.println("c.getRemovedSize() : " + c.getRemovedSize());
	// System.out.println("c.getRemoved() : " + c.getRemoved());
	// }
	// });
	//
	// System.out.println("---------------  add 0");
	// ol0.add(0);
	// System.out.println("---------------  addAll 1,2,3,4");
	// ol0.addAll(1, 2, 3, 4);
	// System.out.println("---------------  remove i=3");
	// ol0.remove(3);
	// System.out.println("---------------  setAll 8,9,10");
	// ol0.setAll(Arrays.asList(8, 9, 10));
	//
	// }
}
