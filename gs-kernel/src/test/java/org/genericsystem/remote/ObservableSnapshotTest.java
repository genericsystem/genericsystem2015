package org.genericsystem.remote;

import org.testng.annotations.Test;

@Test
public class ObservableSnapshotTest extends AbstractTest {
	//
	// public void test001_ContainerObservableSnapshotAddTest() throws InterruptedException {
	//
	// ContainerObservableSnapshot<Integer> containerObsSnapshot = new ContainerObservableSnapshot<>();
	// Set<Integer> setToCheck = new LinkedHashSet<>();
	// Set<Integer> setChecker = new LinkedHashSet<>();
	// Bindings.bindContent(setToCheck, containerObsSnapshot);
	//
	// containerObsSnapshot.add(1);
	// setChecker.add(1);
	//
	// containerObsSnapshot.add(2);
	// setChecker.add(2);
	//
	// containerObsSnapshot.add(3);
	// setChecker.add(3);
	//
	// containerObsSnapshot.remove(2);
	// setChecker.remove(2);
	//
	// containerObsSnapshot.add(4);
	// setChecker.add(4);
	//
	// containerObsSnapshot.add(5);
	// setChecker.add(5);
	//
	// containerObsSnapshot.add(6);
	// setChecker.add(6);
	//
	// containerObsSnapshot.add(7);
	// setChecker.add(7);
	//
	// containerObsSnapshot.add(8);
	// setChecker.add(8);
	//
	// containerObsSnapshot.add(9);
	// setChecker.add(9);
	//
	// containerObsSnapshot.remove(1);
	// setChecker.remove(1);
	//
	// containerObsSnapshot.remove(5);
	// setChecker.remove(5);
	//
	// containerObsSnapshot.remove(9);
	// setChecker.remove(9);
	//
	// assert setToCheck.equals(containerObsSnapshot);
	// assert new ArrayList<>(setToCheck).equals(new ArrayList<>(containerObsSnapshot));
	//
	// containerObsSnapshot.clear();
	// containerObsSnapshot.addAll(Arrays.asList(1, 5, 8));
	// setToCheck.clear();
	// setToCheck.addAll(Arrays.asList(1, 5, 8));
	//
	// assert setToCheck.size() == 3;
	// assert setToCheck.equals(containerObsSnapshot);
	// assert new ArrayList<>(setToCheck).equals(new ArrayList<>(containerObsSnapshot));
	//
	// }
	//
	// public void test002_FilterObservableSnapshotTest() throws InterruptedException {
	//
	// ContainerObservableSnapshot<Integer> containerObsSnapshot = new ContainerObservableSnapshot<>();
	// Predicate<Integer> predicate = x -> (x % 3 == 0);
	// Set<Integer> setToCheck = new LinkedHashSet<>();
	// Bindings.bindContent(setToCheck, containerObsSnapshot.filtered(predicate));
	//
	// List<Integer> setChecker = new LinkedList<>();
	//
	// containerObsSnapshot.addAll(Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12));
	// containerObsSnapshot.add(13);
	// containerObsSnapshot.add(14);
	// setChecker.addAll(Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12));
	// setChecker.add(13);
	// setChecker.add(14);
	//
	// Set<Integer> filteredSetChecker = setChecker.stream().filter(predicate).collect(Collectors.toSet());
	//
	// assert setToCheck.size() == filteredSetChecker.size();
	// assert setToCheck.equals(filteredSetChecker);
	// assert new ArrayList<>(setToCheck).equals(new ArrayList<>(filteredSetChecker));
	// }
	//
	// public void test003_ObservableFilterObservableSnapshotTest() throws InterruptedException {
	//
	// ContainerObservableSnapshot<Integer> containerObsSnapshot = new ContainerObservableSnapshot<>();
	// Predicate<Integer> predicate = x -> (x % 3 == 0);
	// SimpleObjectProperty<Predicate<Integer>> obsPredicate = new SimpleObjectProperty<>(predicate);
	// Set<Integer> setToCheck = new LinkedHashSet<>();
	// Bindings.bindContent(setToCheck, containerObsSnapshot.filtered(obsPredicate));
	//
	// List<Integer> setChecker = new LinkedList<>();
	//
	// containerObsSnapshot.addAll(Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12));
	// containerObsSnapshot.remove(3);
	// setChecker.addAll(Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12));
	// setChecker.remove(3);
	// Set<Integer> filteredSetChecker = setChecker.stream().filter(predicate).collect(Collectors.toSet());
	//
	// assert setToCheck.size() == filteredSetChecker.size();
	// assert setToCheck.equals(filteredSetChecker);
	// assert new ArrayList<>(setToCheck).equals(new ArrayList<>(filteredSetChecker));
	//
	// predicate = x -> (x % 2 == 0);
	// obsPredicate.set(predicate);
	// filteredSetChecker = setChecker.stream().filter(predicate).collect(Collectors.toSet());
	//
	// assert setToCheck.size() == filteredSetChecker.size();
	// assert setToCheck.equals(filteredSetChecker);
	// assert new ArrayList<>(setToCheck).equals(new ArrayList<>(filteredSetChecker));
	// }
	//
	// public void test004_ConcatObservableSnapshotTest() throws InterruptedException {
	//
	// ContainerObservableSnapshot<Integer> containerObsSnapshot = new ContainerObservableSnapshot<>();
	// ContainerObservableSnapshot<Integer> containerObsSnapshot2 = new ContainerObservableSnapshot<>();
	// Set<Integer> setToCheck = new LinkedHashSet<>();
	// Bindings.bindContent(setToCheck, containerObsSnapshot.concat(containerObsSnapshot2));
	//
	// List<Integer> setChecker = new LinkedList<>();
	//
	// containerObsSnapshot.addAll(Arrays.asList(0, 1, 2, 3, 4, 5));
	// setChecker.addAll(Arrays.asList(0, 1, 2, 3, 4, 5));
	// containerObsSnapshot2.addAll(Arrays.asList(6, 7, 8, 9, 10, 11, 12));
	// setChecker.addAll(Arrays.asList(6, 7, 8, 9, 10, 11, 12));
	//
	// assert new ArrayList<>(setChecker).equals(new ArrayList<>(setToCheck));
	// }
	//
	// public void test006_ObservableListSnapshotTest() throws InterruptedException {
	//
	// ContainerObservableSnapshot<Integer> containerObsSnapshot = new ContainerObservableSnapshot<>();
	// List<Integer> listToCheck = containerObsSnapshot.toObservableList();
	//
	// containerObsSnapshot.add(0);
	// containerObsSnapshot.addAll(Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9));
	// containerObsSnapshot.remove(2);
	// containerObsSnapshot.remove(9);
	//
	// assert listToCheck.size() == containerObsSnapshot.size();
	// assert new ArrayList<>(containerObsSnapshot).equals(listToCheck);
	// }
	//
	// @Test(invocationCount = 5)
	// public void test007_ObservableEngineDependenciesTest() throws InterruptedException {
	// CocClientEngine engine = new CocClientEngine();
	//
	// ObservableList<Generic> dependenciesObservableList = engine.getCurrentCache().getDependenciesObservableList(engine);
	//
	// if (!engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList))
	// Thread.sleep(100);
	// assert engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList);
	// }
	//
	// @Test(invocationCount = 5)
	// public void test008_ObservableEngineDependenciesAddTest() throws InterruptedException {
	// CocClientEngine engine = new CocClientEngine();
	//
	// ObservableList<Generic> dependenciesObservableList = engine.getCurrentCache().getDependenciesObservableList(engine);
	//
	// if (!engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList))
	// Thread.sleep(100);
	// assert engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList);
	//
	// engine.addInstance("Instance0");
	// engine.addInstance("Instance1");
	// engine.addInstance("Instance2");
	//
	// if (!engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList))
	// Thread.sleep(100);
	// assert engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList);
	// }
	//
	// @Test(invocationCount = 5)
	// public void test009_ObservableEngineDependenciesRemoveTest() throws InterruptedException, ConcurrencyControlException {
	// CocClientEngine engine = new CocClientEngine();
	//
	// ObservableList<Generic> dependenciesObservableList = engine.getCurrentCache().getDependenciesObservableList(engine);
	//
	// engine.addInstance("Instance0");
	// engine.addInstance("Instance1");
	// Generic g2 = engine.addInstance("Instance2");
	// engine.addInstance("Instance3");
	// engine.addInstance("Instance4");
	//
	// if (!engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList))
	// Thread.sleep(100);
	// assert engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList);
	//
	// g2.remove();
	//
	// if (!engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList))
	// Thread.sleep(100);
	// assert engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList);
	// }
	//
	// @Test(invocationCount = 5)
	// public void test010_ObservableEngineDependenciesDifferentialTest() throws InterruptedException, ConcurrencyControlException {
	// CocClientEngine engine = new CocClientEngine();
	//
	// ObservableList<Generic> dependenciesObservableList = engine.getCurrentCache().getDependenciesObservableList(engine);
	//
	// engine.addInstance("InstanceL1_0");
	// engine.addInstance("InstanceL1_1");
	// Generic l1g2 = engine.addInstance("InstanceL1_2");
	// engine.addInstance("InstanceL1_3");
	// Generic l1g4 = engine.addInstance("InstanceL1_4");
	//
	// engine.getCurrentCache().mount();
	//
	// Generic l2g0 = engine.addInstance("InstanceL2_0");
	// Generic l2g1 = engine.addInstance("InstanceL2_1");
	// engine.addInstance("InstanceL2_2");
	// l2g0.remove();
	// l1g2.remove();
	//
	// engine.getCurrentCache().mount();
	//
	// l2g1.remove();
	// engine.addInstance("InstanceL3_0");
	// engine.addInstance("InstanceL3_1");
	// l1g4.remove();
	// engine.addInstance("InstanceL3_2");
	//
	// if (!engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList))
	// Thread.sleep(100);
	// assert engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList);
	// }
	//
	// @Test(invocationCount = 1)
	// public void test011_ObservableEngineDependenciesClearTest() throws InterruptedException, ConcurrencyControlException {
	// CocClientEngine engine = new CocClientEngine();
	//
	// ObservableList<Generic> dependenciesObservableList = engine.getCurrentCache().getDependenciesObservableList(engine);
	//
	// engine.addInstance("InstanceL1_0");
	// engine.addInstance("InstanceL1_1");
	// Generic l1g2 = engine.addInstance("InstanceL1_2");
	// engine.addInstance("InstanceL1_3");
	// engine.addInstance("InstanceL1_4");
	//
	// engine.getCurrentCache().mount();
	//
	// Generic l2g0 = engine.addInstance("InstanceL2_0");
	// Generic l2g1 = engine.addInstance("InstanceL2_1");
	// engine.addInstance("InstanceL2_2");
	// l2g0.remove();
	// l1g2.remove();
	//
	// engine.getCurrentCache().mount();
	//
	// l2g1.remove();
	// engine.addInstance("InstanceL3_0");
	// engine.addInstance("InstanceL3_1");
	// engine.addInstance("InstanceL3_2");
	//
	// engine.getCurrentCache().clear();
	//
	// if (!engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList))
	// Thread.sleep(100);
	// assert engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList);
	// }
	//
	// @Test(invocationCount = 1)
	// public void test012_ObservableEngineDependenciesShiftTsTest() throws InterruptedException, ConcurrencyControlException {
	// CocClientEngine engine = new CocClientEngine();
	//
	// ObservableList<Generic> dependenciesObservableList = engine.getCurrentCache().getDependenciesObservableList(engine);
	//
	// engine.addInstance("InstanceL1_0");
	// engine.addInstance("InstanceL1_1");
	// Generic l1g2 = engine.addInstance("InstanceL1_2");
	// engine.addInstance("InstanceL1_3");
	// engine.addInstance("InstanceL1_4");
	//
	// engine.getCurrentCache().shiftTs();
	//
	// Generic l2g0 = engine.addInstance("InstanceL2_0");
	// Generic l2g1 = engine.addInstance("InstanceL2_1");
	// engine.addInstance("InstanceL2_2");
	// l2g0.remove();
	// l1g2.remove();
	//
	// engine.getCurrentCache().mount();
	//
	// l2g1.remove();
	// engine.addInstance("InstanceL3_0");
	// engine.addInstance("InstanceL3_1");
	// engine.addInstance("InstanceL3_2");
	//
	// if (!engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList))
	// Thread.sleep(100);
	// assert engine.getCurrentCache().getDependencies(engine).toList().equals(dependenciesObservableList);
	// }
}
