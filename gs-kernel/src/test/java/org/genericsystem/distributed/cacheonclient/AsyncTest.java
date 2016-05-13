//package org.genericsystem.distributed.cacheonclient;
//
//import java.util.NavigableSet;
//import java.util.TreeSet;
//import java.util.concurrent.CompletableFuture;
//
//import org.genericsystem.common.Generic;
//import org.testng.annotations.Test;
//
//@Test
//public class AsyncTest extends AbstractTest {
//
//	public void test001() throws InterruptedException {
//		CocClientEngine engine = new CocClientEngine();
//		new TreeLoader().traverse(engine).thenAccept(set -> System.out.println(set));
//		Thread.sleep(200);// to avoid garbaging
//	}
//
//	public CompletableFuture<NavigableSet<Generic>> getAllDependencies(Generic generic) {
//		return new TreeLoader().traverse(generic);
//	};
//
//	class TreeLoader extends TreeSet<Generic> {
//		public CompletableFuture<NavigableSet<Generic>> traverse(Generic generic) {
//			add(generic);
//			CompletableFuture<NavigableSet<Generic>> cf = new CompletableFuture<NavigableSet<Generic>>();
//			((CocClientEngine) generic.getRoot()).getCurrentCache().getDependenciesPromise(generic).thenAccept(snapshot -> {
//				CompletableFuture<NavigableSet<Generic>> internal = CompletableFuture.completedFuture(this);
//				for (Generic element : snapshot)
//					// internal = internal.thenCompose((a) -> traverse(element));
//					internal = internal.thenCombine(traverse(element), (a, b) -> this);
//				internal.thenRun(() -> cf.complete(this));
//			});
//			return cf;
//		}
//	}
//
//	class TreeLoader2 extends TreeSet<Generic> {
//		public CompletableFuture<NavigableSet<Generic>> traverse(Generic generic) {
//			add(generic);
//			CompletableFuture<NavigableSet<Generic>> cf = new CompletableFuture<NavigableSet<Generic>>();
//			((CocClientEngine) generic.getRoot()).getCurrentCache().getDependenciesPromise(generic).thenAccept(snapshot -> {
//				CompletableFuture<NavigableSet<Generic>> internal = CompletableFuture.completedFuture(this);
//				for (Generic element : snapshot)
//					internal = internal.thenCompose((a) -> traverse(element));
//				// internal = internal.thenACombine(traverse(element), (a, b) -> this);
//					internal.thenRun(() -> cf.complete(this));
//				});
//			return cf;
//		}
//	}
//}
