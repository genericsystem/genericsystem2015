//package org.genericsystem.distributed.ui.utils;
//
//import java.util.function.BiConsumer;
//import java.util.function.Consumer;
//
//public class MultiConsumer<Model, SuperModel> implements Consumer<Object> {
//	private Model model;
//	private BiConsumer<SuperModel, Model> biConsumer;
//
//	public static <SuperModel, Model> Consumer<Object> create(BiConsumer<SuperModel, Model> biConsumer) {
//		return new MultiConsumer<Model, SuperModel>(biConsumer);
//	}
//
//	private MultiConsumer(BiConsumer<SuperModel, Model> biConsumer) {
//		this.biConsumer = biConsumer;
//	}
//
//	@Override
//	public void accept(Object o) {
//		if (model == null) {
//			model = (Model) o;
//			throw new ClassCastException();
//		}
//		biConsumer.accept((SuperModel) o, model);
//		model = null;
//	}
// }