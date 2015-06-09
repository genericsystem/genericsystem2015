//package org.genericsystem.defaults;
//
//import java.io.Serializable;
//import java.util.Objects;
//
//public interface DefaultGenerator<T extends DefaultVertex<T>> {
//
//	Serializable generate(T type);
//
//	public static class IntAutoIncrementGenerator<T extends DefaultVertex<T>> implements DefaultGenerator<T> {
//
//		@Override
//		public Serializable generate(T type) {
//			return incrementedValue(type);
//		}
//
//		protected int incrementedValue(T type) {
//			T sequence = type.getRoot().getSequence();
//			T sequenceHolder = type.getHolders(sequence).first();
//			int value = sequenceHolder != null ? (Integer) sequenceHolder.getValue() + 1 : 0;
//			type.setHolder(sequence, value);
//			return value;
//		}
//
//		public static class StringAutoIncrementGenerator<T extends DefaultVertex<T>> extends IntAutoIncrementGenerator<T> {
//			@Override
//			public Serializable generate(T type) {
//				Serializable value = incrementedValue(type);
//				return type.getValue() instanceof Class<?> ? ((Class<?>) type.getValue()).getSimpleName() + "-" + value : Objects.toString(type.getValue() + "-" + value);
//			}
//		}
//	}
// }
