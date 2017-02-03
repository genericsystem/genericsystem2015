// package org.genericsystem.defaults.tools;
//
// import java.util.Collections;
// import java.util.List;
//
// import com.sun.javafx.collections.NonIterableChange;
// import javafx.collections.ObservableList;
//
// public class SimplePermutationChange<E> extends NonIterableChange<E>{
//
// private final int[] permutation;
//
// public SimplePermutationChange(int from, int to, int[] permutation, ObservableList<E> list) {
// super(from, to, list);
// this.permutation = permutation;
// }
//
//
// @Override
// public List<E> getRemoved() {
// checkState();
// return Collections.<E>emptyList();
// }
//
// @Override
// protected int[] getPermutation() {
// checkState();
// return permutation;
// }
// }
