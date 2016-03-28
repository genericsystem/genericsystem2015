package org.genericsystem.defaults.tools;

import java.lang.ref.WeakReference;
import java.util.Arrays;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;

import com.sun.javafx.binding.ExpressionHelperBase;

/**
 * @author Nicolas Feybesse
 *
 * @param
 */
public class ObservableBase implements Observable {
	public static ObservableBase createObservable(Observable... observables) {
		return new ObservableBase(observables);
	}

	private BindingHelperObserver observer;

	protected ObservableBase(Observable... observables) {
		bind(observables);
	}

	protected final void bind(Observable... dependencies) {
		if ((dependencies != null) && (dependencies.length > 0)) {
			if (observer == null) {
				observer = new BindingHelperObserver(this);
			}
			for (final Observable dep : dependencies) {
				if (dep != null) {
					dep.addListener(observer);
				}
			}
		}
	}

	protected final void unbind(Observable... dependencies) {
		if (observer != null) {
			for (final Observable dep : dependencies) {
				if (dep != null) {
					dep.removeListener(observer);
				}
			}
			observer = null;
		}
	}

	private static class BindingHelperObserver implements InvalidationListener {

		private final WeakReference<ObservableBase> ref;

		public BindingHelperObserver(ObservableBase binding) {
			if (binding == null) {
				throw new NullPointerException("Binding has to be specified.");
			}
			ref = new WeakReference<>(binding);
		}

		@Override
		public void invalidated(Observable observable) {
			final ObservableBase binding = ref.get();
			if (binding == null) {
				observable.removeListener(this);
			} else {
				binding.invalidate();
			}
		}
	}

	private ExpressionHelper helper;

	@Override
	public void addListener(InvalidationListener listener) {
		helper = ExpressionHelper.addListener(helper, this, listener);
	}

	public void invalidate() {
		// System.out.println("fireInvalidationEvent");
		fireInvalidationEvent();
	}

	@Override
	public void removeListener(InvalidationListener listener) {
		helper = ExpressionHelper.removeListener(helper, listener);
	}

	protected void fireInvalidationEvent() {
		ExpressionHelper.fireValueChangedEvent(helper);
	}

	@SuppressWarnings("restriction")
	public static abstract class ExpressionHelper extends ExpressionHelperBase {

		public static ExpressionHelper addListener(ExpressionHelper helper, Observable observable, InvalidationListener listener) {
			if ((observable == null) || (listener == null)) {
				throw new NullPointerException();
			}
			return (helper == null) ? new SingleInvalidation(observable, listener) : helper.addListener(listener);
		}

		public static ExpressionHelper removeListener(ExpressionHelper helper, InvalidationListener listener) {
			if (listener == null) {
				throw new NullPointerException();
			}
			return (helper == null) ? null : helper.removeListener(listener);
		}

		public static void fireValueChangedEvent(ExpressionHelper helper) {
			if (helper != null) {
				helper.fireInvalidationEvent();
			}
		}

		protected final Observable observable;

		private ExpressionHelper(Observable observable) {
			this.observable = observable;
		}

		protected abstract ExpressionHelper addListener(InvalidationListener listener);

		protected abstract ExpressionHelper removeListener(InvalidationListener listener);

		protected abstract void fireInvalidationEvent();

		private static class SingleInvalidation extends ExpressionHelper {

			private final InvalidationListener listener;

			private SingleInvalidation(Observable expression, InvalidationListener listener) {
				super(expression);
				this.listener = listener;
			}

			@Override
			protected ExpressionHelper addListener(InvalidationListener listener) {
				return new Generic(observable, this.listener, listener);
			}

			@Override
			protected ExpressionHelper removeListener(InvalidationListener listener) {
				return (listener.equals(this.listener)) ? null : this;
			}

			@Override
			protected void fireInvalidationEvent() {
				try {
					listener.invalidated(observable);
				} catch (Exception e) {
					Thread.currentThread().getUncaughtExceptionHandler().uncaughtException(Thread.currentThread(), e);
				}
			}
		}

		private static class Generic extends ExpressionHelper {

			private InvalidationListener[] invalidationListeners;
			private int invalidationSize;
			private boolean locked;

			private Generic(Observable observable, InvalidationListener listener0, InvalidationListener listener1) {
				super(observable);
				this.invalidationListeners = new InvalidationListener[] { listener0, listener1 };
				this.invalidationSize = 2;
			}

			@Override
			protected Generic addListener(InvalidationListener listener) {
				if (invalidationListeners == null) {
					invalidationListeners = new InvalidationListener[] { listener };
					invalidationSize = 1;
				} else {
					final int oldCapacity = invalidationListeners.length;
					if (locked) {
						final int newCapacity = (invalidationSize < oldCapacity) ? oldCapacity : (oldCapacity * 3) / 2 + 1;
						invalidationListeners = Arrays.copyOf(invalidationListeners, newCapacity);
					} else if (invalidationSize == oldCapacity) {
						invalidationSize = trim(invalidationSize, invalidationListeners);
						if (invalidationSize == oldCapacity) {
							final int newCapacity = (oldCapacity * 3) / 2 + 1;
							invalidationListeners = Arrays.copyOf(invalidationListeners, newCapacity);
						}
					}
					invalidationListeners[invalidationSize++] = listener;
				}
				return this;
			}

			@Override
			protected ExpressionHelper removeListener(InvalidationListener listener) {
				if (invalidationListeners != null) {
					for (int index = 0; index < invalidationSize; index++) {
						if (listener.equals(invalidationListeners[index])) {
							if (invalidationSize == 1) {
								invalidationListeners = null;
								invalidationSize = 0;
							} else if ((invalidationSize == 2)) {
								return new SingleInvalidation(observable, invalidationListeners[1 - index]);
							} else {
								final int numMoved = invalidationSize - index - 1;
								final InvalidationListener[] oldListeners = invalidationListeners;
								if (locked) {
									invalidationListeners = new InvalidationListener[invalidationListeners.length];
									System.arraycopy(oldListeners, 0, invalidationListeners, 0, index);
								}
								if (numMoved > 0) {
									System.arraycopy(oldListeners, index + 1, invalidationListeners, index, numMoved);
								}
								invalidationSize--;
								if (!locked) {
									invalidationListeners[invalidationSize] = null; // Let gc do its work
								}
							}
							break;
						}
					}
				}
				return this;
			}

			@Override
			protected void fireInvalidationEvent() {
				final InvalidationListener[] curInvalidationList = invalidationListeners;
				final int curInvalidationSize = invalidationSize;

				try {
					locked = true;
					for (int i = 0; i < curInvalidationSize; i++) {
						try {
							curInvalidationList[i].invalidated(observable);
						} catch (Exception e) {
							Thread.currentThread().getUncaughtExceptionHandler().uncaughtException(Thread.currentThread(), e);
						}
					}
				} finally {
					locked = false;
				}
			}
		}
	}
}