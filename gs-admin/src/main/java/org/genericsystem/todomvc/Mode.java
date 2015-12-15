package org.genericsystem.todomvc;

import java.util.function.Predicate;

interface Mode {
	Predicate<Todo> predicate();

	static Mode ALL = new All();
	static Mode ACTIVE = new Active();
	static Mode COMPLETE = new Complete();

	static class All implements Mode {
		@Override
		public Predicate<Todo> predicate() {
			return todo -> true;
		}
	}

	static class Active implements Mode {
		@Override
		public Predicate<Todo> predicate() {
			return todo -> !todo.completed.getValue();
		}
	}

	static class Complete implements Mode {
		@Override
		public Predicate<Todo> predicate() {
			return todo -> todo.completed.getValue();
		}
	}
}