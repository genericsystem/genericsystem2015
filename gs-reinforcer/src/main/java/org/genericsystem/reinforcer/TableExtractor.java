package org.genericsystem.reinforcer;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

// Recognize tables, extract lines
public class TableExtractor {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	public static List<Table> extractTables(Labels labels) {
		List<List<Label>> lines = labels.groupByLine();
		List<Block> blocks = new ArrayList<>();
		Block currentBlock = null;

		logger.info("Lines found: {}", lines);
		// Group lines into blocks.
		for (List<Label> line : lines) {
			if (line.size() > 1) {
				if (currentBlock != null)
					currentBlock.addLine(line);
				else {
					currentBlock = new Block();
					currentBlock.addLine(line);
				}
			} else {
				if (currentBlock != null)
					blocks.add(currentBlock);
				blocks.add(new Block().addLine(line));
				currentBlock = null;
			}
		}
		if (currentBlock != null)
			blocks.add(currentBlock);

		logger.info("Blocks: {}", blocks);

		// TODO: Merge successive blocks that could belong to the same table.

		List<Table> tables = new ArrayList<>();
		for (Block block : blocks)
			if (block.size() > 1)
				tables.add(new Table(block.createColumns()));

		logger.info("Tables found: {}", tables);
		return tables;
	}

	public static class Block {
		List<List<Label>> lines = new ArrayList<>();

		public Block addLine(List<Label> line) {
			lines.add(line);
			return this;
		}

		public int size() {
			return lines.size();
		}

		List<Column> createColumns() {
			List<Column> columns = new ArrayList<>();
			int lineNo = 0;
			for (List<Label> line : lines) {
				for (Label label : line) {
					Column column = findColumn(columns, label);
					if (column != null)
						column.addContent(lineNo, label);
					else {
						column = new Column();
						column.addContent(lineNo, label);
						columns.add(column);
					}
				}
				lineNo++;
			}
			return columns;
		}

		private Column findColumn(List<Column> columns, Label label) {
			for (Column column : columns)
				if (column.overlaps(label))
					return column;
			return null;
		}

		@Override
		public String toString() {
			return "\nBlock: " + lines;
		}
	}

	public static class Table {
		private final List<Column> columns;

		public Table(List<Column> columns) {
			this.columns = columns;
		}

		public int nbColumns() {
			return columns.size();
		}

		@Override
		public String toString() {
			return "Table " + columns;
		}
	}
	public static class Column {
		private Map<Integer, Cell> cells = new HashMap<>();
		private double minX = Double.MAX_VALUE;
		private double maxX = 0;

		void addContent(int lineNo, Label label) {
			if (cells.containsKey(lineNo))
				cells.get(lineNo).addContent(label);
			else {
				Cell newCell = new Cell(lineNo);
				newCell.addContent(label);
				cells.put(lineNo, newCell);
			}
			double x = label.getRect().getX();
			double x2 = x + label.getRect().getWidth();
			if (x < minX)
				minX = x;
			if (x2 > maxX)
				maxX = x2;
		}

		public boolean overlaps(Label label) {
			return label.getRect().br().getX() > minX && label.getRect().getX() < maxX;
		}

		@Override
		public String toString() {
			return "\nColumn: minX: " + minX + ", maxX: " + maxX + ", cells: " + cells.toString();
		}
	}

	public static class Cell {
		private int line;
		private List<Label> contents = new ArrayList<>();

		public Cell(int line) {
			this.line = line;
		}

		void addContent(Label label) {
			contents.add(label);
		}

		@Override
		public String toString() {
			return "Cell, line " + line + ": " + contents.toString();
		}
	}
}
