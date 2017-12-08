package org.genericsystem.reinforcer;

import java.util.List;

import org.genericsystem.reinforcer.TableExtractor.Table;
import org.testng.annotations.Test;

@Test
public class Tables {

	// 4 labels at the same places in 4 all images, 2 classes depending on text.
	public void test001() {
		Labels labels = new Labels();
		labels.addLabel(1, 1, 50, 20, "Surname;");
		labels.addLabel(55, 1, 100, 20, "Kenobi");
		labels.addLabel(1, 22, 50, 42, "Name:");
		labels.addLabel(55, 22, 100, 42, "Obi-Wan");

		List<Table> tables1 = TableExtractor.extractTables(labels);
		assert tables1.size() == 1;
		assert tables1.get(0).nbColumns() == 2;
	}

	// 4 labels at roughly the same places in all images, 2 classes depending on text.
	public void test002() {
		Labels labels1 = new Labels();
		labels1.addLabel(5, 2, 50, 21, "Surname;");
		labels1.addLabel(60, 3, 110, 19, "Kenobi");
		labels1.addLabel(6, 24, 54, 44, "Name.");
		labels1.addLabel(60, 23, 104, 46, "Obi-Wan");
		Labels labels2 = new Labels();
		labels2.addLabel(5, 3, 106, 39, "Surnarne:");
		labels2.addLabel(114, 4, 207, 41, "Vader");
		labels2.addLabel(6, 45, 108, 83, "Name:");
		labels2.addLabel(117, 43, 210, 85, "Darth");
		Labels labels3 = new Labels();
		labels3.addLabel(6, 1, 78, 27, "Surname;");
		labels3.addLabel(84, 6, 144, 33, "Name:");
		labels3.addLabel(2, 33, 81, 60, "Kenobi");
		labels3.addLabel(84, 36, 151, 69, "Obi-Wan");
		Labels labels4 = new Labels();
		labels4.addLabel(20, 42, 69, 59, "Surname;");
		labels4.addLabel(76, 43, 121, 60, "Name:");
		labels4.addLabel(21, 62, 72, 83, "Vader");
		labels4.addLabel(74, 65, 123, 81, "Darth");

		Labels labels5 = new Labels();
		labels5.addLabel(1, 1, 50, 20, "Surname;");
		labels5.addLabel(55, 1, 100, 20, "Name:");
		labels5.addLabel(1, 22, 50, 42, "Skywalker");
		labels5.addLabel(55, 22, 100, 42, "Luke");
		Labels labels6 = new Labels();
		labels6.addLabel(1, 1, 50, 20, "Surnarne;");
		labels6.addLabel(55, 1, 100, 20, "Name:");
		labels6.addLabel(1, 22, 50, 42, "Yoda");
		labels6.addLabel(55, 22, 100, 42, "Master");
		Labels labels7 = new Labels();
		labels7.addLabel(1, 5, 50, 30, "Sumame;");
		labels7.addLabel(55, 7, 100, 29, "Skywalker");
		labels7.addLabel(1, 30, 50, 50, "Name:");
		labels7.addLabel(55, 31, 100, 49, "Luke");
		Labels labels8 = new Labels();
		labels8.addLabel(1, 1, 50, 20, "Surname;");
		labels8.addLabel(55, 1, 100, 20, "Yoda");
		labels8.addLabel(1, 22, 50, 42, "Name:");
		labels8.addLabel(55, 22, 100, 42, "Master");

		List<Table> tables1 = TableExtractor.extractTables(labels1);
		assert tables1.size() == 1;
		assert tables1.get(0).nbColumns() == 2;
		List<Table> tables2 = TableExtractor.extractTables(labels2);
		assert tables2.size() == 1;
		assert tables2.get(0).nbColumns() == 2;
		List<Table> tables3 = TableExtractor.extractTables(labels3);
		assert tables3.size() == 1;
		assert tables3.get(0).nbColumns() == 2;
		List<Table> tables4 = TableExtractor.extractTables(labels4);
		assert tables4.size() == 1;
		assert tables4.get(0).nbColumns() == 2;
		List<Table> tables5 = TableExtractor.extractTables(labels5);
		assert tables5.size() == 1;
		assert tables5.get(0).nbColumns() == 2;
		List<Table> tables6 = TableExtractor.extractTables(labels6);
		assert tables6.size() == 1;
		assert tables6.get(0).nbColumns() == 2;
		List<Table> tables7 = TableExtractor.extractTables(labels7);
		assert tables7.size() == 1;
		assert tables7.get(0).nbColumns() == 2;
		List<Table> tables8 = TableExtractor.extractTables(labels8);
		assert tables8.size() == 1;
		assert tables8.get(0).nbColumns() == 2;
	}

	// Varying length
	public void test003() {
		Labels labels1 = new Labels();
		labels1.addLabel(1, 1, 50, 20, "Name(s|;");
		labels1.addLabel(55, 1, 100, 20, "Smith");
		Labels labels2 = new Labels();
		labels2.addLabel(1, 1, 50, 20, "Name(s):");
		labels2.addLabel(55, 1, 300, 20, "Jones Mason Lopez");

		// Tests
		Labels labels3 = new Labels();
		labels3.addLabel(1, 1, 50, 20, "Name(s):");
		labels3.addLabel(55, 1, 100, 20, "Quinn");
		Labels labels4 = new Labels();
		labels4.addLabel(1, 1, 50, 20, "Narne(s):");
		labels4.addLabel(55, 1, 200, 20, "Irwin White");
		Labels labels5 = new Labels();
		labels5.addLabel(1, 1, 50, 20, "Narne(s):");
		labels5.addLabel(55, 1, 400, 20, "Irwin John Mark White");

		List<Table> tables1 = TableExtractor.extractTables(labels1);
		assert tables1.size() == 0;
		List<Table> tables2 = TableExtractor.extractTables(labels2);
		assert tables2.size() == 0;
		List<Table> tables3 = TableExtractor.extractTables(labels3);
		assert tables3.size() == 0;
		List<Table> tables4 = TableExtractor.extractTables(labels4);
		assert tables4.size() == 0;
		List<Table> tables5 = TableExtractor.extractTables(labels5);
		assert tables5.size() == 0;
	}

	public void test004() {
		Labels labels1 = new Labels();
		labels1.addLabel(1, 1, 100, 20, "Took");
		labels1.addLabel(1, 22, 80, 42, "Peregrin");
		labels1.addLabel(1, 42, 140, 64, "2990-");
		Labels labels2 = new Labels();
		labels2.addLabel(1, 1, 100, 20, "Brandybuck");
		labels2.addLabel(1, 22, 80, 42, "Meriadoc");
		labels2.addLabel(1, 42, 140, 64, "2982-");

		// Tests
		Labels labels3 = new Labels();
		labels3.addLabel(1, 1, 100, 20, "Gamgee");
		labels3.addLabel(1, 22, 80, 42, "Samwise");
		labels3.addLabel(1, 42, 140, 64, "29680922-");
		Labels labels4 = new Labels();
		labels4.addLabel(1, 1, 100, 20, "Baggins");
		labels4.addLabel(1, 22, 80, 42, "Frodo");
		labels4.addLabel(1, 42, 140, 64, "29800406-");

		List<Table> tables1 = TableExtractor.extractTables(labels1);
		assert tables1.size() == 0;
		List<Table> tables2 = TableExtractor.extractTables(labels2);
		assert tables2.size() == 0;
		List<Table> tables3 = TableExtractor.extractTables(labels3);
		assert tables3.size() == 0;
		List<Table> tables4 = TableExtractor.extractTables(labels4);
		assert tables4.size() == 0;
	}

	// One column vs. two-column layout
	public void test005() {
		Labels labels1 = new Labels();
		labels1.addLabel(1, 1, 100, 20, "Took");
		labels1.addLabel(1, 22, 80, 42, "Peregrin");
		labels1.addLabel(1, 42, 140, 64, "2990-");
		Labels labels2 = new Labels();
		labels2.addLabel(1, 1, 100, 20, "Brandybuck");
		labels2.addLabel(1, 22, 80, 42, "Meriadoc");
		labels2.addLabel(1, 42, 140, 64, "2982-");

		// Tests
		Labels labels3 = new Labels();
		labels3.addLabel(1, 1, 100, 20, "Gamgee");
		labels3.addLabel(1, 22, 80, 42, "Samwise");
		labels3.addLabel(1, 42, 140, 64, "29680922-");
		Labels labels4 = new Labels();
		labels4.addLabel(1, 1, 100, 20, "Baggins");
		labels4.addLabel(1, 22, 80, 42, "Frodo");
		labels4.addLabel(1, 42, 140, 64, "29800406-");

		Labels labels5 = new Labels();
		labels5.addLabel(1, 1, 100, 20, "Took");
		labels5.addLabel(200, 1, 350, 20, "Peregrin");
		labels5.addLabel(1, 22, 180, 42, "Brandybuck");
		labels5.addLabel(203, 22, 405, 42, "Meriadoc");
		Labels labels6 = new Labels();
		labels6.addLabel(1, 1, 100, 20, "Baggins");
		labels6.addLabel(200, 1, 350, 20, "Frodo");
		labels6.addLabel(1, 22, 180, 42, "Gamgee");
		labels6.addLabel(203, 22, 405, 42, "Samwise");

		Labels labels7 = new Labels();
		labels7.addLabel(1, 1, 100, 20, "Baggins");
		labels7.addLabel(200, 1, 350, 20, "Bilbo");
		labels7.addLabel(1, 22, 180, 42, "Took");
		labels7.addLabel(203, 22, 405, 42, "Belladonna");
		Labels labels8 = new Labels();
		labels8.addLabel(1, 1, 100, 20, "Bolger");
		labels8.addLabel(200, 1, 361, 19, "Fredegar");
		labels8.addLabel(1, 22, 180, 42, "Gardner");
		labels8.addLabel(198, 21, 342, 43, "Rosie");

		List<Table> tables1 = TableExtractor.extractTables(labels1);
		assert tables1.size() == 0;
		List<Table> tables2 = TableExtractor.extractTables(labels2);
		assert tables2.size() == 0;
		List<Table> tables3 = TableExtractor.extractTables(labels3);
		assert tables3.size() == 0;
		List<Table> tables4 = TableExtractor.extractTables(labels4);
		assert tables4.size() == 0;
		List<Table> tables5 = TableExtractor.extractTables(labels5);
		assert tables5.size() == 1;
		assert tables5.get(0).nbColumns() == 2;
		List<Table> tables6 = TableExtractor.extractTables(labels6);
		assert tables6.size() == 1;
		assert tables6.get(0).nbColumns() == 2;
		List<Table> tables7 = TableExtractor.extractTables(labels7);
		assert tables7.size() == 1;
		assert tables7.get(0).nbColumns() == 2;
		List<Table> tables8 = TableExtractor.extractTables(labels8);
		assert tables8.size() == 1;
		assert tables8.get(0).nbColumns() == 2;
	}

	public void test006() {
		Labels labels1 = new Labels();
		labels1.addLabel(1, 1, 100, 20, "Took");
		labels1.addLabel(1, 22, 120, 42, "Peregrin");
		labels1.addLabel(1, 42, 140, 64, "2990-");
		Labels labels2 = new Labels();
		labels2.addLabel(1, 1, 200, 20, "Brandybuck");
		labels2.addLabel(1, 22, 200, 42, "Meriadoc");
		labels2.addLabel(1, 42, 140, 64, "2982-");

		// Tests
		Labels labels3 = new Labels();
		labels3.addLabel(1, 1, 150, 20, "Gamgee");
		labels3.addLabel(1, 22, 180, 42, "Samwise");
		labels3.addLabel(1, 42, 200, 64, "29680922-");
		Labels labels4 = new Labels();
		labels4.addLabel(1, 1, 160, 20, "Baggins");
		labels4.addLabel(1, 22, 90, 42, "Frodo");
		labels4.addLabel(1, 42, 210, 64, "29800406-");

		Labels labels5 = new Labels();
		labels5.addLabel(1, 1, 100, 20, "Took");
		labels5.addLabel(200, 1, 350, 20, "Peregrin");
		labels5.addLabel(1, 22, 180, 42, "Brandybuck");
		labels5.addLabel(203, 22, 405, 42, "Meriadoc");
		Labels labels6 = new Labels();
		labels6.addLabel(1, 1, 100, 20, "Baggins");
		labels6.addLabel(200, 1, 350, 20, "Frodo");
		labels6.addLabel(1, 22, 180, 42, "Gamgee");
		labels6.addLabel(203, 22, 405, 42, "Samwise");

		Labels labels7 = new Labels();
		labels7.addLabel(1, 1, 100, 20, "Baggins");
		labels7.addLabel(200, 1, 350, 20, "Bilbo");
		labels7.addLabel(1, 22, 180, 42, "Took");
		labels7.addLabel(203, 22, 405, 42, "Belladonna");
		Labels labels8 = new Labels();
		labels8.addLabel(1, 1, 100, 20, "Bolger");
		labels8.addLabel(200, 1, 361, 19, "Fredegar");
		labels8.addLabel(1, 22, 180, 42, "Gardner");
		labels8.addLabel(198, 21, 342, 43, "Rosie");

		List<Table> tables1 = TableExtractor.extractTables(labels1);
		assert tables1.size() == 0;
		List<Table> tables2 = TableExtractor.extractTables(labels2);
		assert tables2.size() == 0;
		List<Table> tables3 = TableExtractor.extractTables(labels3);
		assert tables3.size() == 0;
		List<Table> tables4 = TableExtractor.extractTables(labels4);
		assert tables4.size() == 0;
		List<Table> tables5 = TableExtractor.extractTables(labels5);
		assert tables5.size() == 1;
		assert tables5.get(0).nbColumns() == 2;
		List<Table> tables6 = TableExtractor.extractTables(labels6);
		assert tables6.size() == 1;
		assert tables6.get(0).nbColumns() == 2;
		List<Table> tables7 = TableExtractor.extractTables(labels7);
		assert tables7.size() == 1;
		assert tables7.get(0).nbColumns() == 2;
		List<Table> tables8 = TableExtractor.extractTables(labels8);
		assert tables8.size() == 1;
		assert tables8.get(0).nbColumns() == 2;
	}

	public void test007() {
		Labels labels1 = new Labels();
		labels1.addLabel(21, 1, 120, 20, "Took");
		labels1.addLabel(21, 22, 140, 42, "Peregrin");
		labels1.addLabel(21, 42, 160, 64, "2990-");
		Labels labels2 = new Labels();
		labels2.addLabel(1, 1, 200, 20, "Brandybuck");
		labels2.addLabel(1, 22, 200, 42, "Meriadoc");
		labels2.addLabel(1, 42, 140, 64, "2982-");
		labels2.addLabel(1, 66, 100, 83, "H");

		// Tests
		Labels labels3 = new Labels();
		labels3.addLabel(1, 31, 150, 49, "Gamgee");
		labels3.addLabel(1, 52, 180, 71, "Samwise");
		labels3.addLabel(1, 72, 200, 95, "29680922-");
		Labels labels4 = new Labels();
		labels4.addLabel(1, 1, 160, 20, "Baggins");
		labels4.addLabel(1, 22, 90, 42, "Frodo");
		labels4.addLabel(1, 42, 210, 64, "29800406-");

		Labels labels5 = new Labels();
		labels5.addLabel(1, 1, 100, 20, "Took");
		labels5.addLabel(200, 1, 350, 20, "Peregrin");
		labels5.addLabel(355, 1, 450, 20, "Merry");
		labels5.addLabel(1, 22, 180, 42, "Brandybuck");
		labels5.addLabel(203, 22, 405, 42, "Meriadoc");
		Labels labels6 = new Labels();
		labels6.addLabel(1, 1, 100, 20, "Baggins");
		labels6.addLabel(200, 1, 350, 20, "Frodo");
		labels6.addLabel(1, 22, 180, 42, "Gamgee");
		labels6.addLabel(203, 22, 405, 42, "Samwise");

		Labels labels7 = new Labels();
		labels7.addLabel(1, 1, 100, 20, "Baggins");
		labels7.addLabel(200, 1, 350, 20, "Bilbo");
		labels7.addLabel(355, 1, 450, 20, "Frodo");
		labels7.addLabel(1, 22, 180, 42, "Took");
		labels7.addLabel(203, 22, 405, 42, "Belladonna");
		Labels labels8 = new Labels();
		labels8.addLabel(1, 1, 100, 20, "Bolger");
		labels8.addLabel(200, 1, 361, 19, "Fredegar");
		labels8.addLabel(1, 22, 180, 42, "Gardner");
		labels8.addLabel(198, 21, 342, 43, "Rosie");

		List<Table> tables1 = TableExtractor.extractTables(labels1);
		assert tables1.size() == 0;
		List<Table> tables2 = TableExtractor.extractTables(labels2);
		assert tables2.size() == 0;
		List<Table> tables3 = TableExtractor.extractTables(labels3);
		assert tables3.size() == 0;
		List<Table> tables4 = TableExtractor.extractTables(labels4);
		assert tables4.size() == 0;
		List<Table> tables5 = TableExtractor.extractTables(labels5);
		assert tables5.size() == 1;
		assert tables5.get(0).nbColumns() == 2;
		List<Table> tables6 = TableExtractor.extractTables(labels6);
		assert tables6.size() == 1;
		assert tables6.get(0).nbColumns() == 2;
		List<Table> tables7 = TableExtractor.extractTables(labels7);
		assert tables7.size() == 1;
		assert tables7.get(0).nbColumns() == 2;
		List<Table> tables8 = TableExtractor.extractTables(labels8);
		assert tables8.size() == 1;
		assert tables8.get(0).nbColumns() == 2;
	}

	public void test008() {
		Labels labels = new Labels();
		labels.addLabel(1, 1, 50, 20, "Surname;");
		labels.addLabel(55, 1, 100, 20, "Name:");
		labels.addLabel(105, 1, 120, 20, "?");
		labels.addLabel(1, 22, 50, 42, "Skywalker");
		labels.addLabel(55, 22, 100, 42, "Luke");
		labels.addLabel(105, 22, 120, 42, "A");
		labels.addLabel(1, 44, 50, 64, "Vader");
		labels.addLabel(55, 44, 100, 64, "Darth");
		labels.addLabel(1, 66, 50, 88, "Kenobi");
		labels.addLabel(55, 66, 100, 88, "Obi-Wan");
		labels.addLabel(105, 66, 120, 88, "B");

		List<Table> tables1 = TableExtractor.extractTables(labels);
		assert tables1.size() == 1;
		assert tables1.get(0).nbColumns() == 3;
	}
}
