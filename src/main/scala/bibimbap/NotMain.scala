package bibimbap

import jline._

object NotMain {
  def run {
    // var mask : Character = _
    // var trigger : String = _

    val reader = new ConsoleReader
    reader.setBellEnabled(false)

    val completor = new SimpleCompletor(Array[String](
      "foo", "bar", "baz"
    ))

    reader.addCompletor(completor)

    reader.readLine("Try me.")
  }
}
/*  
        List completors = new LinkedList();

        if (args.length > 0) {
            if (args[0].equals("none")) {
            } else if (args[0].equals("files")) {
                completors.add(new FileNameCompletor());
            } else if (args[0].equals("classes")) {
                completors.add(new ClassNameCompletor());
            } else if (args[0].equals("dictionary")) {
                completors.add(new SimpleCompletor(
                        new GZIPInputStream(Example.class
                                .getResourceAsStream("english.gz"))));
            } else if (args[0].equals("simple")) {
                completors.add(new SimpleCompletor(new String[] {
                        "foo", "bar", "baz" }));
            } else {
                usage();

                return;
            }
        }

        if (args.length == 3) {
            mask = new Character(args[2].charAt(0));
            trigger = args[1];
        }

        reader.addCompletor(new ArgumentCompletor(completors));

        String line;
        PrintWriter out = new PrintWriter(System.out);

        while ((line = reader.readLine("prompt> ")) != null) {
            out.println("======>\"" + line + "\"");
            out.flush();

            // If we input the special word then we will mask
            // the next line.
            if ((trigger != null) && (line.compareTo(trigger) == 0)) {
                line = reader.readLine("password> ", mask);
            }
            if (line.equalsIgnoreCase("quit")
                    || line.equalsIgnoreCase("exit")) {
                break;
            }
        }
    }
}

*/
