import ammonite.ops._
import io.circe.Json
import io.circe.yaml._

object Main extends App {

  val path = Path.expandUser(args.head)

  val files = ls.rec(path)

  def fix(file: Path): Unit = {
    println(s"fixing $file")
    val lines = read.lines(file)

    // assumes the first line is a ---
    val front :: back :: Nil = lines.foldLeft(Seq.empty[String]) { case (parts, line) =>
      if (line == "---") {
        parts :+ ""
      }
      else {
        val index = parts.size - 1
        val updated = parts(index) + "\n" + line
        parts.updated(index, updated)
      }
    }

    val json = parser.parse(front)

    if (json.isLeft) {
      println((front, json))
    }

    val jsonCursor = json.toOption.get.hcursor
    val title = jsonCursor.downField("title")
    val date = jsonCursor.downField("date")
    val url = jsonCursor.downField("url")
    val categories = jsonCursor.downField("categories")

    val updatedJson = Json.fromFields(Seq(
      "title" -> title.focus.get,
      "date" -> date.focus.get,
      "url" -> url.focus.get,
      "tags" -> categories.focus.get
    ))

    val updatedFront = printer.print(updatedJson)

    val updatedBack = back
      .replaceAll("&#8217;", "'").replaceAll("&#8230;", "...").replaceAll("&#8220;", "\"").replaceAll("&#8221;", "\"").replaceAll("&#8211;", "-")
      .replaceAll("<pre lang=\"scala\">", "```scala\n")
      .replaceAll("<pre lang=\"java\">", "```java\n")
      .replaceAll("<pre lang=\"console\">", "```bash\n")
      .replaceAll("<pre lang=\"bash\">", "```bash\n")
      .replaceAll("<pre lang=\"BASH\">", "```bash\n")
      .replaceAll("<pre lang=\"groovy\">", "```groovy\n")
      .replaceAll("<pre lang=\"sql\">", "```sql\n")
      .replaceAll("<pre lang=\"routes\">", "```routes\n")
      .replaceAll("<pre lang=\"props\">", "```properties\n")
      .replaceAll("<pre lang=\"properties\">", "```properties\n")
      .replaceAll("<pre lang=\"JavaScript\">", "```javascript\n")
      .replaceAll("<pre lang=\"javascript\">", "```javascript\n")
      .replaceAll("<pre lang=\"ruby\">", "```ruby\n")
      .replaceAll("<pre lang=\"html\">", "```html\n")
      .replaceAll("<pre lang=\"apex\">", "```apex\n")
      .replaceAll("<pre lang=\"curl\">", "```curl\n")
      .replaceAll("<pre lang=\"conf\">", "```config\n")
      .replaceAll("<pre lang=\"text\">", "```txt\n")
      .replaceAll("<pre lang=\"TXT\">", "```txt\n")
      .replaceAll("<pre lang=\"json\">", "```json\n")
      .replaceAll("<pre lang=\"yaml\">", "```yaml\n")
      .replaceAll("<pre lang=\"yml\">", "```yaml\n")
      .replaceAll("<pre lang=\"YML\">", "```yaml\n")
      .replaceAll("<pre lang=\"sbt\">", "```sbt\n")
      .replaceAll("<pre lang=\"url\">", "```url\n")
      .replaceAll("<pre lang=\"clojure\">", "```clojure\n")
      .replaceAll("<pre lang=\"xml\">", "```xml\n")
      .replaceAll("<pre lang=\"mxml\">", "```mxml\n")
      .replaceAll("<pre lang=\"clojure\">", "```clojure\n")
      .replaceAll("<pre lang=\"html4strict\">", "```html4strict\n")
      .replaceAll("<pre lang=\"go\">", "```go\n")
      .replaceAll("<pre lang=\"css\">", "```css\n")
      .replaceAll("<pre lang=\"coffeescript\">", "```coffeescript\n")
      .replaceAll("<pre lang=\"python\">", "```python\n")
      .replaceAll("<pre lang=\"actionscript\">", "```actionscript\n")
      .replaceAll("<pre lang=\"ActionScript\">", "```actionscript\n")
      .replaceAll("<pre lang=\"actionscript3\">", "```actionscript3\n")
      .replaceAll("<pre lang=\"config\">", "```config\n")
      .replaceAll("<pre lang=\"none\">", "```\n")
      .replaceAll("<pre lang=\"nothing\">", "```\n")
      .replaceAll("<pre lang=\"logger\">", "```\n")
      .replaceAll("<pre lang=\"logs\">", "```\n")
      .replaceAll("<pre lang=\"something\">", "```\n")
      .replaceAll("<pre lang=\"whatever\">", "```\n")
      .replaceAll("<pre>", "```\n")
      .replaceAll("\n</pre>", "\n```")
      .replaceAll("</pre>", "\n```")
      .replaceAll("&lt;", "<")
      .replaceAll("&gt;", ">")
      .replaceAll("&amp;", "&")
      .replaceAll("https://d3o2fl8o7m2ury.cloudfront.net", "")

    val newLines = Seq("---\n", updatedFront, "---\n", updatedBack)

    write.over(file, newLines)
  }

  //fix(files.head)
  files.foreach(fix)

}
