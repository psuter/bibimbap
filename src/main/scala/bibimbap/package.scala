import akka.actor._
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

package object bibimbap {
  val executorService = Executors.newFixedThreadPool(10)
  implicit val ec = ExecutionContext.fromExecutor(executorService)
}
