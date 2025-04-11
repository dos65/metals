package scala.meta.pc;


import java.nio.file.Path;
import java.util.Optional;
import scala.meta.Tree;

public interface TreesInterface {
  
  Optional<Tree> getTree(Path path);
}
