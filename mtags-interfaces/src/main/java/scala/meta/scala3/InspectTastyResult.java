package scala.meta.scala3;

import java.util.List;

public interface InspectTastyResult {
  public List<String> toplevels();
  public String sourceFilePath();
}
