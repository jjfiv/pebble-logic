package pebble;

import java.awt.Dimension;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JViewport;
import javax.swing.SwingUtilities;

/**
 *
 * @author jfoley
 */
public class CanvasBuffer {
  private final JPanel panel;
  private final JScrollPane scroller;
  private final JPanel parent;

  public CanvasBuffer(JPanel parent) {
    this.parent = parent;
    this.panel = new JPanel();
    this.scroller = new JScrollPane(panel);
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    setMinSize(300, 300);
  }

  public void add() {
    parent.add(scroller);
  }

  private void setMinSize(int x, int y) {
    Dimension size = new Dimension(x, y);
    JViewport viewport = scroller.getViewport();
    viewport.setMinimumSize(size);
    viewport.setPreferredSize(size);
  }

  public void append(JComponent component) {
    append(component, true);
  }

  public void append(JComponent component, boolean update) {
    panel.add(component);
    if (update) {
      update();
      scrollDown();
    }
  }

  public void scrollDown() {
    SwingUtilities.invokeLater(new Runnable() {
      @Override
      public void run() {
        JScrollBar vsb = scroller.getVerticalScrollBar();
        vsb.setValue(vsb.getMaximum());
      }
    });
  }

  public void update() {
    panel.revalidate();
  }
  
}
