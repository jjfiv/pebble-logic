package edu.umass.vde;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Deque;
import java.util.LinkedList;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JViewport;
import javax.swing.Timer;

/**
 *
 * @author jfoley
 */
public class CanvasBuffer {
  public final static int SIZE = 1024;

  private final JPanel panel;
  private final JScrollPane scroller;
  private final JPanel parent;
  
  private final Deque<JComponent> components;

  public CanvasBuffer(JPanel parent) {
    this.parent = parent;
    this.panel = new JPanel();
    this.scroller = new JScrollPane(panel);
    components = new LinkedList<JComponent>();
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
    components.offer(component);
    if (update) {
      update();
      scrollDown();
    }
  }
  /**
   * Hack because invokeLater doesn't invoke after large things are formatted,
   * so we don't scroll down far enough.
   */
  private final Timer scrollDownTimer = new Timer(10, new ActionListener() {
    @Override
    public void actionPerformed(ActionEvent ae) {
      JScrollBar vsb = scroller.getVerticalScrollBar();
      vsb.setValue(vsb.getMaximum());
      scrollDownTimer.stop();
    }
  });

  public void scrollDown() {
    this.scrollDownTimer.setInitialDelay(20);
    scrollDownTimer.start();
  }

  public void update() {
    while(components.size() > SIZE) {
      panel.remove(components.pop());
    }
    panel.revalidate();
  }
}
