package edu.umass.vde;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Deque;
import java.util.LinkedList;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
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
  public final Deque<String> history;
  public int historyPosition = 0;

  public CanvasBuffer(JPanel parent) {
    this.parent = parent;
    this.panel = new JPanel();
    this.scroller = new JScrollPane(panel);
    this.history = new LinkedList<String>();
    components = new LinkedList<JComponent>();
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    Utils.setMinSize(scroller.getViewport(), 600, 400);
  }
  
  public void accept(String text) {
    history.offer(text);
    
    while(history.size() > SIZE)
      history.pop();
    
    historyPosition = 0;
  }
  
  public String previous() {
    if(history.size() == 0) return null;
    historyPosition = (historyPosition+1) % history.size();
    return (String) history.toArray()[historyPosition];
  }
  
  public String next() {
    if(history.size() == 0 || historyPosition == 0) return null;
    historyPosition--;
    return (String) history.toArray()[historyPosition];
  }

  public void add() {
    parent.add(scroller);
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
