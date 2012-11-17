Vim�UnDo� ��{ȑ@7�;���4�vV�&=�xN�v   �                                   O佭    _�                      j        ����                                                                                                                                                                                                                                                                                                                                                             O佬    �               �   package lu.uni.adtool.ui;       import java.awt.BorderLayout;   import java.awt.Frame;   #import java.awt.GridBagConstraints;   import java.awt.GridBagLayout;   import java.awt.Insets;       "import java.awt.event.ActionEvent;       import java.text.DecimalFormat;           import javax.swing.JButton;   'import javax.swing.JFormattedTextField;   import javax.swing.JPanel;       *import lu.uni.adtool.domains.rings.RealG0;       /**   O * Dialog to edit values for the attribute domains of real numbers greater than    * 0..     * @author Piot Kordy    */   -public class RealG0Dialog extends InputDialog   {   ;  static final long serialVersionUID = 458586363646948463L;     /**      * Constructs a new instance.      */   (  public RealG0Dialog(final Frame frame)     {   2    super(frame,"Enter non-negative real number");     }     /**   *   * Handle clicks on the various buttons.      * {@inheritDoc}   D   * @see java.awt.event.ActionListener#actionPerformed(ActionEvent)      */   2  public void actionPerformed(final ActionEvent e)     {   .    if ("-100".equals(e.getActionCommand())) {         add(-100);       }   2    else if ("-10".equals(e.getActionCommand())) {         add(-10);       }   1    else if ("-1".equals(e.getActionCommand())) {         add(-1);       }   1    else if ("+1".equals(e.getActionCommand())) {         add(1);       }   2    else if ("+10".equals(e.getActionCommand())) {         add(10);       }   3    else if ("+100".equals(e.getActionCommand())) {         add(100);       }   4    else if ("/1000".equals(e.getActionCommand())) {         times(0.001);       }   3    else if ("/100".equals(e.getActionCommand())) {         times(0.01);       }   2    else if ("/10".equals(e.getActionCommand())) {         times(0.1);       }   2    else if ("x10".equals(e.getActionCommand())) {         times(10);       }   3    else if ("x100".equals(e.getActionCommand())) {         times(100);       }   4    else if ("x1000".equals(e.getActionCommand())) {         times(1000);       }   7    else if ("Infinity".equals(e.getActionCommand())) {   @      valueField.setValue(new Double(Double.POSITIVE_INFINITY));         sync();       }   3    else if ("Zero".equals(e.getActionCommand())) {   )      valueField.setValue(new Double(0));         sync();       }   	    else{         super.actionPerformed(e);       }         }         /**      * {@inheritDoc}      * @see InputDialog#sync()      */     protected final void sync()     {   C    final double d = ((Number)valueField.getValue()).doubleValue();       if(d >= 0){         value = new RealG0(d);       }   	    else{   B      valueField.setValue(new Double(((RealG0)value).getValue()));       }     }     protected void createLayout()     {   0    final DecimalFormat f = new DecimalFormat();   #    f.setMaximumFractionDigits(50);   ,    valueField = new JFormattedTextField(f);   @    valueField.setValue(new Double(((RealG0)value).getValue()));       valueField.setColumns(15);   7    valueField.addPropertyChangeListener("value",this);   *    final JPanel inputPane = new JPanel();   -    inputPane.setLayout(new GridBagLayout());   :    final GridBagConstraints c = new GridBagConstraints();       JButton button;   #    c.insets = new Insets(0,8,0,0);       c.gridy=0;       c.gridx=0;   !    button = new JButton("-100");   $    button.setActionCommand("-100");   #    button.addActionListener(this);       inputPane.add(button,c);       c.gridx=1;        button = new JButton("-10");   #    button.setActionCommand("-10");   #    button.addActionListener(this);       inputPane.add(button,c);       c.gridx=2;       button = new JButton("-1");   "    button.setActionCommand("-1");   #    button.addActionListener(this);       inputPane.add(button,c);       c.gridx=3;       c.gridwidth = 2;        inputPane.add(valueField,c);       c.gridwidth = 1;       c.gridx=5;       button = new JButton("+1");   "    button.setActionCommand("+1");   #    button.addActionListener(this);       inputPane.add(button,c);       c.gridx=6;        button = new JButton("+10");   #    button.setActionCommand("+10");   #    button.addActionListener(this);       inputPane.add(button,c);       c.gridx=7;   !    button = new JButton("+100");   $    button.setActionCommand("+100");   #    button.addActionListener(this);       inputPane.add(button,c);       c.gridy=1;       c.gridx=0;   "    button = new JButton("/1000");   %    button.setActionCommand("/1000");   #    button.addActionListener(this);       inputPane.add(button,c);       c.gridx=1;   !    button = new JButton("/100");   $    button.setActionCommand("/100");   #    button.addActionListener(this);       inputPane.add(button,c);       c.gridx=2;        button = new JButton("/10");   #    button.setActionCommand("/10");   #    button.addActionListener(this);       inputPane.add(button,c);       c.gridx=3;   !    button = new JButton("Zero");   $    button.setActionCommand("Zero");   #    button.addActionListener(this);       inputPane.add(button,c);       c.gridx=4;   %    button = new JButton("Infinity");   (    button.setActionCommand("Infinity");   #    button.addActionListener(this);       inputPane.add(button,c);       c.gridx=5;        button = new JButton("x10");   #    button.setActionCommand("x10");   #    button.addActionListener(this);       inputPane.add(button,c);       c.gridx=6;   !    button = new JButton("x100");   $    button.setActionCommand("x100");   #    button.addActionListener(this);       inputPane.add(button,c);       c.gridx=7;   "    button = new JButton("x1000");   %    button.setActionCommand("x1000");   #    button.addActionListener(this);       inputPane.add(button,c);   4    contentPane.add(inputPane, BorderLayout.CENTER);       pack();     }     private void add(final int i)     {   =    double d = ((Number)valueField.getValue()).doubleValue();       d = d + i;   '    valueField.setValue(new Double(d));       sync();     }   $  private void times(final double i)     {   =    double d = ((Number)valueField.getValue()).doubleValue();       d = d * i;   '    valueField.setValue(new Double(d));       sync();     }   }    5��