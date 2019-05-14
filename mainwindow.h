#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = nullptr);
    ~MainWindow();
public slots:
    void delpol(int, int);
private:
    Ui::MainWindow *ui;
private slots:
    void new_polynom();
    void psum();
    void pmin();
    void multiple();
    void division();
    void derivative();
    void in_point();
    void solve_eq();
    void save_calc();
    void calc();
};
#endif // MAINWINDOW_H
